---
title: Debugging intermittent Frida pool failures in CI

description: How concurrent worker initialization was crashing the shared
  frida-helper subprocess and silently killing every spawned process it owned

tags: frida, effect-ts, typescript, reverse-engineering

toc: true
---

# Background

As part of the efffrida project, I built a vitest pool that runs test suites
inside a Frida agent rather than a normal Node.js worker. The idea is that
instead of spinning up a thread per test file, you spawn a process, inject a
Frida script into it, and run your tests there. This lets you write tests that
use Frida APIs directly without any mocking.

The pool creates one `FridaPoolWorker` per test file. Each worker's constructor
kicks off an Effect layer build that:

1. Calls `device.spawn(["/usr/bin/sleep", "infinity"])` to create a blank
   process
2. Calls `device.attach(pid)` to inject the Frida agent into it
3. Calls `device.resume(pid)` so the process actually starts running

Tests then execute inside the injected agent via message passing.

# The Problem

On Linux CI, 1-2 tests were failing per run with:

```
Error: Unable to find process with pid 3565
```

The same test suite always passed locally (MacOSX). The failing test changed run
to run, which was the first sign that something concurrent was going wrong.

# Tracing the Error

The first thing I wanted to understand was where in the stack this error was
being thrown. After digging through the frida-core source, I found it originates
in `linjector.vala`:

```vala
private async string arch_name_from_pid (uint pid) throws Error {
    var cpu_type = yield cpu_type_from_pid (pid);
    ...
}
```

Which calls down to `gum_linux_cpu_type_from_pid()` in frida-gum, which reads
`/proc/{pid}/auxv` to determine the process architecture. If that file doesn't
exist, it throws `GUM_ERROR_NOT_FOUND` which surfaces as "Unable to find
process with pid".

The key detail: this read happens **in the main Node.js process**, before any
IPC call to the frida-helper subprocess. It's not a timeout, not a race on
process readiness. If this fails, the process genuinely does not exist.

So `device.spawn()` had returned a pid, and by the time `device.attach()` got
around to reading `/proc/{pid}/auxv`, that process was already dead.

# The Root Cause

## Everything shares one frida-helper

Frida's `getLocalDevice()` uses a module-level `sharedDeviceManager` singleton.
Every call returns the same `DeviceManager`, which means every pool worker ends
up sharing:

- one `LinuxHostSession`
- one `Linjector`
- one `LinuxHelperProcess`
- one `HelperFactory`
- one `frida-helper-64` subprocess

## Spawned processes are ptrace-owned by that subprocess

When you call `device.spawn()`, the frida-helper does a `fork()` where the child
calls `ptrace(TRACEME)` before `execve()`. This makes **frida-helper-64** the
ptrace tracer for the spawned process. The process stays in this state, stopped
and traced, until `device.resume()` is called which finally does
`ptrace(DETACH)` and lets it run.

There is a Linux kernel rule that matters here:

> When a ptrace tracer process exits, all of its traced children receive
> SIGKILL.

## What happens under concurrent load

Five test files → five `FridaPoolWorker` instances created simultaneously → five
concurrent `Layer.buildWithScope` calls → five concurrent `spawn` + `attach` +
`inject` + `resume` sequences all going through the same frida-helper-64
subprocess.

That's a lot of concurrent ptrace operations hitting one process: forking five
children, seizing them for injection, injecting agents, then detaching. At some
point under that load, the helper crashed or hit an unhandled error. When it
did, the Linux kernel delivered SIGKILL to every process it was currently
tracing.

Workers that had already received their pid from the completed `device.spawn()`
D-Bus call but had not yet called `device.attach()` then hit the dead process
when trying to read `/proc/{pid}/auxv`. Hence the error.

## Why a delay didn't help

I tried adding a delay between spawn and attach as a workaround. It didn't help,
and this explains why: the processes aren't slow to start. They're being killed
after they already exist. Waiting longer after spawn just means more of them are
dead by the time attach runs.

# The Fix

The fix is to prevent concurrent initialization in the first place. Since all
workers go through the same frida-helper subprocess, only one should be doing
spawn+inject at a time.

A static `Promise` chain on `FridaPoolWorker` does this cleanly:

```typescript
export class FridaPoolWorker implements VitestNode.PoolWorker {
    // Serializes startup across all workers: concurrent spawn+inject calls
    // crash the single shared frida-helper subprocess, killing every ptrace-
    // traced child it owns.  One-at-a-time init prevents that.
    private static initQueue: Promise<void> = Promise.resolve();

    constructor(...) {
        // ... layer setup ...

        this.scope = Scope.makeUnsafe();
        const prev = FridaPoolWorker.initQueue;
        const runInit = () =>
            ScriptLive.pipe(Layer.buildWithScope(this.scope), Effect.runPromise);
        this.scriptContext = prev.then(runInit, runInit);
        FridaPoolWorker.initQueue = this.scriptContext.then(
            () => undefined,
            () => undefined
        );
    }
}
```

Each new worker captures the current tail of the queue, starts its own
initialization only after the previous worker's init settles (the two-argument
`.then` means it runs regardless of whether the previous worker succeeded or
failed), and then advances the tail. Workers initialize strictly one at a time.

Test execution itself stays concurrent as only the startup phase is serialized,
so there is no change in throughput once all workers are up.
