---
title: Migrating a large monorepo to effect-smol part 1

description: Migrating a (what I would consider to be) large monorepo to
  effect-smol part 1

tags: effect-ts, typescript, reverse-engineering

toc: true
---

# Introduction

When I first started learning effect-ts, I built several projects to try to get
the feel for using it. One of the places I was most excited to use it in was my
frida agents that I was using to explore TinyTower.

Frida is a dynamic reverse engineering toolkit, and that work "dynamic" was what
made me so excited to try to introduce effect-ts into the mix. I had already
built to much retry logic and error handling for frida, I was excited to also
get all the other benefits that effect brings. The path to marrying frida with
effect was much more tumultuous than I expected though. That might have
partially been because I had big goals lol.

# Goals

After having spent some time in the effect ecosystem building wireguard and
docker packages, I then transitioned to building my frida+effect integrations. I
thought of a few packages I wanted to build:

- a frida tools wrapper for providing frida devices and sessions as effect
  layers

- an effect "platform-frida" package for providing frida runtime specific
  implementations of services

- an effect "sql-frida" package for providing a frida implementation for the
  effect sql interface

- an effect "rpc-frida" package for providing a frida implementation for the
  effect rpc interface

The last two I had no idea where to start on, nor how I would even implement
them. Additionally, along the way, I built two more packages:

- a polyfills package for polyfill-ing everything needed to get effect working in
  frida's GumJS environment

- a vitest pool for running test suites inside of a frida agent

Today, all of these packages have been built and are available under the
`@efffrida` organization on npm. Now, I wanted to start migrating to
effect-smol.

# Initial Migration Attempt

I initially attempted to have AI do the migration for me. If you want to try to
do the same, I will make a git tag from right before the migration. In the end,
the agent consumed all of my weekly rate limit after having compacting the
context a few times. I would say that it got about halfway, but there was some
refactoring I needed to do on the code it migrated.

# Pain Points

There is a list of APIs that have been pain points for others while migrating,
it can be found here <https://github.com/Effect-TS/effect-smol/issues/1378>. In
my case, I found that some of the `Layer` api changes and no more
`Effect.asyncEffect` and `Effect.dieMessage` to be points of friction. The agent
didn't really know how to migrate these, and generated code that I was
unsatisfied with.
