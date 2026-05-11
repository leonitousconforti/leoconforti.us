---
title: MITM Unity mobile games including ssl traffic

description: Man-in-the-middle network traffic capture for unity mobile games including ssl traffic

tags: unity, reverse-engineering

toc: true
---

# What is a man-in-the-middle attack

[Wikipedia](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) has a great
article on MITM attacks, I would recommend reading that over what I try to
summarize lol.

To quote the Wikipedia,

"In cryptography and computer security, a mitm attack is a cyber attack where
the attacker secretly relays and possibly alters the communications between two
parties who believe that they are directly communicating with each other, where
in actuality the attacker has inserted themselves between the two user parties."

# Motivation

I play a single player mobile game on my phone called TinyTower bu Nimblebit.
Even though this is a primarily single player game, there is still some social
features in the game such as visiting friends and sending gifts to other
players. The presence of these actions requires there to be some network
communication to send the items to the other players, these seem like they could
be cool features to build a trading platform for the game on top of!

# Problem Statement

In order to build a trading platform for in-game items in TinyTower, we need a
way to send items to other players. This feature exists natively in the game, we
just need to figure out what happens and how it works. Today, we will use a MITM
setup to capture the network traffic while playing the game and sending an item
to another play to try to get some preliminary insights.

# MITM Setup

I am going to use Charles Proxy in this article because I think it has a nice
user interface and feels intuitive.

## Charles Installation

To install Charles Proxy, visit <https://www.charlesproxy.com/>, navigate to
the download page, and follow the specific instructions for your operating
system. I use MacOS, so I will use the MacOS dmg download. Once Charles Proxy is
installed on your system, go ahead and launch it. You should be looking at
something that looks like this

![CharlesProxy](/images/mitm-charles-proxy/Screenshot 2026-05-11 at 9.40.30 AM.png)
