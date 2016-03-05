# Pnums

[![Build Status](https://travis-ci.org/jwmerrill/Pnums.jl.svg?branch=master)](https://travis-ci.org/jwmerrill/Pnums.jl)

## Status

This is an experimental prototype. I'm not ready to make any commitments to the API or the storage format yet. Feel free to look around, but please don't publicize this yet.

## Description

Pnum stands for either "prototype Unum," or "projective number." I haven't totally decided yet. I don't want to call these Unums yet, because I've only implemented a tiny bit of the Unums 2.0 proposal, and I'm implementing some things that aren't in it, AFAICT (intervals with Pnum endpoints, akin to Unums 1.0 Ubounds, which I'm calling Pbounds).

Pnums are exactly as described by Gustafson for [Unums 2.0](http://www.johngustafson.net/presentations/Multicore2016-JLG.pdf). They are encoded as indexes into a lookup table of exact values, and alternate between representing one of these exact values or representing the open interval between successive exact values.

Pbounds represent intervals of the projective circle. They are encoded as 2 Pnum endpoints, which represent the entire anti-clockwise interval between the first endpoint and the second endpoint inclusive. They also store a separate tag to represent whether or not the set is empty.

## Roadmap

In the prototype implementation so far, the exact Pnum values are -1, 0, 1, and /0, where /0 is Gustafson's notation for the single point at projective infinity.

After I have finished implementing arithmetic and a few functions on these Pnums and their Pbounds, I will attempt to implement a larger, "practical" Pnum; probably 15-bit Pnums that can act as endpoints for 32-bit Pbounds.

I am not planning to implement Gustafson's dense Sets of Real Numbers (SORN's) until I have throughly explored Pnums and Pbounds. I may implement sparse sets before I implement dense sets, because I suspect they will be more practical. Dense SORN's are a nice, clarifying theoretical construct, but I am skeptical that they will be practically useful because they take so much space to represent.

I have some ideas about how to implement a software scratchpad to enable fused operations like the fused dot product, but I don't plan to work on that until I've explored the unfused landscape.
