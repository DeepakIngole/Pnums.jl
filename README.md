# Pnums

[![Build Status](https://travis-ci.org/jwmerrill/Pnums.jl.svg?branch=master)](https://travis-ci.org/jwmerrill/Pnums.jl)

## Status

This is an experimental prototype. I'm not ready to make any commitments to the API or the storage format yet. Feel free to look around, but please don't publicize this yet.

## Description

Pnum stands for either "prototype Unum," or "projective number." I haven't totally decided yet. I don't want to call these Unums yet, because I've only implemented a tiny bit of the Unums 2.0 proposal, and I would like to have the freedom to experiment with ideas that may not be part of the proposal.

Pnums are exactly as described by Gustafson for [Unums 2.0](http://www.johngustafson.net/presentations/Multicore2016-JLG.pdf). They are encoded as indexes into a lookup table of exact values, and alternate between representing one of these exact values or representing the open interval between successive exact values.

Pbounds represent intervals of the projective circle. They are encoded as 2 Pnum endpoints, which represent the entire anti-clockwise interval between the first endpoint and the second endpoint inclusive. They also store a separate tag to represent whether or not the set is empty.

## Roadmap

The initial prototype implements 3-bit Pnums: the exact values -1, 0, 1, and /0, and the open intervals between them (/0 is Guastafson's notation for the single point at projective infinity). I have implemented arithmetic operations on these Pnums, which produce 8-bit Pbounds made up of two Pnum endpoints and a two-bit tag that can signal the empty set. Arithmetic is closed over the Pbounds.

I have also implemented simple root bisection, which is capable of things like this:

```
julia> bisectroot(x->x*(x*x - 1), pb3"everything")

3-element Array{Pnums.Pbound{Pnums.Pnum3},1}:
 pb3"-1"
  pb3"0"
  pb3"1"
```

Notice that this is *global* root finding.

There is no exact representation of 3 in the 3-bit Pnums, but bisection can still accurately find  regions that enclose the exact solution of `x^2 = 3`.

```
julia> bisectroot(x->x*x-3, pb3"everything")

2-element Array{Pnums.Pbound{Pnums.Pnum3},1}:
 pb3"(/0, -1)"
  pb3"(1, /0)"
```

I have also implemented a rudimentary version of dense Sets of Real Numbers over these Pnums, which is currently used only for testing the correctness of the Pbound routines. This is an important function, though. Multiplication is a non-convex, saddle-shaped function, and there are many edge cases to get correct when splitting a pair of Pbounds into quadrants and reassembling the pieces of an answer.

Now that 3-bit Pnums are basically working, I plan to implement 7- and 15-bit Pnums and their associated 16- and 32-bit Pbounds. Implementing these will force the design to become generic enough that it should be reasonably easy to experiment with various related systems.
