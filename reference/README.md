Reference implementation in Haskell
-----------------------------------

First we implement a slow but hopefully easier to understand version in Haskell,
to get more familiarity with all the details.

The implementation is loosely based on (the FRI portion of) Plonky2, which in 
turn is more-or-less the same as the DEEP-FRI paper. We use different conventions 
though, as Plonky2 is rather over-complicated.

See the [docs](../docs/) directory for details.

### Improving performance

We could significantly improve the speed of the Haskell implementation by binding C code 
(from the [`zikkurat-algebra`](https://github.com/faulhornlabs/zikkurat-algebra/) library)
for some of the critical routines: Goldilocks field and extension, hashing,
fast Fourier transform.

### References

- E. Ben-Sasson, L. Goldberg, S. Kopparty, and S. Saraf: _"DEEP-FRI: Sam-
pling outside the box improves soundness"_ - https://ia.cr/2019/336.
- Ulrich Haböck: _"A summary on the FRI low degree test"_
