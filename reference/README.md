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
for some of the critical routines: Goldilocks field and extension, hashing, fast Fourier 
transform.

The switch between the simple but intentionally naive (and very slow) Haskell 
implementation and the significantly faster C bindings is controlled by by the 
C preprocessor flag `-DUSE_NAIVE_HASKELL` (so the faster one is the default).

### Implementation status

- [ ] cabalize
- [x] FRI prover
- [x] FRI verifier
- [x] proof serialization
- [ ] serious testing of the FRI verifier
- [ ] full outsourcing protocol
- [x] faster Goldilocks field operations via C FFI
- [x] quadratic field extension in C too (useful for the folding prover?)
- [x] faster hashing via C FFI
- [ ] faster NTT via C FFI
- [ ] disk layout optimization
- [ ] end-to-end workflow with input/output data in files
- [ ] command line interface
- [ ] even more detailed documentation of the protocol

### References

- E. Ben-Sasson, L. Goldberg, S. Kopparty, and S. Saraf: _"DEEP-FRI: Sampling outside the box improves soundness"_ - https://eprint.iacr.org/2019/336
- Ulrich Haböck: _"A summary on the FRI low degree test"_ - https://eprint.iacr.org/2022/1216
