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

### Implementation status

- [x] FRI prover
- [x] FRI verifier
- [x] proof serialization
- [ ] serious testing of the FRI verifier
- [ ] full outsourcing protocol
- [ ] command line interface
- [x] faster Goldilocks field operations via C FFI
- [ ] quadratic field extension in C too
- [x] faster hashing via C FFI
- [ ] faster NTT via C FFI

### References

- E. Ben-Sasson, L. Goldberg, S. Kopparty, and S. Saraf: _"DEEP-FRI: Sampling outside the box improves soundness"_ - https://eprint.iacr.org/2019/336
- Ulrich Haböck: _"A summary on the FRI low degree test"_ - https://eprint.iacr.org/2022/1216
