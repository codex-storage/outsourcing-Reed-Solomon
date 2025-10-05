Trustless outsourcing of Reed-Solomon encoding of datasets
----------------------------------------------------------

This is intended to be an implementation of the following idea:

A client, owning a dataset (authenticated with a Merkle commitment) wants
to Reed-Solomon encode that data to an extended one (extend with parity data),
but instead of doing it themselves, they want an untrusted server to do it.

The server should respond with a Merkle commitment of the encoded data, 
and a proof that the encoding was done correctly and corresponds (contains) 
the original dataset.

The core idea is to use the FRI (or similar) low-degree test as a proof of
correct Reed-Solomon encoding.

See [docs/Overview.md](docs/Overview.md) for more details.
