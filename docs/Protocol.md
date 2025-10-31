The RS outsourcing protocol
---------------------------

The Reed-Solomon outsourcing protocol is an interactive protocol to convince a client that an untrusted server applied Reed-Solomon encoding to the client's data correctly.

More precisely, the data is organized as a matrix; it's committed to by a Merkle root built on the top of row hashes; and Reed-Solomon encoding is applied to each column independently. 

The client uploads the data and the server replies with the Merkle root of the RS-encoded data, and a proof connecting the Merkle roots of the original data and the encoded root (and also proving correctness the of encoding).

As usual, the protocol can be made mostly non-interactive; but still there are two phases of communication: data upload by the client, and then a server reply with a proof.

Note: In the proposed use case, there isn't really a "client", and the original data is already committed; however it's easier to describe in a two-party setting (which could be also useful in other settings).

The protocol has three major parts:

- data preparation
- FRI protocol to prove the codeword
- connecting the original data to the encoded data

Furthermore we need to specify the conventions used, for example:

- encoding of raw data into field elements
- hash function used to hash the rows
- Merkle tree construction

### Global parameters

The client and the server needs to agree on the global parameters. These include:

- the size and shape of the data to be encoded
- the rate $\rho = 2^{-r}$ of the Reed-Solomon coding
- all the parameters of the FRI proof

We use the Goldilocks prime field $p=2^{64}-2^{32}+1$. In the FRI protocol we also use the quadratic field extension $\widetilde{\mathbb{F}} = \mathbb{F}_p[X]/(X^2-7)$; the particular extensions chosen mainly for compatibility reasons: $p(X) = X^2\pm 7$ are the two "simplest" irreducible polynomials over $\mathbb{F}_p$, and $X^2-7$ was chosen by Plonky2 and Plonky3.

We use the [Monolith](https://eprint.iacr.org/2023/1025) hash function. For linear hashing, we use the sponge construction, with state size `t = 12` and `rate = 8`, the `10*` padding strategy, and a custom IV (for domain separation).

For Merkle trees we use a custom construction with a keyed compression function based on the Monolith permutation.

### Data preparation

We need to convert the linear sequence of bytes on some harddrive into a matrix of field elements. There are many ways to do that, and some important trade-offs to decide about.

TODO;

See also the separate disk layout document.

### Connecting the original data to the encoded data

As mentioned above, the original data is encoded as a matrix $D\in\mathbb{F}^{N\times M}$ of field elements, and the columnwise RS-encoded data by an another such matrix $A\in\mathbb{F}^{N'\times M}$ of size $N'\times M$. Ideally both $N$ and $N'$ are powers of two, however, in practice we may require trade-offs where this is not satisfied.

Both matrices are committed by a Merkle root, with the binary Merkle tree built over the (linear) hashes of the matrix rows.

#### The ideal case

In the simplest, ideal case, we have $N=2^n$ and $N'=N/\rho=2^{n+r}$ where the code rate $\rho=2^{-r}$.

The encoded data can be partitioned as $(D;P_1,P_2,\dots,P_{R-1})$ where $D\in \mathbb{F}^{N\times M}$ is the original data, and $P_i\in \mathbb{F}^{N\times M}$ are the parity data (here $R=2^r=\rho^{-1}$). 

Each of these $R$ matrices can be committed with a Merkle root $\mathsf{h}_i\in\mathcal{H}$; the commitment of the encoded data $A$ (the vertical concatenation of these matrices) will be then the root of the Merkle tree built on these $R=2^r$ roots.

Here is an ASCII diagram for $\rho = 1/4$:

                        root(A)
                          /  \
                        /      \
                      /          \
                   /\              /\ 
                  /  \            /  \
     root(D) = h0      h1      h2      h3 = root(P3) 
               /\      /\      /\      /\   
              /  \    /  \    /  \    /  \ 
             /____\  /____\  /____\  /____\
               D       P1      P2      P3   

We want to connect the commitment of the original data $\mathsf{root}(D)=\mathsf{h}_0$ with the commitment of the encoded data $\mathsf{root}(A)$.

Note that $(\mathbf{h}_1,\mathbf{h}_2,\mathbf{h}_3)\in\mathcal{H}^3$ are a Merkle proof for this connection! To verify we only need to check that:

$$ \mathsf{root}(A) \;=\; \mathsf{hash}\Big(\;\mathsf{hash}\big(
 \mathsf{root}(D)\,\|\,\mathsf{h}_0\big) \;\big\|\;
 \mathsf{hash}\big(\mathsf{h}_2\,\|\,\mathsf{h}_3\big) \;\Big) 
$$

#### What if $N$ is not a power of two?

We may want to allow the original data matrix's number of columns not being a power of two.

Reasons to do this include:

- finer control over data size than just changing $M$
- having some other restrictions on the number of columns $M$ and wishing for less waste of space

As we want to use Fast Fourier Transform for the encoding, the simplest solution is pad to the next power of two $2^n$, for example with zeros.

We don't have to store these zeros on the disk, they can be always "added" run-time. By default, the size of the parity data will be still this $(\rho^{-1}-1)\times 2^n$ though.

#### What if $N'$ is not a power of two?

This is more interesting. In the above ideal setting, we allow $\rho=1/2^r$, however in practice that means $\rho=1/2$, as already $\rho=1/4$ would mean a 4x storage overhead!

But already in the only practical case we have a 2x overhead, we may want a smaller one, say 1.5x.

Unfortunately, the FRI protocol as described only works on power of two sizes, so in this case we would still do the encoding with $\rho=1/2$, but then discard half of the parity data (**WARNING!** Doing this may have serious security implications, which we ignore here).

In this case we will have to connect _three_ Merkle roots:

- the original data $D$;
- the RS codewords $E$ with $\rho=1/2$ (recall that FRI itself is also a proof against a Merkle commitment);
- and the truncated codewords (with effective $\rho=2/3$) $A$.

In glorious ASCII, this would look something like this:

               root(E) = h0123
                           /\
                         /    \
                       /        \
                     /            \
         root(D) = h01 -- r(A)     h23
                   /\       \      /\ 
                  /  \       \    /  \
               h0      h1      h2      h3 = root(P3) 
               /\      /\      /\      /\   
              /  \    /  \    /  \    /  \ 
             /____\  /____\  /____\  /____\
               D0      D1      P2      P3   
             \____________/  \____________/
                data D          parity
             \____________________/
                   truncated A

Here the public information is $\mathsf{root}(D)=\mathsf{h}_{01}$ and 

$$\mathsf{root}(A) \;:=\; \mathsf{hash}\big (\mathsf{root}(D)\,\|\,\mathsf{root}(P_2)\big) \;=\; \mathsf{hash}(\mathsf{h}_{01}\|\mathsf{h}_2)
$$

The connection proof will then consist of $\mathsf{h}_{2,3}=\mathsf{root}(P_{2,3})$ and $\mathsf{root}(E)=\mathsf{h}_{0123}$; and we can then check that:

$$
\begin{align*}
  \mathsf{root}(A) &= \mathsf{hash}\big(\;
      \mathsf{h_{01}}\;\|\; \mathsf{h_2}\;\big) \\
  \mathsf{root}(E) &= \mathsf{hash}\big(\;\mathsf{h}_{01} \;\|\; \mathsf{hash}(\mathsf{h}_{2}\|\mathsf{h}_2)\;\big) 
\end{align*}
$$

and that $E$ is really a matrix of codewords.

#### What if we want $\rho^{-1}>2$, that is, a larger code?

The security reasoning of the FRI protocol is very involved (see the corresponding security document), and we may want the codeword be significantly larger because of that; for example $\rho=1/8$.

From the "connecting original to encoded" point of view, this situation is similar to the previous one.

### Batched FRI protocol

The FRI protocol we use is essentially the same as the one in Plonky2, which is furthermore essentially the same as in the paper ["DEEP-FRI: Sampling outside the box improves soundness"](https://eprint.iacr.org/2019/336) by Eli Ben-Sasson et al.

Setup: We have a matrix of Goldilocks field elements of size $N\times M$ with $N=2^n$ being a power of two. We encode each column with Reed-Solomon encoding into size $N/\rho$ (also assumed to be a power of two), interpreting the data as values of a polynomial on a coset $\mathcal{C}\subset \mathbb{F}^\times$, and the codeword on larger coset $\mathcal{C}'\supset\mathcal{C}$.

The protocol proves that (the columns of) the matrix are "close to" Reed-Solomon codewords (in a precise sense).

**The prover's side:**

- the prover and the verifier agree on the public parameters
- the prover computes the Reed-Solomon encoding of the columns, and commits to the encoded (larger) matrix with a Merkle root (or Merkle cap)
- the verifier samples a random $\alpha\in\widetilde{\mathbb{F}}$ combining coefficient
- the provers computes the linear combination of the RS-encoded columns with coefficients $1,\alpha,\alpha^2,\dots,\alpha^{M-1}$
- "commit phase": the prover repeatedly
    - commits the current vector of values
    - the verifier chooses a random $\beta_k\in\widetilde{\mathbb{F}}$ folding coefficient
    - "folds" the polynomial with the pre-agreed folding arity $A_k = 2^{a_k}$
    - evaluates the folded polynomial on the evaluation domain $\mathcal{D}_{k+1} = \mathcal{D}_{k} ^ {A_k}$
- until the degree of the folded polynomial becomes small enough
- then the final polynomial is sent in clear (by its coefficients)
- an optional proof-of-work "grinding" is performed by the prover
- the verifier samples random row indices $0 \le \mathsf{idx}_j < N/\rho$ for $0\le j < n_{\mathrm{rounds}}$
- "query phase": repeatedly (by the pre-agreed number $n_{\mathrm{rounds}}$ of times)
    - the provers sends the full row corresponding the index $\mathsf{idx}_j$, together with a Merkle proof (against the Merkle root of the encoded matrix)
    - repeatedly (for each folding step):
        - extract the folding coset including the "upstream index" from the folded encoded vector
        - send it together with a Merkle proof against the corresponding commit phase Merkle root
- serialize the proof into a bytestring
 
**The verifier's side:**

- deserialize the proof from a bytestring
- check the "shape" of the proof data structure against the global parameters:
    - all the global parameters match the expected (if included in the proof) 
    - merkle cap sizes
    - number of folding steps and folding arities 
    - number of commit phase Merkle caps
    - degree of the final polynomial
    - all Merkle proof lengths
    - number of query rounds
    - number of steps in each query round
    - opened matrix row sizes
    - opened folding coset sizes
- compute all the FRI challenges from the transcript:
    - combining coeff $\alpha\in\widetilde{\mathbb{F}}$
    - folding coeffs $\beta_k\in\widetilde{\mathbb{F}}$
    - grinding PoW response
    - query indicies $0 \le \mathsf{idx}_j < N/\rho$
- check the grinding proof-of-work condition
- for each query round:
    - check the row opening Merkle proof
    - compute the combined "upstream value" $u_0 = \sum \alpha^j\cdot \mathsf{row}_j \in\widetilde{\mathbb{F}}$
    - for each folding step:
        - check the "upstream value" $u_k\in\widetilde{\mathbb{F}}$ against the corresponding element in the opened coset
        - check the folding coset values opening Merkle proof
        - compute the "downstream value" $u_{k+1}\in\widetilde{\mathbb{F}}$ from the coset values using the folding coefficient $\beta_k$ (for example by applying an IFFT on the values and linearly combining the result with powers of $\beta$)
    - check the final downstream value against the evaluation of the final polynomial at the right location
- accept if all checks passed.
    
This concludes the batched FRI protocol.

### Summary

We have to prove two things:

- that commitment to the "encoded data" really corresponds to something which looks like a set of Reed-Solomon codewords
- and that that is really an encoding of the original data, which practically means, because this was a  so-called "systematic code", that the original data is contained in the encoded data

The first point can be done using the FRI protocol, and the second part via a very simple Merkle proof-type argument.

There are a lot of complications in the details, starting from how to encode the data into a matrix of field elements (important because of performance considerations) to all the peculiar details of the (optimized) FRI protocol.

