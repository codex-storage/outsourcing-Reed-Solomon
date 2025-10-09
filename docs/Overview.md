Outsourcing local erasure coding
--------------------------------

The purpose of local erasure coding (we used to call this "slot-level EC" in old Codex) is to increase the strength of storage proofs based on random sampling.

The core principle behind this idea is the distance amplification property of Reed-Solomon codes.

The concept is simple: If we encode $K$ data symbols into $N$ code symbols, then for the data to be irrecoverably lost, you need to lose at least $N-K+1$ symbols (it's a bit more complicated with data corruption, but let's ignore that for now). In a typical case of $N=2K$, this means that checking just one randomly chosen symbol gives you approximately $p\approx 1/2$ probability of detecting data loss.

### Outsourcing to the provider

In "old Codex", this encoding (together with the network-level erasure coding) was done by the client before uploading. 

However, it would be preferable to outsource the local encoding to the providers, for several reasons:

- the providers typically have more computational resources than the clients (especially if the client is for example a mobile phone)
- because the network chunks are hold by different providers, the work could be distributed among several providers, further decreasing the per-person work
- if it's the provider who does it, it can be postponed until enough  data (possible many small pieces from many different clients) is accumulated to make the resulting data unit size economical

However, in general we don't want to trust the provider(s), but instead verify that they did it correctly.

We need to verify 3 things:

- the original data (authenticated by a Merkle root or similar commitment) is kept intact;
- the encoding is done correctly;
- the new Merkle root corresponds to the encoded data.
 
Fortunately, there is a way to do all this.

### FRI low-degree check

The FRI protocol (short for "Fast Reed-Solomon Interactive Oracle Proofs of Proximity") is an interactive protocol in which the prover convinces the verifier that a Merkle-committed vector of finite field elements is _close to_ a Reed-Solomon codeword (with rate $\rho=2^{-r}$).

Note that obviously we cannot do better than "close to" without checking every single element of the vector (which again obviously wouldn't be a useful approach), so "close to" must be an acceptable compromise.

However, in the ideal situation, if the precise distance bound $\delta$ of the "close to" concept is small enough, then there is exactly 1 codeword within that radius ("unique decoding regime"). In that situation errors in the codeword can be corrected simply by replacing it with the closest codeword. A somewhat more complicated situation is the so-called "list decoding regime".

As usual we can make this non-interactive via the Fiat-Shamir heuristic.

This gives us a relatively simple plan of attack:

- the client (uploader) calculates the Merkle root of the original data (assumed to have size $K=2^k$)
- the provider calculates the parity data (same amount, so the encoding has rate $\rho=1/2$), and its Merkle root
- the Merkle tree of the codeword (the concatenation of the original data and the parity data) is built by attaching these two subtrees to a single root node. That is, the Merkle root of the codeword is the hash of the pair of roots, of the original data and of the parity data
- the provider executes the FRI protocol to prove that the codeword is in fact (close to) a Reed-Solomon codeword
- the provider also distributes the Merkle root of the parity data together with the FRI proof. This is the proof (a singleton Merkle path) connecting the original data Merkle root and the codeword Merkle root (storage proofs will be validated against the latter)
- the metadata is updated: the new content ID will be Merkle root of the codeword, against which storage proofs will be required in the future. Of course one will also need a mapping from the original content ID(s) to the new locations (root + pointer(s) inside the RS encoded data)

Remark: If the 2x storage overhead is too big, after executing the protocol, we may try to trim some of the parity (say 50% of it). You can probably still prove some new Merkle root with a little bit of care, but non-power-of-two sizes make everything more complicated.

Remark #2: There are also improved versions of the FRI protocol like STIR and WHIR. I believe those can be used in the same way. But as FRI is already rather complicated, for now let's concentrate on that.

### Batching

FRI is a relatively expensive protocol. I expect this proposal to work well for say up to 1 GB data sizes, and acceptably up to 10 GB of data. But directly executing FRI on such data sizes would be presumably very expensive.

Fortunately, FRI can be batched, in a very simple way: Suppose you want to prove that $M$ vectors $v_i\in \mathbb{F}^N$ are all codewords. To do that, just consider a random linear combination (recall that Reed-Solomon is a linear code)
 
$$ V := \sum_{i=1}^M \alpha^i v_i $$

with a randomly chosen $0\neq\alpha\in\mathbb{F}$ (choosen by the verifier or via Fiat-Shamir). Intuitively, it's very unlikely that any of $v_i$ is _not a codeword_ but $V$ is (this can be quantified precisely). So it's enough to run the FRI on the combined vector $V$.

Note: If the field is not big enough, you may need to either repeat this with several different $\alpha$-s, or consider a field extension. This is the case for example with the Goldilocks field, which has size $|\mathbb{F}|\approx 2^{64}$. Plonky2 for example choses $\alpha\in\widetilde{\mathbb{F}}$ from a degree two field extension $\widetilde{\mathbb{F}}$ (so approx. 128 bits), which is big enough for any practical purposes. FRI is then executed in that bigger field.

This approach has another nice consequences: Now instead of doing one big RS encoding, we have to do many smaller ones. This is good, because:

- that's always faster (because of the $O(N\log(N))$ scaling of FFT)
- it needs much less memory
- can be trivially parallelized

This "wide" approach is basically the same as the one used in Plonky2 and Plonky3.

### Field and hash choice

We need to choose a prime field (but see below) for the Reed-Solomon encoding, and one (or more) hash functions to construct the Merkle tree.

Just for executing the FRI protocol, the hash function could be any (cryptographic) hash, and we could even use different hash functions for constructing the row hashes and the Merkle tree. However, if in the future we want to do recursive proof aggregation, then since in that situation the Merkle path proofs need to be to be calculated inside ZK too, it's better to choose a ZK-friendly hash.

With these in mind, a reasonable choice seems to be the Goldilocks field ($p=2^{64}-2^{32}+1$) and the Monolith hash function (which is one of the fastest ZK-friendly hashes). This way the Reed-Solomon encoding and the hash function use a compatible structure.

Remark: While in principle both FFT and FRI should work with a binary field instead of a prime field (see eg. FRI-Binius), I'm not at all familiar with those variations, so let's leave that for future work. Also, if we want to do recursive proof aggregation, again using prime fields for such proof systems is more common (but again, in principle that should be possible too with a binary field).

### Data layout

So the basic idea is to organize the data into a $2^n\times M$ matrix of field elements.

Then extend each column via a rate $\rho=1/2$ Reed-Solomon code to a matrix of size $2^{n+1}\times M$, so that top half is the original data, and the bottom half is parity.

Then hash each row, and build a Merkle tree on the top of the row hashes (this is the structure we need for the batched FRI argument).

#### Row hashes

However, we have a lot of freedom on how to construct the row hashes. The simplest is of course just a linear (sequential) hash. This is efficient (linear sponge hash with $t=12$ should be faster than building a Merkle tree, as you can consume 8 field elements with a single permutation call, instead of an average 4 with a binary Merkle tree); however a disadvantage is that a Merkle path proof needs to include a whole row, which can be potentially a large amount of data if $M$ is big.

The other end of the spectrum is to use a Merkle tree over the rows too. Then the Merkle path is really only a Merkle path. However, to make this reasonably simple, we need $M$ to be a power-of-two.

We can also go anywhere inbetween: Split the row into say 8, 16 or 32 chunks; hash those individually; and build a tiny Merkle tree (of depth 3, 4 or 5, respectively) on the top of them. The Merkle root of this small tree will be the row hash. This looks like a nice compromise with the good properties of both extreme cases, while also keeping the Merkle trees complete binary trees (that is, power-of-two number of leaves).

A problem though with this approach is that a single Merkle path doesn't really "proves" the full file, except when you also include the full row data. Which can be much bigger than the Merkle path itself...

So maybe actually including full rows is the right approach, and we just have to accept larger proofs (eg. with 2048 byte rows and a 16GB dataset, we have a Merkle tree of depth 23, that is, a Merkle proof is 736 bytes + the 2048 bytes row data is 2784 bytes proof per sample).

We also have the freedom to reorder the rows (except keeping the original data in the top half and the parity in the bottom; this is needed to be able to connect the Merkle root of the original data with the Merkle root of the encoded data).

Such reorderings can help making the FRI protocol more efficient.

#### Choice of the width

We may need to constraint the width $M$ too. This is unfortunate, because the height is already constrained to be a power of two, so we may end up with too many constraints (requiring too much padding and thus making the system less efficient).

One reason to constraint the width, if we want our Merkle tree to be compatible with the network block structure. Recall that from the network point of view, the datasets are organized in blocks of uniform size (currently 64kb blocks), and then having a SHA256 Merkle tree on the top of them.

Though I'm not sure at the moment if this compatibility is required at all?

But if we want this, then we want a row, or a small (power of two) number of rows to contain exactly a single network block's data.

Recall that with Goldilocks field we can pack either 7 bytes into a single field element, or somewhat better, 31 bytes into 4 field elements.

So one possible arrangement could be for example to have rows of size $M=268$, then each row can fit $67\times 4$ field elements, that's $67\times 31 = 2077 > 2048$ bytes. Then 32 rows can hold 64kb.

Another (wider) version could be $265\times 4$ field element per row, fitting $265\times 31 = 8215 > 8192$ bytes, so that 8 rows hold 64kb. But see above about including a full row in the proof.


### On-disk layout

Another subtle issue is how to order this data on the disk. Unfortunately spinning disks are very slow (150-300 MB/sec sequential fastest, and really really slow seeking: typically well below 1000 seeks per second).

What are our typical access patterns?

- to do the FFT encoding, we need to access all the columns, independently (and then they will be processed in parallel)
- to do the query phase of the FRI protocol, we need to access some randomly selected rows (maybe 50--100 of them), and also the full dataset in a form of the "combined polynomial"
- when doing the randomly sampled "storage proofs" (which is supposed to happen periodically!), again we need to access random rows

Accessing both rows and columns efficiently is pretty much contradictory to each other...

Maybe a compromise could be something like a "cache-oblivious" layout on disk, for example, partitition the matrix into medium-sized squares, so that both rows and columns are somewhat painful, but neither of them too much painful.

On the other hand, ideally the encoding and the FRI proof is done only once, while storage proofs are done periodically, so maybe row access should have a priority? It's a bit hard to estimate the cost-benefit profile of this, it also depends on the average lifetime. We have a rather heavy one-time cost, and a rather light but periodically occuring cost...

Btw, the harddrive _sequential_ speed limit means, that unless there the data is on SSD or some other fast medium, the bottleneck will be always close to the spinning drive. For example, while Goldilocks FFT has similar speeds as sequential harddrive reads/writes, it can be trivially parallelized (as we do many independent FFTs, one for each column).

### Bundling of files

The idea was that when we have to deal with lots of small files, we can start collecting and merging them, until the size of this bundle reaches an economical size (we don't want too many proofs circulating in the network).

When a good size is reached (or too much time passed), we can then do the erasure coding, kind of "sealing" the bundle (though in principle we could collect even more and redo the encoding the same way).

#### Discrepancy of lifetimes

If we have a lot of small files (from potentially many users), they most probably have different expected lifetimes.

However, I expect that typical small files have similar lifetimes (probably something on the order of 1 month). And keeping a file for a little bit longer shouldn't be a big issue.

Furthermore it's the provider who chooses what to bundle together, so they can select for similar expiries (maybe building up several bundles at the same time).

So I don't see this as a serious issue.

#### Discrepancy of file sizes

Files can also have different sizes, and non-power-of-two sizes.

Merging two Merkle trees is the simplest when they both have the same number of leaves, which number is also a power of two.

We can in theory pad any file to have power-of-two number of Merkle leaves, though this results in waste.

But again, if we expect a distribution of (padded) file sizes, then we can try some heuristics to pack them nicely.

For example: `(((1+1)+2)+4+4+4)+16 = 32`

#### How does this interacts with repair?

Well obviously when such a bundle is lost, then the cost of repair is bigger than if a chunk of a small file is lost.

However, we still have the information that where different chunks of the files in the different parts of the bundle are (that is required just for downloading them anyway), so in principle the repair can be done, alas with a larger number of network connections (because it's unlikely that other chunks of the constituent files are located at the same providers).

Another question is whether we want to repair the whole bundle, or the constituent files separately? By which I mean that the chunks of files have to be moved to other providers, should that be a single provider for the whole bundle, or different ones for the different files in that bundle?

### The FRI protocol

The FRI protocol (invented by Eli Ben-Sasson et al.) consists of two phases:

- the commit phase
- and the query phase

Note that somewhat confusingly, the commit phase is NOT calculating the Merkle commitment of the Reed-Solomon encoded data above; that's kind of "external" to the protocol, as we want to prove that an already committed vector is (close to) a codeword.

We will first describe the simplest version, then describe optimizations which are commonly used.

#### The commit phase

First we calculate the combined vector (linear combination of the columns); this is completely straightfoward.

This vector is interpreted as the values of a polynomial on a multiplicative subgroup (with initial size $2^{n}/\rho = 2^{n+1}$ as we use rate $\rho=1/2$; however with degree only $2^n-1$).

Then the prover will repeatedly "fold" this polynomial, halving both the size and the degree, until it reaches a constant polynomial (which will be represeneted by a vector of size two, both values being the same).

Each folded version is committed to using a Merkle tree, and at the very end the final singleton coefficient is sent in clear. In the query phase, we will then check (probabilistically) that the folding was done correctly.

The folding step works as follow: Let $p_k(x)$ be the polynomial before the $k$-the folding, then if

$$ p_k(x) = p_k^{\textrm{even}}(x^2) + x \cdot p_k^{\textrm{odd}}(x^2) $$

the next, folded polynomial will be

$$ p_{k+1}(y) := p_{k}^{\textrm{even}}(y) + \beta_k\cdot p_{k}^{\textrm{odd}}(y) $$

with $\beta_k \in\widetilde{\mathbb{F}}$ chosen by the verifier (or in practice, via Fiat-Shamir); and we evaluate it on the half-sized domain (multiplicative subroup) $D_{k+1} := (D_{k})^2$, generated by $\omega^2$ if the previous one was generated by $\omega$. 

So both the size of the vector and the degree of polynomial (whose evaluations are the elements of the vectors) is halved in each such folding step. After $\log_2(\deg)$ steps we get a constant polynomial, represented (in this simplest version) by a vector consisting two equal numbers.

Remark: In practice the domains are usually shifted to be a coset instead of a subgroup (this is called the DEEP method?).

Note: By simple calculation, we have

$$ 
\begin{align*}
p_{\textrm{even}}(x^2) &= \;\;\frac{1}{2}\Big[ p(x) + p(-x) \Big] \\
p_{\textrm{odd}}(x^2) &= \frac{1}{2 x}\Big[ p(x) - p(-x) 
\Big] 
\end{align*}
$$

This should be instantly recongizable as the "butterfly" step in the inverse FFT algorithm.

#### The query phase

In the query phase, we do several query rounds (enough to match the required security level); each round verifies the correctness folding from a randomly chosen starting point (with some probability of false positive; but enough such checks should drive down the probability to be negligible).

First, the verifier chooses a random 

$$x_{0}=\eta\cdot\omega^{\mathsf{idx}}\quad\in\quad D_{0}=\{\,\eta\cdot\omega^i\;:\; 0 \le i < 2^{n+1}\,\}$$

The corresponding full row (the $\mathsf{idx}$-th row of the LDE matrix) is opened with a Merkle proof, and the verifier computes the linear combination with the powers of $\alpha\in\widetilde{\mathbb{F}}^\times$. This will become the initial "upstream" value, against which we do the consistency check.

Next the following folding consistency check is repeated, until we reach a constant (degree 0) polynomial.

We want to check that for $x_{1}:=(x_0)^2=\eta^2\cdot\omega^{2\times\mathsf{idx}}$ we have $p_{1}(x_{1})$ as the expected value. From the above equations, the verifier can compute this from $p_{0}(x_{0})$ an $p_{0}(-x_{0})$.

So to do this, the provers opens these two values (with Merkle proofs against the commit phase Merkle commitment). The first value $p_{0}(x_{0})$ is checked to match the "upstream" value.

Then, using the above formulas, the verifier can compute the expected $p_{1}(x_{1})$:

$$
\begin{align*}
p_1(x_1) &= p_1(x_0^2) = p_0^{\textrm{even}}(x_0^2) + \beta_0\cdot p_0^{\textrm{odd}}(x_0^2) = \\
 &=
 \frac{1}{2}\Big[ p_0(x_0) + p_0(-x_0) \Big] +
  \beta_0 \cdot \frac{1}{2 x_0}\Big[ p_0(x_0) - p_0(-x_0) 
   \Big] \\
 &= \left(\frac{1}{2}+\frac{\beta_0}{2x_0}\right)p_0(x_0) + 
 \left(\frac{1}{2}-\frac{\beta_0}{2x_0}\right)p_0(-x_0) 
\end{align*}
$$

which becomes the "downstream" value, that is, the "upstream" value of the next folding step.

The same kind step is iterated with $x_{i+1}:=(x_{i})^2$, until we get down to degree zero. Finally, the last "downstream value" is checked against the pre-committed final constant value.

Note: opening a full row only happens once, before the very first folding step. But that's required otherwise we would have no connection between the data matrix and the combined vector(s).

Remark: The verifier should sample the query locations for _all rounds_ before the first round - otherwise a malicious prover could try and cheat round-by-round.

#### Optimizations

_Merkle caps._ Instead of using Merkle roots to commit, we can use "Merkle caps". This simply means to cut the Merkle tree at a given level; so the commitment will be say 16 hashes instead a single root hash. This is a tradeoff between commitment size and Merkle proof sizes.

_Pruning Merkle paths_. Alternatively, we could merge Merkle paths: the top parts will have a lot of shared values. This however makes recursive proofs too complicated.

_Folding in larger steps_ (eg. $16\mapsto 1$ or $8\mapsto 1$ instead of $2\mapsto 1$). In this case the computation of a single step will be a (very small) FFT. See the companion document for more details.

_Opening full cosets_. We can reoder the rows, so that when we need to open values (a small coset for folding), we can do that with a single Merkle path proof.

_Early stopping at a low degree polynomial_. We can stop at say a degree 31 polynomial instead of a constant (degree 0) one. Sending the coefficients of this polynomial in clear is usually a win.

_Grinding_. Between the commit phase and the query phase we can add a proof-of-work challenge. This ensures that a malicious prover can only do a much slower rate of brute-force trying to cheat Fiat-Shamir. Note: the challenge (and response) must go into the Fiat-Shamir transcript.

#### Why does this work?

It's _complicated_, but some important observations are:

- when folding, both the size of the vector and the degree of the polynomial decreases by the same factor; in particular, the rate of folded code is the same as the rate of the original code
- if $f$ was $\delta$-far from the original code, then $\mathsf{Fold}(f,\beta)$ is also $\delta$-far from the folded code (with high probability, if $\beta$ was chosen randomly)

#### Security

The math behind all this is very complicated; however it's a somewhat intuitive conjecture that the security level (assuming big enough fields) should be approximately

$$\lambda \approx \textrm{rate_bits} \times \textrm{num_query_rounds} + \textrm{proof_of_work_bits}$$

(TODO: justify this somehow?)

A standard security target is 100 bits. In our case the $\textrm{rate_bits} = -\log_2(\rho)$ is 1; so with 16 bits of grinding, we need 84 query rounds. The rate is thus more-or-less a tradeoff between prover speed and proof size. 

While we need an at most 2:1 expansion from the original data (otherwise the storage overhead would be too big), in theory we could try using a larger expansion rate (say $\rho=1/4$ or $\rho=1/8$), but then after executing the protocol, keeping only the same number of parity data as the original data. This could result in a smaller proof size (less query rounds) but larger prover time.

See also the companion security document for more details.

### References

- Eli Ben-Sasson, Iddo Bentov, Yinon Horesh and Michael Riabzev: _"Fast Reed-Solomon Interactive Oracle Proofs of Proximity"_
- Eli Ben-Sasson, Lior Goldberg, Swastik Kopparty and Shubhangi Saraf: _"DEEP-FRI: Sampling Outside the Box Improves Soundness"_
- Ulrich Haböck: _"A summary on the FRI low degree test"_
- Alexander R. Block et al: _"Fiat-Shamir Security of FRI and Related SNARKs"_
