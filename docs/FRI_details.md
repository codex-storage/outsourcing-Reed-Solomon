FRI row indexing and FFT details
--------------------------------

The motivation here was to try and work out how to index the rows in a Plonky2-style FRI protocol. More generally, other details like the FFT folding verifier are also worked out.

### Evaluation domains

Plonky2 uses cosets for evaluation domains (instead of subgroups). This is presumably the "out of domain sampling" idea (DEEP-FRI?).

$$ \mathcal{D}_0 \;:=\; \mathbf{g}\cdot \langle \omega \rangle \;=\; \big\{\, \mathbf{g}\cdot\omega^i\;:\; 0\le i < N\cdot 2^r \,\big\}$$

where $\mathbf{g}\in\mathbb{F}^\times$ is a generator of $\mathbb{F}^\times$, and $\omega\in\mathbb{F}^\times$ is an $N\cdot 2^r=2^{(n+r)}$-th root of unity, where $n=\log_2(N)$ is the logarithm of the size $N$ of the original data, and $r=-\log_2(\rho)$ is the "rate bits".

Then if we fold by a factor of $K=2^\kappa$, the next domain will be

$$ \mathcal{D}_1 \;:=\; (\mathcal{D}_0)^K = \mathbf{g}^K \cdot \langle\omega^K \rangle \;=\; \big\{\, \mathbf{g}^K\cdot\omega^{Ki}\;:\; 0\le i < (N/K)\cdot 2^r \,\big\}$$

And so on. Of course we don't have to use the same $k$ at every folding step (this is called "reduction strategies" in Plonky2)

### Basic FRI verification workflow

Given a query index $\mathsf{idx}$, we

- first check the opened row against the Merkle root of the LDE (low-degree extended) matrix with a Merkle proof ("initial tree proof" in Plonky2 lingo)
- combine the opened row with $\alpha\in\widetilde{\mathbb{F}}$ to get the "upstream value"
- then repeatedly fold:
    - open the "folding coset" from the commit phase Merkle tree (checking the corresponding Merkle proof)
    - check the upstream value against the corresponding element in the coset
    - calculate a small FFT on this coset (see below for details)
    - combine the transformed values with the step's $\beta\in\widetilde{\mathbb{F}}$ (a new $\beta$ is sampled for each folding step, during the commit phase)
    - this will be the next step's "upstream value"
    - loop until the folded degree becomes small enough
- check the final polynomial (sent as coefficients) against the last upstream value by evaluating at the given location.

### Discrete Fourier Transform (DFT)

Given a subgroup $H=\langle \omega \rangle\subset \mathbb{F}^\times$ of size $|H|=K$, the DFT and its inverse converts between the coefficient form and evaluation form of a polynomial $f(x)\in\mathbb{F}^{<K} [x]$ of degree $K-1$:

\begin{align*}
f(\omega^i) &= \sum_{k=0}^{K-1} \omega^{ik}a_k \\
a_i &= \frac{1}{K} \sum_{k=0}^{K-1} \omega^{-ik}f(\omega^k)
\end{align*}

It's easy to shift this to a coset $\eta H=\{\eta\cdot \omega^i\;:\;0\le i < K\}$:

\begin{align*}
f(\eta\cdot \omega^i) &= \sum_{k=0}^{K-1}  \omega^{ik} \eta^k a_k \\
a_i &= \eta^{-i}  \frac{1}{K}\sum_{k=0}^{K-1} \omega^{-ik}f(\eta\cdot\omega^k)
\end{align*}

### Folding with FFT

Given a polynomial $\mathcal{P}(x)$ of size $N$ (degree $\deg(\mathcal{P})=N-1$), when folding with factor $K=2^\kappa$, we want to decompose it as 

$$ \mathcal{P}(x) = \sum_{i=0}^{K-1} x^i\cdot p_i(x^K) $$

and then combine these into the folded polynomial $\mathcal{P}'$:

$$ \mathcal{P}'(y) := \sum_{i=0}^{K-1} \beta^i\cdot p_i(y) $$

So this what the prover does, internally; meantime the verifier, in each query round, gets the values of these polynomials on a coset $\mathcal{C}\subset \mathcal{D}$ of size $|\mathcal{C}|=K=2^\kappa$ in the evaluation domain $\mathcal{D}=\{\mathbf{h}\cdot \omega^i\}$ of size $N$:

$$ \mathcal{C} \;=\; \big\{\; x_j = \mathbf{h}\cdot \omega^{\mathsf{idx} + j\times (N/K)} = \mathbf{h}\cdot \omega^{\mathsf{idx}}\mu^j \;:\; 0 \le j < K 
\;\big\}
$$

where $0 \le \mathsf{idx} < N$ is the "upstream" query index. The corresponding "upstream location" was $x_0=\mathbf{h}\cdot \omega^\mathsf{idx}$, and the folded location will be $y:=x_0^K = \mathbf{h}^K\cdot \omega^{K\times \mathsf{idx}}$ (with "downstream" query index $\mathsf{idx}' := \mathsf{idx}\;\textrm{mod} \;N/K$; note that we use natural ordering here, unlike Plonky2 which uses bit-reversed ordering).

**So why does the FFT works here?**

Let $\mu=\omega^{N/K}$ be the generator of the (subgroup corresponding to) the coset $\mathcal{C}$ of size $|\mathcal{C}|=K$.

Observe the following elementary fact:
$$ \frac{1}{K}\sum_{i=0}^{K-1} \mu^{ik} \;=\; 
  \left\{\begin{array}{ll}
  1 &\textrm{if }\, k\equiv 0\;\,(\textrm{mod}\; K) \\
  0 &\textrm{otherwise}
  \end{array}\right.
$$

From this, we have
$$
p_l(x^K) \; =\; \frac{1}{Kx^l} \sum_{j=0}^{K-1}\mu^{-jl} \cdot \mathcal{P}(\mu^j\cdot x)
$$

Too see this, write $\mathcal{P}(x)=\sum_i a_ix^i$; since everything is linear, it's enough to check that for the term $a_mx^m$, we have

$$
 \frac{1}{K} \sum_{j=0}^{K-1}\mu^{-jl} \cdot a_m(\mu^j x)^m = \frac{a_m x^m}{K}
 \sum_{j=0}^{K-1}\mu^{j(m-l)} = 
  \left\{\begin{array}{ll}
  a_m x^m &\textrm{if }\, m\equiv l\;\,(\textrm{mod}\; K) \\
  0 &\textrm{otherwise}
  \end{array}\right.
$$

Now substituting the coset elements $x_m=\mathbf{h}\cdot \omega^{\mathsf{idx}}\mu^m$ and the corresponding vlaues $y_m=\mathcal{P}(x_m)$ into our formula for $p_l(x^K$), we get:

$$
\begin{align*}p_l(x_m^K) \;&=\; p_l(\mathbf{h}^K\cdot \omega^{K\mathsf{idx}}\mu^{Km}) \\
&=\;
\frac{1}{K\cdot\mathbf{h}^l\cdot \omega^{l\mathsf{idx}}\mu^{lm}} \sum_{j=0}^{K-1}\mu^{-jl} \cdot \mathcal{P}(\mathbf{h}\cdot \omega^{\mathsf{idx}}\mu^{m+j}) \\
  &= \frac{1}{K\cdot (\mathbf{h}\omega^{\mathsf{idx}})^l}
   \sum_{j=0}^{K-1}\mu^{-l(m+j)} y_{m+j} 
 \;=\; \frac{1}{K\cdot (\mathbf{h}\omega^{\mathsf{idx}})^l}
   \sum_{j=0}^{K-1}\mu^{-lj} y_{j}
\end{align*} 
$$

in which we can recognize the inverse coset DFT formula from above.

TODO: maybe make the indices notation more consinstent...

**Winograd small FFT**

For small sized FFTs with specific sizes, up to maybe size $2^5=32$, there are specialized FFT algorithms with a bit fewer number of multiplications than the standard one.

This may be worthwhile to investigate and benchmark.

### Row ordering

First a remark about FFT. Some (in-place) FFT implementations naturally give the result in the "bit-reversed" permutation. This is the permutation where the vector indices, written in binary, have their bits reversed:

$$
\begin{align*}
  \big[2^n\big] \quad &\longrightarrow\quad \big[2^n\big] \\
       \sum_{i=0}^{n-1} 2^i b_i \quad&\longmapsto\quad \sum_{i=0}^{n-1} 2^i b_{n-1-i}
\end{align*}
$$

As our FFT implementation gives the result in the natural order (cyclic group order), and bit-reversal just makes everything harder to understand, we always use the natural order here.

It's straightforward, if confusing, to adapt everything for a bit-reversed FFT (for example Plonky2 indices mostly everything in the bit-reversed order).

#### Matrix row ordering

As we only open single, randomly-selected rows, the Merkle tree ordering doesn't matter for efficiency.

However, to be able prove the connection of the original data and the parity data, we need these to be subtrees.

Since with FFT, we normally put the original data on a subgroup, and the parity data on its $2^r-1$ cosets, the most natural indexing is the following:

\begin{align*}
\mathsf{data}     &: \{\,0,\,R\quad\;\;\;,\,2R\quad\;\;,\,\dots,\, (N-1)R\quad\;\;\,\, \}  \\
\mathsf{parity_1} &: \{\,1,\,R+1,\,2R+1,\,\dots,\,(N-1)R+1\, \}  \\
\mathsf{parity_2} &: \{\,2,\,R+2,\,2R+2,\,\dots,\, 
(N-1)R+2\, \} \\ 
\vdots & \\
\mathsf{parity}_{R-1} &: \{\,R-1,\,2R-1,\,3R-1,\,\dots,\, 
NR-1\, \} 
\end{align*}

where $R=2^r=1/\rho$ is the expansion ratio.

Note: using $r>1$ (so $R>2$ or $\rho<1/2$) in the FRI protocol may make sense even if we only keep a smaller amount of the parity data at the end (to limit the storage overhead), as it may improve soundness or decrease proof size (while also making the proof time longer).

#### Commit phase vectors ordering

Here we open cosets of size corresponding to the folding step arity $K$ (which can be different in the different folding steps).

So such cosets should be the leaves of the tree. If the size of the of the vector is $M$, then the cosets (in natural ordering) are indexed as:

$$ \mathcal{C}_i = \big\{\, i ,\, i+M/K ,\, i + 2(M/K) ,\, \dots ,\, i + (K-1)(M/K) \,\big\} $$

for $0\le i < M/K$. Thus the Merkle tree should have size $M/K$ and with the $i$-th leaf being $\mathcal{C}_i$.

Of course we could permute the order of the leaves as we want, but this seems to be most natural order.

Remark: Which coset we want to open is determined by the "upstream index" $0\le \mathsf{idx} < M$. To get the corresponding coset index we can simply compute
$i:= \mathsf{idx}\;\textrm{mod}\;M/K$. However, to match the above FFT computation, we also have to cyclically permute the coset, so that it starts at $\mathsf{idx}$. In practice this mean rotating the opened coset by $\lfloor \mathsf{idx}/(M/K)\rfloor$.

Alternatively, instead of "rotating" the coset, we can instead calculate which element of the coset should match the upstream value. This is maybe a little bit more intuitive.

### Unique decoding, list decoding, etc

TODO; also see the security document
