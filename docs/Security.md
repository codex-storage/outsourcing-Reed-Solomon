Security of FRI
---------------

Soundness properties of FRI are very complicated. Here I try to collect together some information.

### Some basic notions

We will node some basics about coding theory:

- the _rate_ of a code $\mathcal{C}$ is $\rho=K/N$, where we encode $K$ data symbols into a size $N$ codeword (so the number of parity symbols is $N-1)
- the _distance_ between two codewords (of size $N$) is understood as relative Hamming distance: $\Delta(u,v) = \frac{1}{N}|\{i\;:\;u_i\neq v_i\}|$
- the minimum distance between two codewords is denoted by $0\le\mu\le 1$. In case of Reed-Solomon (or more generally, an MDS code), $\mu=(N-K+1)/N\approx 1-\rho$
- the _unique decoding radius_ is $\delta_{\textrm{unique}}=\mu/2$. Obviously if $\Delta(u,\mathcal{C})<\delta_{\textrm{unique}}$ then there is exactly 1 codeword "close to" $u$; more generally if $u$ is any string, there is _at most_ 1 codeword within radius $\delta_{\textrm{unique}}$
- for Reed-Solomon, $\delta_{\textrm{unique}}\approx (1-\rho)/2$

After this point, it becomes a bit more involved:

- the Johnson radius is $\delta_{\textrm{Johnson}} = 1 - \sqrt{1 − \mu} \approx 1-\sqrt{\rho}$
- within the Johnson radius, that is $\delta<\delta_{\textrm{Johnson}}$ we have the Johnson bound for the number of codewords "closer than $\delta$": $|\textrm{List}|<1/\epsilon(\delta)$, where $\epsilon(\delta) = 2\sqrt{1-\mu}(1-\sqrt{1-\mu}-\delta) \approx 2\sqrt{\rho}(1-\sqrt{\rho}-\delta)$
- the capacity radius is $\delta_{\textrm{capacity}} = 1-\rho$. Between the Johnson radius and the capacity radious, it's somewhat unknown territory, but there are conjectures that for "nice" codes (like RS), the number of close codewords is $\mathsf{poly}(N)$.
- above the capacity radius, the number of codewords is at least $|\mathbb{F}|$

In summary (for Reed-Solomon): Below in the unique decoding radius $\delta < (1-\rho)/2$ everything is fine. 

Above it we are in the "list decoding regime". Below the Johnson radius $\delta < 1-\sqrt{\rho}$ we can bound the number of close codewords explicitely:

$$ \Big|\big\{ v\in\mathcal{C} \;:\; \Delta(u,v)<\delta \;\big\}\Big| \le \frac{1}{2\sqrt{\rho}(1-\sqrt{\rho}-\delta)} $$

Finally, below the capacity radius we have some conjectures about the asymptotic growth of the number of close codewords.

### Reed-Solomon IOP of proximity

TODO; see Lecture 15

### Soundness of basic FRI

TODO

### Batch FRI

TODO; see Haböck's paper.

### FRI as polynomial commiment and out-of-domain sampling

TODO

### References

- Eli Ben-Sasson, Dan Carmon, Yuval Ishai, Swastik Kopparty, and Shubhangi Saraf: _"Proximity gaps for Reed-Solomon codes"_
- Ulrich Haböck: _"A summary on the FRI low degree test"_
