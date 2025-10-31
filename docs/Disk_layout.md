Disk layout
-----------

We hope that this protocol will be feasible to be used with at least up to \~10GB data sizes (for example 8GB original data + 8GB parity is 16GB total size).

However, we don't want to keep such amount of data all in memory, for several reasons:

- we want to support relatively low-spec hardware
- the machine may want to do others things at the same time (this is supposed to be a background process)
- we may want to support parallelism

So it would be good if the memory consumption could be bounded (the max residency to be a configuration parameter).

Recall that semantically, all our data is organized as _matrices_ (2 dimensional); however, data as stored on disk is 1 dimensional. These two facts are in conflict with each other, especially as we have different access patterns:

- when the client computes the Merkle root of the original data, that's built on the top of _matrix rows_ 
- during the Reed-Solomon encoding itself, we access the _matrix columns_ independently (and the CPU computation can be done in parallel)
- when computing the Merkle tree of the encoded data, that's again _rows_
- during the FRI proof, we sample random _matrix rows_
- during future storage proofs, again one or a few _rows_ are sampled

We also expect people using HDDs (spinning disks, as opposed to SSDs), especially if the network data storage scales up the non-trivial size, as HDDs are much cheaper. Unfortunately, spinning disks are very slow, both in linear read/write (about 200-300 MB/sec) and seeking (up to 1000 seeks/sec); and disk access, unlike CPU tasks, cannot be parallelized.

This means disk layout is critical for performance.

#### Data Conversion

We can encode 31 bytes into 4 Goldilocks field elements. That's about 10% more efficient than encoding 7 bytes into a single field element, while still quite simple, so we should do that (a free 10% is a free 10%!).

#### Parallel row hashing

We are using a sponge construction (with state size of 12 field elements and rate 8) with the Monolith permutation for linear hashing.

The matrix Merkle trees (original and encoded) are built on the top of these row hashes.

As the permutation state of a single hash is encoded in $12\times 8 = 96$ bytes of memory, as long as the number of rows is not too big, we can compute these hashes in parallel even if we access the matrix columnwise: Simply process 8 columns (of size $N$) in a step, while keeping all the $N$ hash states ($N\times 96$ bytes) in memory.

### Example configuration

As an example, let's aim 8GB original data and 8GB parity, so in total 16GB of data.

With $N=2^{22}$ and $N'=2^{23}$, we need $M=265$ to encode this amount of data (one field element can on average encoded 7.75 bytes, as 4 field element can encode 31 bytes).

With this setup, one (already RS-encoded) column takes 64 megabytes of memory (62MB of raw data decoded into 64MB of field elements).

If processing columnwise, doing the RS-encoding and computing the row hashes, we need to keep in memory $12+8 = 20$ columns at the same time (12 for the hash states, and 8 for the hash input), that about 1.25 GB, which seems acceptable (of course more memory is needed for other purposes, but presumably this will dominate).

So it seems to be a reasonable idea to store the original data columnwise. 

#### Collecting small files

However, this seems to be in conflict with the main intended use case, namely _collecting small files_: Because those are _also_ identified by Merkle roots, and we want to prove _merging_ of such partial datasets (with extremely short Merkle proofs), those must also must consists of contiguous _rows_ (of the same size! which is here $M$). But those pieces can be still stored on-disk columnwise - this however means more seeking when reading in the merged dataset.

                              M
          /       +-----------------------------+   _
         |   2^18 |          file #0            |    \
         |        +-----------------------------+     +--+
         |   2^18 |          file #1            |   _/    \
         |        +-----------------------------+   _      +---+
         |   2^18 |          file #2            |    \    /     \
         |        +-----------------------------+     +--+       \
         |   2^18 |          file #3            |   _/            \
         |        +-----------------------------+                  \
         |        |                             |   _               \
         |   2^19 |          file #4            |    \               +-- root
     N   |        |                             |     \             /
     =   |        +-----------------------------+      +-+         /
    2^22 |        |                             |     /   \       /
         |   2^19 |          file #5            |   _/     \     /
         |        |                             |           \   /
         |        +-----------------------------+            +-+
         |        |                             |           /
         |        |                             |          /
         |        |                             |         /
         |   2^21 |          file #6            |      __/
         |        |                             |
         |        |                             |
         |        |                             |
          \       +-----------------------------+
          
For a single file, it makes sense to store the field elements columnwise. Assuming $N=2^n \ge 4$, we can read $7.75\times N = 31\times 2^{n-2}$ in a column.

Even if we normally read 8 columns at the same time, storing fully columnwise still makes sense because this allows a memory tradeoff with arbitrary constituent file sizes (we can read 1/2/4 columns as a contiguous block at a time instead of 8 or more):

    +-----+------+------+-- ... --+----------+
    |  0  | N    | 2N   |         | (M-1)N   |
    |  1  | N+1  | 2N+1 |         | (M-1)N+1 |
    |  2  | N+2  | 2N+2 |         | (M-1)N+2 |
    |     |      |      |         |          |
    | ... | ...  | ...  |   ...   |   ...    |
    |     |      |      |         |          |
    | N-2 | 2N-2 | 3N-2 |         |  MxN-2   |
    | N-1 | 2N-1 | 3N-1 |         |  MxN-1   |
    +-----+------+------+-- ... --+----------+
    
We can also collect the small files on SSD and store only the "sealed", erasure-coded merged blocks on HDD.

#### Building the Merkle trees

After computing the $N'\approx 2^{23}$ hashes, each consisting 4 field elements (32 bytes in memory) what we have is 256MB of data.

We build a binary Merkle tree on the top of that, that's another 256MB (in total 512MB with the leaf hashes).

This (the Merkle tree) we want to keep for the FRI prover. However, we don't want to keep all the actual data in memory, as that is too large. In the future phases (FRI sampling, and later storage proofs), we want to sample individual (random) _rows_. That would prefer in a different disk layout.

#### Partial on-line transposition

So we are processing the data in $N' \times 8$ "fat columns" (both computing the RS encoding and the row hashes at the same time), but we want to store the result of the RS-encoding in a way which is more suitable for efficient reading of random _rows_.

As we cannot really transpose the whole matrix without either consuming a very large number of memory (which we don't want) or a very large number of disk seeks (which is _really_ slow), we can only have some kind of trade-off. This is pretty standard in computer science, see for example "cache-oblivious data structures".

We can also use the SSD as a "cache" during the transposition process (as almost all machines these days include at least a small SSD).

Option #1: Cut the $N'\times 8$ "fat columns" into smaller $K\times 8$ blocks, transpose them in-memory one-by-one (so they are now row-major), and write them to disk. To read a row you need to read $8M$ bytes and need $M/8$ seeks (the latter will dominate). No need for temporary SSD space.

Option #2: Do the same, but reorder the $K\times 8$ blocks (using an SSD as a temporary storage), such that consecutive rows form a "fat row" of size $K\times M$. This needs $(N'/K)\times(M/8)$ seeks when writing, and $8\times K\times (M/8)$ bytes to read a row, but no seek when reading.

                                M
        ___________________________________________________________
       /                                                           \
            8         8         8         8                   8
       +---------+---------+---------+---------+--     --+---------+
       |         |         |         |         |         |         |
     K |    0    |    1    |    2    |    3    |   ...   |  M/8-1  |
       |         |         |         |         |         |         |
       +---------+---------+---------+---------+--     --+---------+
       |         |         |         |         |         |         | 
     K |   M/8   |  M/8+1  |  M/8+2  |  M/8+3  |   ...   | 2M/8-1  |
       |         |         |         |         |         |         | 
       +---------+---------+---------+---------+--     --+---------+
       |         |         |         |         |         |         |     
    

For example choosing $K=2^10$ we have 64kb blocks of size $K\times 8$, and reading a row requires reading $8\times K\times M \approx 2\textrm{ MB}$ of data.

Creating this "semi-transposed" structure takes $(N'/K)\times(M/8)\approx 300k$ seeks, which should be hopefully acceptable on an SSD; and then can be copied to a HDD linearly.

