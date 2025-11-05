

#ifndef _MONOLITH_H_INCLUDED_

#include <stdint.h>

//------------------------------------------------------------------------------

void goldilocks_monolith_permutation     (uint64_t *state);
void goldilocks_monolith_permutation_into(uint64_t *src, uint64_t *tgt);
void goldilocks_monolith_keyed_compress  (const uint64_t *x, const uint64_t *y, uint64_t key, uint64_t *out);
void goldilocks_monolith_compress        (const uint64_t *x, const uint64_t *y,               uint64_t *out);
void goldilocks_monolith_bytes_digest    (int rate, int N, const uint8_t  *input, uint64_t *hash);
void goldilocks_monolith_felts_digest    (int rate, int N, const uint64_t *input, uint64_t *hash);

//------------------------------------------------------------------------------

#endif // _MONOLITH_H_INCLUDED_
