
#include <stdint.h>

//------------------------------------------------------------------------------

void goldilocks_ntt_forward           (              int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);
void goldilocks_ntt_forward_shifted   (uint64_t eta, int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);

void goldilocks_ntt_forward_asymmetric(int m_src, int m_tgt, uint64_t gen_src, uint64_t gen_tgt, const uint64_t *src, uint64_t *tgt);

void goldilocks_ntt_inverse           (              int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);
void goldilocks_ntt_inverse_shifted   (uint64_t eta, int m, uint64_t gen, const uint64_t *src, uint64_t *tgt);

//------------------------------------------------------------------------------
