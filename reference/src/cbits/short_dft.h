
#ifndef _SHORT_DFT_H_INCLUDED_

#include <stdint.h>

//------------------------------------------------------------------------------

void short_fwd_DFT_size_4         ( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_4_unscaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_4_rescaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );

void short_fwd_DFT_size_8         ( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_8_unscaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_8_rescaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );

void short_fwd_DFT_size_16         ( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_16_unscaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
void short_inv_DFT_size_16_rescaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );

//------------------------------------------------------------------------------

// void short_fwd_DFT_size_4_ext         ( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
// void short_inv_DFT_size_4_ext_unscaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );
// void short_inv_DFT_size_4_ext_rescaled( int src_stride, int tgt_stride, const uint64_t *src, uint64_t *tgt );

//------------------------------------------------------------------------------

#endif // _SHORT_DFT_H_INCLUDED_
