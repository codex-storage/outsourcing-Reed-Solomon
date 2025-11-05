
// quadratic field extension F[x] = F(x) / (x^2 - 7) over the Goldilocks field

#ifndef _GOLDILOCKS_EXT_H_INCLUDED_

#include <stdint.h>
#include "goldilocks.h"

//------------------------------------------------------------------------------

void goldilocks_ext_inj( uint64_t input , uint64_t *out );
void goldilocks_ext_set_one( uint64_t *out );

void goldilocks_ext_neg(const uint64_t *x                     , uint64_t *out);
void goldilocks_ext_add(const uint64_t *x , const uint64_t *y , uint64_t *out);
void goldilocks_ext_sub(const uint64_t *x , const uint64_t *y , uint64_t *out);
void goldilocks_ext_scl(       uint64_t s , const uint64_t *x , uint64_t *out);
void goldilocks_ext_sqr(const uint64_t *x                     , uint64_t *out);
void goldilocks_ext_mul(const uint64_t *x , const uint64_t *y , uint64_t *out);
void goldilocks_ext_inv(const uint64_t *x                     , uint64_t *out);
void goldilocks_ext_div(const uint64_t *x , const uint64_t *y , uint64_t *out);
void goldilocks_ext_pow(const uint64_t *b , int e             , uint64_t *out);

//------------------------------------------------------------------------------

#endif // _GOLDILOCKS_EXT_H_INCLUDED_
