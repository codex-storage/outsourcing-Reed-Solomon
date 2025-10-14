
#include <stdint.h>

//------------------------------------------------------------------------------

#define GOLDILOCKS_PRIME 0xffffffff00000001

//------------------------------------------------------------------------------

int goldilocks_isvalid(uint64_t x);

uint64_t goldilocks_neg(uint64_t x);
uint64_t goldilocks_add(uint64_t x, uint64_t y);
uint64_t goldilocks_sub(uint64_t x, uint64_t y);
uint64_t goldilocks_sqr(uint64_t x);
uint64_t goldilocks_mul(uint64_t x, uint64_t y);
uint64_t goldilocks_mul_small(uint64_t x, uint32_t y);
uint64_t goldilocks_inv(uint64_t a);
uint64_t goldilocks_div(uint64_t a, uint64_t b);
uint64_t goldilocks_pow(uint64_t b, int e);

//------------------------------------------------------------------------------

uint64_t goldilocks_rdc          (__uint128_t x);
uint64_t goldilocks_rdc_to_uint64(__uint128_t x);
uint64_t goldilocks_rdc_small    (__uint128_t x);

uint64_t goldilocks_mul_to_uint64    (uint64_t x, uint64_t y);
uint64_t goldilocks_mul_add128       (uint64_t x, uint64_t y, __uint128_t z);
uint64_t goldilocks_sqr_add          (uint64_t x, uint64_t y);
uint64_t goldilocks_sqr_add_to_uint64(uint64_t x, uint64_t y);
uint64_t goldilocks_mul_small        (uint64_t x, uint32_t y);

//------------------------------------------------------------------------------

void goldilocks_convert_31_bytes_to_4_field_elements (      const uint8_t *ptr, uint64_t *felts );
void goldilocks_convert_bytes_to_field_elements ( int rate, const uint8_t *ptr, uint64_t *felts );

//------------------------------------------------------------------------------
