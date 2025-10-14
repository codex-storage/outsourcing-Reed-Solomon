
#include <stdint.h>
#include <stdio.h>      // for testing only
#include <assert.h>

#include "goldilocks.h"

//------------------------------------------------------------------------------

#define GOLDILOCKS_HALFPRIME_PLUS1 0x7fffffff80000001

//------------------------------------------------------------------------------
// *** Goldilocks field ***

int goldilocks_isvalid(uint64_t x) {
  return (x < GOLDILOCKS_PRIME);
}

uint64_t goldilocks_neg(uint64_t x) {
  return (x==0) ? 0 : (GOLDILOCKS_PRIME - x);
}

uint64_t goldilocks_add(uint64_t x, uint64_t y) {
  uint64_t z = x + y;
  return ( (z >= GOLDILOCKS_PRIME) || (z<x) ) ? (z - GOLDILOCKS_PRIME) : z;
}

uint64_t goldilocks_add_to_uint64(uint64_t x, uint64_t y) {
  uint64_t z = x + y;
  return (z<x) ? (z - GOLDILOCKS_PRIME) : z;
}

uint64_t goldilocks_sub(uint64_t x, uint64_t y) {
  uint64_t z = x - y;
  return (z > x) ? (z + GOLDILOCKS_PRIME) : z;
}

uint64_t goldilocks_sub_safe(uint64_t x, uint64_t y) {
  return goldilocks_add( x , goldilocks_neg(y) );
}

//--------------------------------------

uint64_t goldilocks_rdc(__uint128_t x) {
  // x = n0 + 2^64 * n1 + 2^96 * n2
  uint64_t n0 = (uint64_t)x;
  uint64_t n1 = (x >> 64) & 0xffffffff;
  uint64_t n2 = (x >> 96);
  
  uint64_t mid = (n1 << 32) - n1;     // (2^32 - 1) * n1
  uint64_t tmp = n0 + mid;
  if (tmp < n0) { tmp -= GOLDILOCKS_PRIME; }

  uint64_t res = tmp - n2;
  if (res > tmp) { res += GOLDILOCKS_PRIME; }
  return (res >= GOLDILOCKS_PRIME) ? (res - GOLDILOCKS_PRIME) : res;
}

// reduce to 64-bit, but it can be still bigger than `p`
uint64_t goldilocks_rdc_to_uint64(__uint128_t x) {
  // x = n0 + 2^64 * n1 + 2^96 * n2
  uint64_t n0 = (uint64_t)x;
  uint64_t n1 = (x >> 64) & 0xffffffff;
  uint64_t n2 = (x >> 96);
  
  uint64_t mid = (n1 << 32) - n1;     // (2^32 - 1) * n1
  uint64_t tmp = n0 + mid;
  if (tmp < n0) { tmp -= GOLDILOCKS_PRIME; }

  uint64_t res = tmp - n2;
  if (res > tmp) { res += GOLDILOCKS_PRIME; }
  return res;
}

// we assume x < 2^96
uint64_t goldilocks_rdc_small(__uint128_t x) {
  // x = n0 + 2^64 * n1
  uint64_t n0 = (uint64_t)x;
  uint64_t n1 = (x >> 64);

  uint64_t mid = (n1 << 32) - n1;     // (2^32 - 1) * n1
  uint64_t tmp = n0 + mid;
  if (tmp < n0) { tmp -= GOLDILOCKS_PRIME; }

  uint64_t res = tmp;
  return (res >= GOLDILOCKS_PRIME) ? (res - GOLDILOCKS_PRIME) : res;
}

//--------------------------------------

uint64_t goldilocks_mul(uint64_t x, uint64_t y) {
  __uint128_t z = (__uint128_t)x * (__uint128_t)y;
  return goldilocks_rdc(z); 
}

uint64_t goldilocks_mul_to_uint64(uint64_t x, uint64_t y) {
  __uint128_t z = (__uint128_t)x * (__uint128_t)y;
  return goldilocks_rdc_to_uint64(z); 
}

uint64_t goldilocks_mul_add128(uint64_t x, uint64_t y, __uint128_t z) {
  __uint128_t w = (__uint128_t)x * (__uint128_t)y + z;
  return goldilocks_rdc(w); 
}

uint64_t goldilocks_sqr(uint64_t x) {
  __uint128_t z = (__uint128_t)x * (__uint128_t)x;
  return goldilocks_rdc(z); 
}

uint64_t goldilocks_sqr_add(uint64_t x, uint64_t y) {
  __uint128_t z = (__uint128_t)x * x + y;
  return goldilocks_rdc(z); 
}

// only reduce to uint64, not to [0..p-1]
uint64_t goldilocks_sqr_add_to_uint64(uint64_t x, uint64_t y) {
  __uint128_t z = (__uint128_t)x * x + y;
  return goldilocks_rdc_to_uint64(z); 
}

uint64_t goldilocks_mul_small(uint64_t x, uint32_t y) {
  __uint128_t z = (__uint128_t)x * (__uint128_t)y;
  return goldilocks_rdc_small(z); 
}

//------------------------------------------------------------------------------

uint64_t goldilocks_euclid(uint64_t x0, uint64_t y0, uint64_t u0, uint64_t v0) {

  uint64_t x = x0;
  uint64_t y = y0;
  uint64_t u = u0;
  uint64_t v = v0;

  while( ( (u!=1) && (v!=1) ) ) {

    while (!(u & 1ull)) {
      u = u >> 1;
      int odd = x & 1ull;
      x = x >> 1;
      if (odd) { x += GOLDILOCKS_HALFPRIME_PLUS1; }
    }

    while (!(v & 1ull)) {
      v = v >> 1;
      int odd = y & 1ull;
      y = y >> 1;
      if (odd) { y += GOLDILOCKS_HALFPRIME_PLUS1; }
    }

    if (u < v) {
      // u-v < 0, that is, u < v
      v = v - u;
      y = goldilocks_sub(y , x);
    }
    else {
      // u-v >= 0, that is, u >= v
      u = u - v;
      x = goldilocks_sub(x , y);
    }
  
  }

  if (u == 1) { 
    return x;
  } 
  else { 
    return y;
  }
}

uint64_t goldilocks_div(uint64_t a, uint64_t b) {
  return goldilocks_euclid(a,0,b,GOLDILOCKS_PRIME);
}

uint64_t goldilocks_inv(uint64_t a) {
  return goldilocks_div(1, a);
}

//------------------------------------------------------------------------------

uint64_t goldilocks_pow(uint64_t base, int expo) {
  if (expo == 0) { return 1; }
  if (expo <  0) { return goldilocks_pow( goldilocks_inv(base) , -expo ); }

  int      e   = expo;
  uint64_t sq  = base;
  uint64_t acc = 1;

  while (e != 0) { 
    if ((e & 1) != 0) {
      acc = goldilocks_mul( acc, sq );
    } 
    if (e > 0) {
      sq = goldilocks_mul( sq , sq );
      e  = e >> 1;
    }
  }

  return acc;
}

//==============================================================================
// *** debugging ***

void debug_print_state(const char *msg, int n, uint64_t *state) {
  printf("-----------------\n");
  printf("%s\n",msg);
  for(int i=0;i<n;i++) {
    printf(" - 0x%016llx = %llu\n",state[i],state[i]);
  }
}

//------------------------------------------------------------------------------

#define MASK 0x3fffffffffffffffULL

// NOTE: we assume a little-endian architecture
void goldilocks_convert_31_bytes_to_4_field_elements(const uint8_t *ptr, uint64_t *felts) {
  const uint64_t *q0  = (const uint64_t*)(ptr   );
  const uint64_t *q7  = (const uint64_t*)(ptr+ 7);
  const uint64_t *q15 = (const uint64_t*)(ptr+15);
  const uint64_t *q23 = (const uint64_t*)(ptr+23);

  felts[0] =  (q0 [0]) & MASK;
  felts[1] = ((q7 [0]) >> 6) | ((uint64_t)(ptr[15] & 0x0f) << 58);
  felts[2] = ((q15[0]) >> 4) | ((uint64_t)(ptr[23] & 0x03) << 60); 
  felts[3] = ((q23[0]) >> 2);
}

void goldilocks_convert_bytes_to_field_elements(int rate, const uint8_t *ptr, uint64_t *felts) {
  switch(rate) {

    case 4:
      goldilocks_convert_31_bytes_to_4_field_elements(ptr, felts);
      break;

    case 8:
      goldilocks_convert_31_bytes_to_4_field_elements(ptr   , felts  ); 
      goldilocks_convert_31_bytes_to_4_field_elements(ptr+31, felts+4);
      break;

    default:
      assert( 0 );
      break;
  }
}

//------------------------------------------------------------------------------
