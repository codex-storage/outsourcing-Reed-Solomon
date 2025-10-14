
#include <assert.h>

#include "goldilocks.h"
#include "monolith.h"

//==============================================================================
// *** Monolith hash ***
//
// compatible with <https://extgit.iaik.tugraz.at/krypto/zkfriendlyhashzoo>
//

/* 
monolith test vector (permutation of [0..11]) 
---------------------------------------------
from <https://extgit.iaik.tugraz.at/krypto/zkfriendlyhashzoo/-/blob/master/plain_impls/src/monolith_64/monolith_64.rs?ref_type=heads#L653>

0x516dd661e959f541 = 5867581605548782913
0x082c137169707901 = 588867029099903233
0x53dff3fd9f0a5beb = 6043817495575026667
0x0b2ebaa261590650 = 805786589926590032
0x89aadb57e2969cb6 = 9919982299747097782
0x5d3d6905970259bd = 6718641691835914685
0x6e5ac1a4c0cfa0fe = 7951881005429661950
0xd674b7736abfc5ce = 15453177927755089358
0x0d8697e1cd9a235f = 974633365445157727
0x85fc4017c247136e = 9654662171963364206
0x572bafd76e511424 = 6281307445101925412
0xbec1638e28eae57f = 13745376999934453119

*/

//--------------------------------------
// ** sbox layer

// based on the reference implementation from 
// <https://extgit.iaik.tugraz.at/krypto/zkfriendlyhashzoo>
uint64_t goldilocks_monolith_single_bar(uint64_t x) {

  //  uint64_t y1 = ((x & 0x8080808080808080) >> 7) | ((x & 0x7F7F7F7F7F7F7F7F) << 1); 
  //  uint64_t y2 = ((x & 0xC0C0C0C0C0C0C0C0) >> 6) | ((x & 0x3F3F3F3F3F3F3F3F) << 2); 
  //  uint64_t y3 = ((x & 0xE0E0E0E0E0E0E0E0) >> 5) | ((x & 0x1F1F1F1F1F1F1F1F) << 3); 
  //  uint64_t z  = x ^ ((~y1) & y2 & y3);
  //  uint64_t r  = ((z  & 0x8080808080808080) >> 7) | ((z  & 0x7F7F7F7F7F7F7F7F) << 1);

  const uint64_t mask80 = 0x8080808080808080;
  const uint64_t mask7F = ~mask80;
  uint64_t y1 = ((x  & mask80) >> 7) | ((x  & mask7F) << 1); 
  uint64_t y2 = ((y1 & mask80) >> 7) | ((y1 & mask7F) << 1); 
  uint64_t y3 = ((y2 & mask80) >> 7) | ((y2 & mask7F) << 1); 
  uint64_t z  = x ^ ((~y1) & y2 & y3);
  uint64_t r  = ((z  & mask80) >> 7) | ((z  & mask7F) << 1);
  return r;
}

// the sbox-layer (note: it's only applied to the first 4 field elements!)
void goldilocks_monolith_bars(uint64_t *state) {
  for(int j=0; j<4; j++) { state[j] = goldilocks_monolith_single_bar(state[j]); }
}

//--------------------------------------
// ** nonlinear layer

// the nonlinear layer
//
// remark: since the next layer is always the linear diffusion, it's enough
// to reduce to 64 bit, don't have to reduce to [0..p-1]. 
// As in the linear layer we split into two 32 bit words anyway.
void goldilocks_monolith_bricks(uint64_t *state) {
  for(int i=11; i>0; i--) state[i] = goldilocks_sqr_add_to_uint64( state[i-1] , state[i] );
}

//--------------------------------------
// ** fast diffusion layer

#include "monolith_conv_uint64.inc"

// we split the input to low and high 32 bit words
// do circular convolution on them, which safe because there is no overflow in 64 bit words
// but should be much faster as there are no modulo operations just 64-bit machine word ops
// then reconstruct and reduce at the end
void goldilocks_monolith_concrete(uint64_t *state) {
  uint64_t lo[12];
  uint64_t hi[12];
 
  for(int i=0; i<12; i++) { 
    uint64_t x = state[i];
    lo[i] = x & 0xffffffff;
    hi[i] = x >> 32;
  }

  uint64_circular_conv_12_with( lo , lo );
  uint64_circular_conv_12_with( hi , hi );

  for(int i=0; i<12; i++) {
    __uint128_t x = (((__uint128_t)hi[i]) << 32) + lo[i];
    state[i] = goldilocks_rdc_small(x);
  }
}

void goldilocks_monolith_concrete_rc(uint64_t *state, const uint64_t *rc) {
  uint64_t lo[12];
  uint64_t hi[12];
 
  for(int i=0; i<12; i++) { 
    uint64_t x = state[i];
    lo[i] = x & 0xffffffff;
    hi[i] = x >> 32;
  }

  uint64_circular_conv_12_with( lo , lo );
  uint64_circular_conv_12_with( hi , hi );

  for(int i=0; i<12; i++) {
    __uint128_t x = (((__uint128_t)hi[i]) << 32) + lo[i] + rc[i];
    state[i] = goldilocks_rdc_small(x);
  }
}

//--------------------------------------
// ** rounds

#include "monolith_constants.inc"

void goldilocks_monolith_round(int round_idx, uint64_t *state) {
  goldilocks_monolith_bars       (state);
  goldilocks_monolith_bricks     (state);
  goldilocks_monolith_concrete_rc(state , &(monolith_t12_round_constants[round_idx][0]) );
}

void goldilocks_monolith_permutation(uint64_t *state) {
  // initial layer
  goldilocks_monolith_concrete(state);
  // five rounds with RC
  for(int r=0; r<5; r++) {
    goldilocks_monolith_round(r, state);
  }
  // last round, no RC
  goldilocks_monolith_bars    (state);
  goldilocks_monolith_bricks  (state);
  goldilocks_monolith_concrete(state);
}

//------------------------------------------------------------------------------

// compression function: input is two 4-element vector of field elements, 
// and the output is a vector of 4 field elements
void goldilocks_monolith_keyed_compress(const uint64_t *x, const uint64_t *y, uint64_t key, uint64_t *out) {
  uint64_t state[12];
  for(int i=0; i<4; i++) {
    state[i  ] = x[i];
    state[i+4] = y[i];
    state[i+8] = 0;
  }
  state[8] = key;
  goldilocks_monolith_permutation(state);
  for(int i=0; i<4; i++) {
    out[i] = state[i];
  }
}

void goldilocks_monolith_compress(const uint64_t *x, const uint64_t *y, uint64_t *out) {
  goldilocks_monolith_keyed_compress(x, y, 0, out);
}

//------------------------------------------------------------------------------

// hash a sequence of field elements into a digest of 4 field elements
void goldilocks_monolith_felts_digest(int rate, int N, const uint64_t *input, uint64_t *hash) {

  assert( (rate >= 1) && (rate <= 8) );

  uint64_t domsep = rate + 256*12 + 65536*63;
  uint64_t state[12];
  for(int i=0; i<12; i++) state[i] = 0;
  state[8] = domsep;

  int nchunks = (N + rate) / rate;       // 10* padding
  const uint64_t *ptr = input;
  for(int k=0; k<nchunks-1; k++) {
    for(int j=0; j<rate; j++) { state[j] = goldilocks_add( state[j] , ptr[j] ); }
    goldilocks_monolith_permutation( state );
    ptr += rate;
  }

  int rem = nchunks*rate - N;       // 0 < rem <= rate
  int ofs = rate - rem; 

  // the last block, with padding
  uint64_t last[8];
  for(int i=0    ; i<ofs ; i++) last[i] = ptr[i];
  for(int i=ofs+1; i<rate; i++) last[i] = 0;
  last[ofs] = 0x01;
  for(int j=0; j<rate; j++) { state[j] = goldilocks_add( state[j] , last[j] ); }
  goldilocks_monolith_permutation( state );

  for(int j=0; j<4; j++) { hash[j] = state[j]; }
}

//--------------------------------------

void goldilocks_monolith_bytes_digest(int rate, int N, const uint8_t *input, uint64_t *hash) {

  assert( (rate == 4) || (rate == 8) );

  uint64_t domsep = rate + 256*12 + 65536*8;
  uint64_t state[12];
  for(int i=0; i<12; i++) state[i] = 0;
  state[8] = domsep;

  uint64_t felts[8];

  int rate_in_bytes  = 31 * (rate>>2);                   // 31 or 62
  int nchunks = (N + rate_in_bytes) / rate_in_bytes;     // 10* padding
  const uint8_t *ptr = input;
  for(int k=0; k<nchunks-1; k++) {
    goldilocks_convert_bytes_to_field_elements(rate, ptr, felts);
    for(int j=0; j<rate; j++) { state[j] = goldilocks_add( state[j] , felts[j] ); }
    goldilocks_monolith_permutation( state );
    ptr += rate_in_bytes;
  }

  int rem = nchunks*rate_in_bytes - N;       // 0 < rem <= rate_in_bytes 
  int ofs = rate_in_bytes - rem; 
  uint8_t last[62];

  // last block, with padding
  for(int i=0    ; i<ofs          ; i++) last[i] = ptr[i];
  for(int i=ofs+1; i<rate_in_bytes; i++) last[i] = 0;
  last[ofs] = 0x01;
  goldilocks_convert_bytes_to_field_elements(rate, last, felts);
  for(int j=0; j<rate; j++) { state[j] = goldilocks_add( state[j] ,felts[j] ); }
  goldilocks_monolith_permutation( state );

  for(int j=0; j<4; j++) { hash[j] = state[j]; }
}

//------------------------------------------------------------------------------
