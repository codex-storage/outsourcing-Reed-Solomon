

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "goldilocks.h"
#include "ntt.h"

// -----------------------------------------------------------------------------

void goldilocks_ntt_forward_noalloc(int m, int src_stride, const uint64_t *gpows, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    tgt[0] = src[0];
    return;
  }

  if (m==1) {
    // N = 2
    tgt[0] = goldilocks_add( src[0] , src[src_stride] );    // x + y
    tgt[1] = goldilocks_sub( src[0] , src[src_stride] );    // x - y
    return;
  }

  else {
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    goldilocks_ntt_forward_noalloc( m-1 , src_stride<<1 , gpows , src              , buf + N , buf         );
    goldilocks_ntt_forward_noalloc( m-1 , src_stride<<1 , gpows , src + src_stride , buf + N , buf + halfN );

    for(int j=0; j<halfN; j++) {
      const uint64_t gpow = gpows[j*src_stride];
      tgt[j      ] = goldilocks_mul( buf[j+halfN] , gpow   );   //   g*v[k]
      tgt[j+halfN] = goldilocks_neg( tgt[j      ]          );   // - g*v[k]
      tgt[j      ] = goldilocks_add( tgt[j      ] , buf[j] );   // u[k] + g*v[k]
      tgt[j+halfN] = goldilocks_add( tgt[j+halfN] , buf[j] );   // u[k] - g*v[k]
    }
  }
}

// forward number-theoretical transform (evaluation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void goldilocks_ntt_forward(int m, const uint64_t gen, const uint64_t *src, uint64_t *tgt) {
  int N     = (1<<m);
  int halfN = (N>>1);
  
  // precalculate [1,g,g^2,g^3...]
  uint64_t *gpows = (uint64_t*) malloc( 8 * halfN );
  assert( gpows != 0 );
  uint64_t x = gen;
  gpows[0] = 1;
  gpows[1] = gen;
  for(int i=2; i<halfN; i++) {
    x = goldilocks_mul( x , gen );
    gpows[i] = x;
  }
  
  uint64_t *buf = (uint64_t*) malloc( 8 * (2*N) );
  assert( buf != 0 );
  goldilocks_ntt_forward_noalloc( m, 1, gpows, src, buf, tgt);
  free(buf);
  free(gpows);
}

// it's like `ntt_forward` but we pre-multiply the coefficients with `eta^k`
// resulting in evaluating f(eta*x) instead of f(x)
void goldilocks_ntt_forward_shifted(const uint64_t eta, int m, const uint64_t gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *shifted = malloc( 8 * N );
  assert( shifted != 0 );
  uint64_t x = 1;
  for(int i=0; i<N; i++) {
    shifted[i] = goldilocks_mul( src[i] , x );
    x = goldilocks_mul( x , eta );
  }
  goldilocks_ntt_forward( m, gen, shifted, tgt );
  free(shifted);
}

// it's like `ntt_forward` but asymmetric, evaluating on a larger target subgroup
void goldilocks_ntt_forward_asymmetric(int m_src, int m_tgt, const uint64_t gen_src, const uint64_t gen_tgt, const uint64_t *src, uint64_t *tgt) {
  assert( m_tgt >= m_src );
  int N_src = (1 << m_src);
  int N_tgt = (1 << m_tgt);
  int halfN_src = (N_src >> 1);
  int K = (1 << (m_tgt - m_src));
  
  // precalculate [1,g,g^2,g^3...]
  uint64_t *gpows = malloc( 8 * halfN_src );
  assert( gpows != 0 );
  uint64_t x = gen_src;
  gpows[0] = 1;
  gpows[1] = gen_src;
  for(int i=2; i<halfN_src; i++) {
    x = goldilocks_mul(x, gen_src);
    gpows[i] = x;
  }
  
  uint64_t *shifted = malloc( 8 * N_src );
  assert( shifted != 0 );
  
  uint64_t *buf = malloc( 8 * (2*N_src) );
  assert( buf != 0 );
  
  // temporary target buffer (we could replace this by adding `tgt_stride`)
  uint64_t *tgt_small = malloc( 8 * N_src );
  assert( tgt_small != 0 );
  
  // eta will be the shift
  uint64_t eta = 1;

  for(int k=0; k<K; k++) {
    if (k==0) {
      memcpy( shifted, src, N_src*8 );
    }
    else {
      eta = goldilocks_mul( eta , gen_tgt );
      
      uint64_t x = 1;
      for(int i=0; i<N_src; i++) {
        shifted[i] = goldilocks_mul( src[i] , x );
        x = goldilocks_mul(x, eta);
      }
    }
    
    goldilocks_ntt_forward_noalloc( m_src, 1, gpows, shifted, buf, tgt_small );
    
    uint64_t *p = tgt_small;
    uint64_t *q = tgt + k;
    int tgt_stride = K;
    for(int i=0; i<N_src; i++) {
      q[i] = p[i];
      p += 1;
      q += tgt_stride;
    }
    
  }
  
  free(tgt_small);
  free(buf);
  free(gpows);
  free(shifted);
}


// -----------------------------------------------------------------------------
 
// inverse of 2 (which is is the same as `(p+1)/2`)
const uint64_t goldilocks_oneHalf = 0x7fffffff80000001ull;

void goldilocks_ntt_inverse_noalloc(int m, int tgt_stride, const uint64_t *gpows, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    tgt[0] = src[0];
    return;
  }

  if (m==1) {
    // N = 2

    tgt[0         ] = goldilocks_add( src[0] , src[1] );           // x + y
    tgt[tgt_stride] = goldilocks_sub( src[0] , src[1] );           // x - y
    return;
  }

  else {
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    for(int j=0; j<halfN; j++) {
      uint64_t gpow = gpows[j*tgt_stride];
      buf[j      ] = goldilocks_add( src[j] , src[j+halfN] );       // x + y
      buf[j+halfN] = goldilocks_sub( src[j] , src[j+halfN] );       // x - y
      buf[j+halfN] = goldilocks_mul( buf[j+halfN] , gpow );         // (x - y) / g^k
    }

    goldilocks_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpows , buf         , buf + N , tgt              );
    goldilocks_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpows , buf + halfN , buf + N , tgt + tgt_stride );
  }
}

// inverse number-theoretical transform (interpolation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void goldilocks_ntt_inverse(int m, const uint64_t gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  int halfN = (N>>1);
  
  // precalculate [1,g^{-1},g^{-2},g^{-3}...]
  uint64_t *gpows = malloc( 8 * halfN );
  assert( gpows != 0 );
  uint64_t x    = 1;
  uint64_t ginv = goldilocks_inv(gen);        // gen^-1
  for(int i=0; i<halfN; i++) {
    gpows[i] = x;
    x = goldilocks_mul(x, ginv);
  }
  
  uint64_t *buf = malloc( 8 * (2*N) );
  assert( buf !=0 );
  goldilocks_ntt_inverse_noalloc( m, 1, gpows, src, buf, tgt );

  uint64_t rescale = goldilocks_inv( N );
  for(int i=0; i<N; i++) {
    tgt[i] = goldilocks_mul( tgt[i] , rescale );
  }

  free(buf);
  free(gpows);
}

// it's like `ntt_inverse` but we post-multiply the resulting coefficients with `eta^k`
// resulting in interpolating an f such that f(eta^-1 * omega^k) = y_k
void goldilocks_ntt_inverse_shifted(const uint64_t eta, int m, const uint64_t gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *unshifted = malloc( 8*N );
  assert( unshifted != 0 );
  goldilocks_ntt_inverse( m, gen, src, unshifted );
  uint64_t x = 1;
  for(int i=0; i<N; i++) {
    tgt[i] = goldilocks_mul( unshifted[i] , x );
    x      = goldilocks_mul( x , eta );
  }
  free(unshifted);
}

// -----------------------------------------------------------------------------
 