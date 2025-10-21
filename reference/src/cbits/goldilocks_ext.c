
// quadratic field extension F[x] = F(x) / (x^2 - 7) over the Goldilocks field

#include "goldilocks.h"
#include "goldilocks_ext.h"

//------------------------------------------------------------------------------

void goldilocks_ext_inj( uint64_t input , uint64_t *out ) {
  out[0] = input;
  out[1] = 0;
}

void goldilocks_ext_set_one( uint64_t *out ) {
  out[0] = 1;
  out[1] = 0;
}

//------------------------------------------------------------------------------

void goldilocks_ext_neg(const uint64_t *x , uint64_t *out) {
  out[0] = goldilocks_neg( x[0] );
  out[1] = goldilocks_neg( x[1] );
}

void goldilocks_ext_add(const uint64_t *x , const uint64_t *y , uint64_t *out) {
  out[0] = goldilocks_add( x[0] , y[0] );
  out[1] = goldilocks_add( x[1] , y[1] );
}

void goldilocks_ext_sub(const uint64_t *x , const uint64_t *y , uint64_t *out) {
  out[0] = goldilocks_sub( x[0] , y[0] );
  out[1] = goldilocks_sub( x[1] , y[1] );
}

void goldilocks_ext_scl(uint64_t s , const uint64_t *x , uint64_t *out) {
  out[0] = goldilocks_mul( s , x[0] );
  out[1] = goldilocks_mul( s , x[1] );
}

//------------------------------------------------------------------------------

/*
sqrNaive :: F2 -> F2
sqrNaive (F2 r i) = F2 r3 i3 where
  r3 = r*r + 7 * i*i
  i3 = 2 * r*i

-- uses Karatsuba trick to have one less multiplications
mulKaratsuba :: F2 -> F2 -> F2
mulKaratsuba (F2 r1 i1) (F2 r2 i2) = F2 r3 i3 where
  u = r1*r2
  w = i1*i2
  v = (r1+i1)*(r2+i2)
  r3 = u + 7*w
  i3 = v - u - w
*/

void goldilocks_ext_sqr(const uint64_t *x , uint64_t *out) {
  uint64_t u = goldilocks_a_plus_7b( goldilocks_sqr(x[0]) , goldilocks_sqr( x[1] )) ;
  uint64_t v = goldilocks_mul_by_2( goldilocks_mul( x[0] , x[1] ) );
  out[0] = u;   
  out[1] = v;     // because we want inplace to work too
}

// uses Karatsuba trick to have one less multiplications
void goldilocks_ext_mul(const uint64_t *x , const uint64_t *y , uint64_t *out) {
  uint64_t u = goldilocks_mul( x[0] , y[0] );
  uint64_t w = goldilocks_mul( x[1] , y[1] );
  uint64_t v = goldilocks_mul( goldilocks_add( x[0] , x[1] ) , goldilocks_add( y[0] , y[1] ) );
  out[0] = goldilocks_a_plus_7b( u , w );
  out[1] = goldilocks_sub( v , goldilocks_add( u , w ) );
}

//------------------------------------------------------------------------------

// We can solve the equation explicitly.
//
// > irred = x^2 + p*x + q
// > (a*x + b) * (c*x + d) = (a*c)*x^2 + (a*d+b*c)*x + (b*d)
// >                       = (a*d + b*c - a*c*p)*x + (b*d - a*c*q)
//
// and then we want to solve
//
// > b*d       - a*c*q == 1
// > a*d + b*c - a*c*p == 0
//
// which has the solution:
//
// > c = - a       / (b^2 - a*b*p + a^2*q)  
// > d = (b - a*p) / (b^2 - a*b*p + a^2*q)
//
// Remark: It seems the denominator being zero would mean that our
// defining polynomial is not irreducible.
//
// Note: we can optimize for the common case p=0; and also for q=1.
//
void goldilocks_ext_inv(const uint64_t *ptr , uint64_t *out) {
  uint64_t b = ptr[0];
  uint64_t a = ptr[1];   // (a*x + b)
  uint64_t denom = goldilocks_a_minus_7b( goldilocks_sqr(b) , goldilocks_sqr(a) );
  uint64_t denom_inv = goldilocks_inv( denom );
  out[0] =                 goldilocks_mul( b , denom_inv )  ;
  out[1] = goldilocks_neg( goldilocks_mul( a , denom_inv ) );  
}

void goldilocks_ext_div(const uint64_t *x , const uint64_t *y , uint64_t *out) {
  uint64_t invy[2];
  goldilocks_ext_inv( y , invy );
  goldilocks_ext_mul( x , invy , out );
}

//------------------------------------------------------------------------------

void goldilocks_ext_pow(const uint64_t *base , int expo , uint64_t *out) {

  if (expo == 0) { 
    goldilocks_ext_set_one(out);
    return;
  }

  if (expo <  0) { 
    uint64_t base_inv[2];
    goldilocks_ext_inv( base , base_inv );
    goldilocks_ext_pow( base_inv , -expo , out ); 
    return;
  }

  int      e   = expo;
  uint64_t sq [2] ; sq[0] = base[0] ; sq[1] = base[1] ;
  uint64_t acc[2] ; goldilocks_ext_set_one( acc );

  while (e != 0) { 
    if ((e & 1) != 0) {
      goldilocks_ext_mul( acc , sq , acc );
    } 
    if (e > 0) {
      goldilocks_ext_sqr( sq , sq );
      e  = e >> 1;
    }
  }

  out[0] = acc[0];
  out[1] = acc[1];
}

//------------------------------------------------------------------------------
