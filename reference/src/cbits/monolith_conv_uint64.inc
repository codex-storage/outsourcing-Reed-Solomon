
//
// circular convolution with the vector [7,8,21,22,6,7,9,10,13,26,8,23] algorithms in uint64_t
// the idea is that we can split field elements into (lo + 2^32*hi)
// apply the convolution separately (it won't overflow)
// then combine and reduce
//
// based on the book:
//
// Nussbaumer: "Fast Fourier Transform and Convolution Algorithms"
//

/*

our coefficient vectors:

  [7,8,21,22,6,7,9,10,13,26,8,23]

in CRT rectangle format:

  +----------+
  |  7  6 13 |
  |  26 8  7 | 
  |  9  8 21 |
  | 22 10 23 |
  +----------+

*/

#include <stdint.h>

//------------------------------------------------------------------------------

// convolves with:  b2 = { 64 , 32 , 64 };
//   tgt[0] = 64*x + 64*y + 32*z
//   tgt[1] = 32*x + 64*y + 64*z
//   tgt[2] = 64*x + 32*y + 64*z
void uint64_convolve_with_B2(uint64_t *src, uint64_t *tgt) {
  uint64_t x = src[0];
  uint64_t y = src[1];
  uint64_t z = src[2];

  uint64_t x32 = x << 5;
  uint64_t y32 = y << 5;
  uint64_t z32 = z << 5;

  uint64_t s64 = (x32 + y32 + z32) << 1;

  tgt[0] = s64 - z32;
  tgt[1] = s64 - x32;
  tgt[2] = s64 - y32;
}


// convolves with:  b3 = { -32 , -4 ,   4 };
//   tgt[0] = -32*x +  4*y -  4*z
//   tgt[1] =  -4*x - 32*y + 64*z
//   tgt[2] =   4*x -  4*y - 32*z
void uint64_convolve_with_B3(uint64_t *src, uint64_t *tgt) {
  uint64_t x = src[0];
  uint64_t y = src[1];
  uint64_t z = src[2];

  uint64_t x4 = x << 2;
  uint64_t y4 = y << 2;
  uint64_t z4 = z << 2;

  uint64_t x32 = x4 << 3;
  uint64_t y32 = y4 << 3;
  uint64_t z32 = z4 << 3;

  tgt[0] = - x32 + y4  - z4;
  tgt[1] = - x4  - y32 + z4; 
  tgt[2] =   x4  - y4  - z32;
}

// convolves with:  b4 = { -6 , 0 , 8 };
//   tgt[0] = - 6*x + 8*y 
//   tgt[1] =       - 6*y + 8*z
//   tgt[2] =   8*x       - 6*z
void uint64_convolve_with_B4(uint64_t *src, uint64_t *tgt) {
  uint64_t x = src[0];
  uint64_t y = src[1];
  uint64_t z = src[2];

  uint64_t x8 = x << 3;
  uint64_t y8 = y << 3;
  uint64_t z8 = z << 3;

  uint64_t x6 = x8 - (x + x);
  uint64_t y6 = y8 - (y + y);
  uint64_t z6 = z8 - (z + z);

  tgt[0] = - x6 + y8;
  tgt[1] = - y6 + z8;
  tgt[2] = - z6 + x8;
}

// convolves with:  b5 = {   2 , -4 , -24 };
//   tgt[0] =   2*x - 24*y -  4*z 
//   tgt[1] =  -4*x +  2*y - 24*z
//   tgt[2] = -24*x -  4*y +  2*z
void uint64_convolve_with_B5(uint64_t *src, uint64_t *tgt) {
  uint64_t x = src[0];
  uint64_t y = src[1];
  uint64_t z = src[2];

  uint64_t x2 = x << 1;
  uint64_t y2 = y << 1;
  uint64_t z2 = z << 1;

  uint64_t x4 = x2 << 1;
  uint64_t y4 = y2 << 1;
  uint64_t z4 = z2 << 1;

  uint64_t x24 = x4*6; // (x4 + x4 + x4) << 1;
  uint64_t y24 = y4*6; // (y4 + y4 + y4) << 1;
  uint64_t z24 = z4*6; // (z4 + z4 + z4) << 1;

  tgt[0] =   x2  - y24 - z4 ;
  tgt[1] = - x4  + y2  - z24;
  tgt[2] = - x24 - y4  + z2 ;
}

// convolves with:  b6 = {  -2 , -2 ,  -8 };
//   tgt[0] = - ( 2*x + 8*y + 2*z ) 
//   tgt[1] = - ( 2*x + 2*y + 8*z )
//   tgt[2] = - ( 8*x + 2*y + 2*z )
void uint64_convolve_with_B6(uint64_t *src, uint64_t *tgt) {
  uint64_t x = src[0];
  uint64_t y = src[1];
  uint64_t z = src[2];

  uint64_t x3 = (x << 2) - x ;
  uint64_t y3 = (y << 2) - y ;
  uint64_t z3 = (z << 2) - z ;

  uint64_t s = x + y + z;

  tgt[0] = - ( (s + y3) << 1 );
  tgt[1] = - ( (s + z3) << 1 );
  tgt[2] = - ( (s + x3) << 1 );
}

//------------------------------------------------------------------------------

void uint64_naive_circular_conv( int n, uint64_t *input, uint64_t *coeffs, uint64_t *output ) {
  for(int k=0; k<n; k++) {
    uint64_t acc = 0;
    for(int j=0; j<n; j++) {
      acc += input[j] * coeffs[ (k+n-j)%n ];
    }
    output[k] = acc;
  }  
}

//------------------------------------------------------------------------------

void uint64_add_vec3(uint64_t *xs, uint64_t *ys, uint64_t *zs) {
  for(int i=0; i<3; i++) zs[i] = xs[i] + ys[i];  
}

void uint64_sub_vec3(uint64_t *xs, uint64_t *ys, uint64_t *zs) {
  for(int i=0; i<3; i++) zs[i] = xs[i] - ys[i];  
}

//------------------------------------------------------------------------------

// cyclic convolution of 12 terms via the Agarwal-Cooley algorithm
// with the fixed vector [7,8,21,22,6,7,9,10,13,26,8,23]
//
void uint64_circular_conv_12_with( uint64_t *input , uint64_t *output ) {

  uint64_t input_rect[4][3];       // first index is the outer, second the inner

  for(int k=0; k<12; k++) {
    input_rect[k%4][k%3] = input [k];
  }

  uint64_t *input_ptr = (uint64_t*) input_rect;

  uint64_t *x0 = input_ptr    ;
  uint64_t *x1 = input_ptr + 3;
  uint64_t *x2 = input_ptr + 6;
  uint64_t *x3 = input_ptr + 9;

  uint64_t a0[3], a1[3], a2[3], a3[3], a4[3], a5[3], a6[3];
  for(int j=0; j<3; j++)  {
    a0[j] = x0[j] + x2[j]; 
    a1[j] = x1[j] + x3[j]; 
    a2[j] = a0[j] + a1[j]; 
    a3[j] = a0[j] - a1[j]; 
    a4[j] = x0[j] - x2[j]; 
    a5[j] = x1[j] - x3[j]; 
    a6[j] = a4[j] + a5[j]; 
  }

  uint64_t m0[3], m1[3], m2[3], m3[3], m4[3];
  uint64_convolve_with_B2( a2 , m0 );            // uint64_naive_circular_conv( 3 , a2 , b2 , m0 );
  uint64_convolve_with_B3( a3 , m1 );            // uint64_naive_circular_conv( 3 , a3 , b3 , m1 );
  uint64_convolve_with_B4( a4 , m2 );            // uint64_naive_circular_conv( 3 , a4 , b4 , m2 );
  uint64_convolve_with_B5( a5 , m3 );            // uint64_naive_circular_conv( 3 , a5 , b5 , m3 );
  uint64_convolve_with_B6( a6 , m4 );            // uint64_naive_circular_conv( 3 , a6 , b6 , m4 );

  uint64_t u0[3], u1[3], u2[3], u3[3];
  uint64_add_vec3( m0 , m1 , u0 );
  uint64_sub_vec3( m0 , m1 , u1 );
  uint64_sub_vec3( m4 , m3 , u2 );
  uint64_sub_vec3( m4 , m2 , u3 );

  for(int i=0; i<3; i++) {
    x0[i] = ( u0[i] + 2*u2[i] ) >> 2;
    x1[i] = ( u1[i] + 2*u3[i] ) >> 2;
    x2[i] = ( u0[i] - 2*u2[i] ) >> 2;
    x3[i] = ( u1[i] - 2*u3[i] ) >> 2;
  }
 
  for(int k=0; k<12; k++) {
    output[k] = input_rect[k%4][k%3];
  }
}

//------------------------------------------------------------------------------

/*

void uint64_test_short_conv_with() {
  
  printf("test short convolution algos for uint64\n");
  
  uint64_t input    [12];
  uint64_t coeffs   [12] = {7,8,21,22,6,7,9,10,13,26,8,23};
  uint64_t output   [12];
  uint64_t reference[12];

  // generate some "random-looking" numbers
  uint64_t a=123459;
  uint64_t b=789013;
  for(int i=0;i<12;i++) {
    uint64_t c = (a*b) ^ (a - 12345);
    uint64_t d = (c*a) ^ (b + 67891);
    input [i] = c & 0x0fffffff;             // WE WANT NO OVERFLOW!
    a = b   + c       + 1;
    b = 3*a - 5*c + d - 3;
  }

  for(int i=0; i<12; i++) {
    printf("x[%d] = %016llx  ;  h[%d] = %016llx\n" , i, input[i], i, coeffs[i] );
  }

  // -----------[ length = 12 ]----------- 

  printf("\n");
  printf("length = 12\n");

  uint64_naive_circular_conv   ( 12, input, coeffs, reference );
  uint64_circular_conv_12_with (     input,         output    );

  for(int i=0; i<12; i++) {
    printf("out[%d] = %016llx  ;  ref[%d] = %016llx\n" , i, output[i], i, reference[i] );
  }
}

*/

//------------------------------------------------------------------------------

