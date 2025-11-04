
//
// Short DFT algoritmus (size 8 and 16)
// 
// See: 
//
// - Nussbaumer: "Fast Fourier Transform and Convolution Algorithms", Chapter 5.5
//
// Note: As they describe complex DFT and we need NTT, the conventions differ a bit.
//
// Hence some formulas in the comments look false, that's because we follow the 
// formulas from the book but need to change some coefficients and/or ordering...
//
// Note #2: If the multiplicative generator is changed, the constants need
// to be regenerated. See the module "NTT.FTT.Short"
//

#include <stdint.h>

#include "goldilocks.h"
#include "short_dft.h"

//------------------------------------------------------------------------------
// SIZE = 4

const uint64_t DFT4_J              = 0x0001000000000000 ;

const uint64_t IDFT4_OMEGA         = 0x0001000000000000 ;
const uint64_t IDFT4_INV_OMEGA     = 0xfffeffff00000001 ;
const uint64_t IDFT4_J             = 0x0001000000000000 ;
const uint64_t IDFT4_INV_4         = 0xbfffffff40000001 ;

void short_inv_DFT_size_4_unscaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  int src_stride2 = src_stride  + src_stride ;
  int src_stride3 = src_stride2 + src_stride ;

  uint64_t x0 = src[          0];
  uint64_t x1 = src[src_stride ];
  uint64_t x2 = src[src_stride2];
  uint64_t x3 = src[src_stride3];

  uint64_t t1 = goldilocks_add( x0 , x2 );                              //  x0 + x2
  uint64_t t2 = goldilocks_add( x1 , x3 );                              //  x1 + x3
  uint64_t m0 = goldilocks_add( t1 , t2 );                              //  t1 + t2
  uint64_t m1 = goldilocks_sub( t1 , t2 );                              //  t1 - t2
  uint64_t m2 = goldilocks_sub( x0 , x2 );                              //  x0 - x2
  uint64_t m3 = goldilocks_mul( DFT4_J , goldilocks_sub( x3 , x1 ) );   //  j * (x3 - x1)

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;

  tgt[          0] = m0;
  tgt[tgt_stride ] = goldilocks_add( m2 , m3 );
  tgt[tgt_stride2] = m1;
  tgt[tgt_stride3] = goldilocks_sub( m2 , m3 );

}

void short_fwd_DFT_size_4( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {
  short_inv_DFT_size_4_unscaled( src_stride, tgt_stride, src, tgt );

  int tgt_stride3 = 3*tgt_stride;

  uint64_t tmp     = tgt[tgt_stride ];
  tgt[tgt_stride ] = tgt[tgt_stride3];
  tgt[tgt_stride3] = tmp;
}

void short_inv_DFT_size_4_rescaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  short_inv_DFT_size_4_unscaled( src_stride, tgt_stride, src, tgt );

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;

  tgt[          0] = goldilocks_mul( IDFT4_INV_4 , tgt[          0] );
  tgt[tgt_stride ] = goldilocks_mul( IDFT4_INV_4 , tgt[tgt_stride ] );
  tgt[tgt_stride2] = goldilocks_mul( IDFT4_INV_4 , tgt[tgt_stride2] );
  tgt[tgt_stride3] = goldilocks_mul( IDFT4_INV_4 , tgt[tgt_stride3] );
}

//------------------------------------------------------------------------------
// SIZE = 8

const uint64_t DFT8_OMEGA         = 0xfffffffeff000001 ;
const uint64_t DFT8_INV_OMEGA     = 0x000000ffffffff00 ;
const uint64_t DFT8_J             = 0x0001000000000000 ;
const uint64_t DFT8_COS_U         = 0xffffff7f00800081 ;
const uint64_t DFT8_MINUS_J_SIN_U = 0xffffff7eff800081 ;

void short_fwd_DFT_size_8( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {
  //  u     = 2pi/8
  //  omega = cos(u) + i*sin(u)
  // 
  //   cos_u    ~>  - (omega+omega^7) / 2
  //   j_sin_u  ~>  - (omega-omega^7) / 2
  //  -j_sin_u  ~>  + (omega-omega^7) / 2
  //   j        ~>     omega^2

  int src_stride2 = src_stride  + src_stride ;
  int src_stride3 = src_stride2 + src_stride ;
  int src_stride4 = src_stride2 + src_stride2;
  int src_stride5 = src_stride4 + src_stride ;
  int src_stride6 = src_stride4 + src_stride2;
  int src_stride7 = src_stride4 + src_stride3;

  uint64_t x0 = src[0          ];
  uint64_t x1 = src[src_stride ];
  uint64_t x2 = src[src_stride2];
  uint64_t x3 = src[src_stride3];
  uint64_t x4 = src[src_stride4];
  uint64_t x5 = src[src_stride5];
  uint64_t x6 = src[src_stride6];
  uint64_t x7 = src[src_stride7];
 
  uint64_t t1 = goldilocks_add( x0 , x4 );      //  x0 + x4
  uint64_t t2 = goldilocks_add( x2 , x6 );      //  x2 + x6
  uint64_t t3 = goldilocks_add( x1 , x5 );      //  x1 + x5
  uint64_t t4 = goldilocks_sub( x1 , x5 );      //  x1 - x5
  uint64_t t5 = goldilocks_add( x3 , x7 );      //  x3 + x7
  uint64_t t6 = goldilocks_sub( x3 , x7 );      //  x3 - x7
  uint64_t t7 = goldilocks_add( t1 , t2 );      //  t1 + t2
  uint64_t t8 = goldilocks_add( t3 , t5 );      //  t3 + t5
    
  uint64_t m0 = goldilocks_add( t7 , t8 );      //  (t7 + t8)             
  uint64_t m1 = goldilocks_sub( t7 , t8 );      //  (t7 - t8)             
  uint64_t m2 = goldilocks_sub( t1 , t2 );      //  (t1 - t2)             
  uint64_t m3 = goldilocks_sub( x0 , x4 );      //  (x0 - x4)            

  uint64_t m4 = goldilocks_mul( DFT8_COS_U         , goldilocks_sub( t4 , t6 ) );     //  cos_u * (t4 - t6)    
  uint64_t m5 = goldilocks_mul( DFT8_J             , goldilocks_sub( t5 , t3 ) );     //  j*(t5 - t3)          
  uint64_t m6 = goldilocks_mul( DFT8_J             , goldilocks_sub( x6 , x2 ) );     //  j*(x6 - x2)          
  uint64_t m7 = goldilocks_mul( DFT8_MINUS_J_SIN_U , goldilocks_add( t4 , t6 ) );     //  - j_sin_u * (t4 + t6)
    
  uint64_t s1 = goldilocks_add( m3 , m4 );      //  m3 + m4
  uint64_t s2 = goldilocks_sub( m3 , m4 );      //  m3 - m4
  uint64_t s3 = goldilocks_add( m6 , m7 );      //  m6 + m7
  uint64_t s4 = goldilocks_sub( m6 , m7 );      //  m6 - m7

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;
        
  tgt[          0] = m0;                               //  m0     
  tgt[tgt_stride ] = goldilocks_sub( s2 , s4 );        //  s2 - s4
  tgt[tgt_stride2] = goldilocks_sub( m2 , m5 );        //  m2 - m5
  tgt[tgt_stride3] = goldilocks_add( s1 , s3 );        //  s1 + s3
  tgt[tgt_stride4] = m1;                               //  m1     
  tgt[tgt_stride5] = goldilocks_sub( s1 , s3 );        //  s1 - s3
  tgt[tgt_stride6] = goldilocks_add( m2 , m5 );        //  m2 + m5
  tgt[tgt_stride7] = goldilocks_add( s2 , s4 );        //  s2 + s4
}

const uint64_t IDFT8_OMEGA         = 0xfffffffeff000001 ;
const uint64_t IDFT8_INV_OMEGA     = 0x000000ffffffff00 ;
const uint64_t IDFT8_J             = 0x0001000000000000 ;
const uint64_t IDFT8_COS_U         = 0x0000007fff7fff80 ;
const uint64_t IDFT8_MINUS_J_SIN_U = 0x00000080007fff80 ;
const uint64_t IDFT8_INV_8         = 0xdfffffff20000001 ;

//--------------------------------------

void short_inv_DFT_size_8_unscaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {
  //  u     = 2pi/8
  //  omega = cos(u) + i*sin(u)
  // 
  //   cos_u    ~>    (omega+omega^7) / 2
  //  -j_sin_u  ~>  - (omega-omega^7) / 2
  //   j        ~>     omega^2

  int src_stride2 = src_stride  + src_stride ;
  int src_stride3 = src_stride2 + src_stride ;
  int src_stride4 = src_stride2 + src_stride2;
  int src_stride5 = src_stride4 + src_stride ;
  int src_stride6 = src_stride4 + src_stride2;
  int src_stride7 = src_stride4 + src_stride3;

  uint64_t x0 = src[0          ];
  uint64_t x1 = src[src_stride ];
  uint64_t x2 = src[src_stride2];
  uint64_t x3 = src[src_stride3];
  uint64_t x4 = src[src_stride4];
  uint64_t x5 = src[src_stride5];
  uint64_t x6 = src[src_stride6];
  uint64_t x7 = src[src_stride7];
 
  uint64_t t1 = goldilocks_add( x0 , x4 );      //  x0 + x4
  uint64_t t2 = goldilocks_add( x2 , x6 );      //  x2 + x6
  uint64_t t3 = goldilocks_add( x1 , x5 );      //  x1 + x5
  uint64_t t4 = goldilocks_sub( x1 , x5 );      //  x1 - x5
  uint64_t t5 = goldilocks_add( x3 , x7 );      //  x3 + x7
  uint64_t t6 = goldilocks_sub( x3 , x7 );      //  x3 - x7
  uint64_t t7 = goldilocks_add( t1 , t2 );      //  t1 + t2
  uint64_t t8 = goldilocks_add( t3 , t5 );      //  t3 + t5
    
  uint64_t m0 = goldilocks_add( t7 , t8 );      //  (t7 + t8)             
  uint64_t m1 = goldilocks_sub( t7 , t8 );      //  (t7 - t8)             
  uint64_t m2 = goldilocks_sub( t1 , t2 );      //  (t1 - t2)             
  uint64_t m3 = goldilocks_sub( x0 , x4 );      //  (x0 - x4)            

  uint64_t m4 = goldilocks_mul( IDFT8_COS_U         , goldilocks_sub( t4 , t6 ) );     //  cos_u * (t4 - t6)    
  uint64_t m5 = goldilocks_mul( IDFT8_J             , goldilocks_sub( t5 , t3 ) );     //  j*(t5 - t3)          
  uint64_t m6 = goldilocks_mul( IDFT8_J             , goldilocks_sub( x6 , x2 ) );     //  j*(x6 - x2)          
  uint64_t m7 = goldilocks_mul( IDFT8_MINUS_J_SIN_U , goldilocks_add( t4 , t6 ) );     //  - j_sin_u * (t4 + t6)
    
  uint64_t s1 = goldilocks_add( m3 , m4 );      //  m3 + m4
  uint64_t s2 = goldilocks_sub( m3 , m4 );      //  m3 - m4
  uint64_t s3 = goldilocks_add( m6 , m7 );      //  m6 + m7
  uint64_t s4 = goldilocks_sub( m6 , m7 );      //  m6 - m7

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;
        
  tgt[          0] = m0                       ;        //  m0     
  tgt[tgt_stride ] = goldilocks_add( s1 , s3 );        //  s1 + s3
  tgt[tgt_stride2] = goldilocks_add( m2 , m5 );        //  m2 + m5
  tgt[tgt_stride3] = goldilocks_sub( s2 , s4 );        //  s2 - s4
  tgt[tgt_stride4] = m1                       ;        //  m1     
  tgt[tgt_stride5] = goldilocks_add( s2 , s4 );        //  s2 + s4
  tgt[tgt_stride6] = goldilocks_sub( m2 , m5 );        //  m2 - m5
  tgt[tgt_stride7] = goldilocks_sub( s1 , s3 );        //  s1 - s3
}

//------------------

void short_inv_DFT_size_8_rescaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  short_inv_DFT_size_8_unscaled( src_stride, tgt_stride, src, tgt );

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;

  tgt[          0] = goldilocks_mul( IDFT8_INV_8 , tgt[          0] );
  tgt[tgt_stride ] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride ] );
  tgt[tgt_stride2] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride2] );
  tgt[tgt_stride3] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride3] );
  tgt[tgt_stride4] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride4] );
  tgt[tgt_stride5] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride5] );
  tgt[tgt_stride6] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride6] );
  tgt[tgt_stride7] = goldilocks_mul( IDFT8_INV_8 , tgt[tgt_stride7] );
}

//------------------------------------------------------------------------------
// SIZE = 16

const uint64_t DFT16_OMEGA          = 0xefffffff00000001 ;
const uint64_t DFT16_INV_OMEGA      = 0x0000001000000000 ;
const uint64_t DFT16_J              = 0x0001000000000000 ;
const uint64_t DFT16_COS_U          = 0xf800000700000001 ;
const uint64_t DFT16_COS_2U         = 0x0000007fff7fff80 ;
const uint64_t DFT16_COS_3U         = 0x0007fffffff7f800 ;
const uint64_t DFT16_MINUS_J_SIN_U  = 0x0800000800000000 ;
const uint64_t DFT16_MINUS_J_SIN_2U = 0x00000080007fff80 ;
const uint64_t DFT16_MINUS_J_SIN_3U = 0xfff7ffff0007f801 ;
const uint64_t DFT16_COS_3U_PLUS_U          = 0xf8080006fff7f801 ;
const uint64_t DFT16_COS_3U_MINUS_U         = 0x0807fff7fff7f800 ;
const uint64_t DFT16_J_SIN_3U_MINUS_U       = 0x08080007fff80800 ;
const uint64_t DFT16_J_SIN_MINUS_3U_MINUS_U = 0x07f800080007f800 ;

void short_fwd_DFT_size_16( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  int src_stride2 = src_stride  + src_stride ;
  int src_stride3 = src_stride2 + src_stride ;
  int src_stride4 = src_stride2 + src_stride2;
  int src_stride5 = src_stride4 + src_stride ;
  int src_stride6 = src_stride4 + src_stride2;
  int src_stride7 = src_stride4 + src_stride3;
  int src_stride8 = src_stride4 + src_stride4;

  uint64_t x0  = src[ 0           ];
  uint64_t x1  = src[ src_stride  ];
  uint64_t x2  = src[ src_stride2 ];
  uint64_t x3  = src[ src_stride3 ];
  uint64_t x4  = src[ src_stride4 ];
  uint64_t x5  = src[ src_stride5 ];
  uint64_t x6  = src[ src_stride6 ];
  uint64_t x7  = src[ src_stride7 ];
  uint64_t x8  = src[               src_stride8 ];
  uint64_t x9  = src[ src_stride  + src_stride8 ];
  uint64_t x10 = src[ src_stride2 + src_stride8 ];
  uint64_t x11 = src[ src_stride3 + src_stride8 ];
  uint64_t x12 = src[ src_stride4 + src_stride8 ];
  uint64_t x13 = src[ src_stride5 + src_stride8 ];
  uint64_t x14 = src[ src_stride6 + src_stride8 ];
  uint64_t x15 = src[ src_stride7 + src_stride8 ];

  uint64_t t1  = goldilocks_add( x0  , x8  );       //  x0  + x8 
  uint64_t t2  = goldilocks_add( x4  , x12 );       //  x4  + x12
  uint64_t t3  = goldilocks_add( x2  , x10 );       //  x2  + x10
  uint64_t t4  = goldilocks_sub( x2  , x10 );       //  x2  - x10
  uint64_t t5  = goldilocks_add( x6  , x14 );       //  x6  + x14
  uint64_t t6  = goldilocks_sub( x6  , x14 );       //  x6  - x14
  uint64_t t7  = goldilocks_add( x1  , x9  );       //  x1  + x9 
  uint64_t t8  = goldilocks_sub( x1  , x9  );       //  x1  - x9 
  uint64_t t9  = goldilocks_add( x3  , x11 );       //  x3  + x11
  uint64_t t10 = goldilocks_sub( x3  , x11 );       //  x3  - x11
  uint64_t t11 = goldilocks_add( x5  , x13 );       //  x5  + x13
  uint64_t t12 = goldilocks_sub( x5  , x13 );       //  x5  - x13
  uint64_t t13 = goldilocks_add( x7  , x15 );       //  x7  + x15
  uint64_t t14 = goldilocks_sub( x7  , x15 );       //  x7  - x15
  uint64_t t15 = goldilocks_add( t1  , t2  );       //  t1  + t2 
  uint64_t t16 = goldilocks_add( t3  , t5  );       //  t3  + t5 
  uint64_t t17 = goldilocks_add( t15 , t16 );       //  t15 + t16
  uint64_t t18 = goldilocks_add( t7  , t11 );       //  t7  + t11
  uint64_t t19 = goldilocks_sub( t7  , t11 );       //  t7  - t11
  uint64_t t20 = goldilocks_add( t9  , t13 );       //  t9  + t13
  uint64_t t21 = goldilocks_sub( t9  , t13 );       //  t9  - t13
  uint64_t t22 = goldilocks_add( t18 , t20 );       //  t18 + t20
  uint64_t t23 = goldilocks_add( t8  , t14 );       //  t8  + t14
  uint64_t t24 = goldilocks_sub( t8  , t14 );       //  t8  - t14
  uint64_t t25 = goldilocks_add( t10 , t12 );       //  t10 + t12
  uint64_t t26 = goldilocks_sub( t12 , t10 );       //  t12 - t10   

  uint64_t m0  = goldilocks_add( t17 , t22 );       // t17 + t22
  uint64_t m1  = goldilocks_sub( t17 , t22 );       // t17 - t22
  uint64_t m2  = goldilocks_sub( t15 , t16 );       // t15 - t16
  uint64_t m3  = goldilocks_sub( t1  , t2  );       // t1  - t2 
  uint64_t m4  = goldilocks_sub( x0  , x8  );       // x0  - x8 

  uint64_t m5  = goldilocks_mul( DFT16_COS_2U , goldilocks_sub(t19 , t21) );
  uint64_t m6  = goldilocks_mul( DFT16_COS_2U , goldilocks_sub(t4  , t6 ) );
  uint64_t m7  = goldilocks_mul( DFT16_COS_3U , goldilocks_add(t24 , t26) );
  uint64_t m8  = goldilocks_mul( DFT16_COS_3U_PLUS_U  , t24 );
  uint64_t m9  = goldilocks_mul( DFT16_COS_3U_MINUS_U , t26 );
  uint64_t m10 = goldilocks_mul( DFT16_J , goldilocks_sub(t20 , t18) );
  uint64_t m11 = goldilocks_mul( DFT16_J , goldilocks_sub(t5  , t3 ) );
  uint64_t m12 = goldilocks_mul( DFT16_J , goldilocks_sub(x12 , x4 ) );
  uint64_t m13 = goldilocks_mul( DFT16_MINUS_J_SIN_2U , goldilocks_add( t19 , t21) );
  uint64_t m14 = goldilocks_mul( DFT16_MINUS_J_SIN_2U , goldilocks_add( t4  , t6 ) );
  uint64_t m15 = goldilocks_mul( DFT16_MINUS_J_SIN_3U , goldilocks_add( t23 , t25) );
  uint64_t m16 = goldilocks_mul( DFT16_J_SIN_3U_MINUS_U       , t23 );
  uint64_t m17 = goldilocks_mul( DFT16_J_SIN_MINUS_3U_MINUS_U , t25 );
  
  uint64_t s1  = goldilocks_add( m3  , m5  );            //  m3  + m5 
  uint64_t s2  = goldilocks_sub( m3  , m5  );            //  m3  - m5 
  uint64_t s3  = goldilocks_add( m11 , m13 );            //  m11 + m13
  uint64_t s4  = goldilocks_sub( m13 , m11 );            //  m13 - m11
  uint64_t s5  = goldilocks_add( m4  , m6  );            //  m4  + m6 
  uint64_t s6  = goldilocks_sub( m4  , m6  );            //  m4  - m6 
  uint64_t s7  = goldilocks_sub( m8  , m7  );            //  m8  - m7 
  uint64_t s8  = goldilocks_sub( m9  , m7  );            //  m9  - m7 
  uint64_t s9  = goldilocks_add( s5  , s7  );            //  s5  + s7 
  uint64_t s10 = goldilocks_sub( s5  , s7  );            //  s5  - s7 
  uint64_t s11 = goldilocks_add( s6  , s8  );            //  s6  + s8 
  uint64_t s12 = goldilocks_sub( s6  , s8  );            //  s6  - s8 
  uint64_t s13 = goldilocks_add( m12 , m14 );            //  m12 + m14
  uint64_t s14 = goldilocks_sub( m12 , m14 );            //  m12 - m14
  uint64_t s15 = goldilocks_add( m15 , m16 );            //  m15 + m16
  uint64_t s16 = goldilocks_sub( m15 , m17 );            //  m15 - m17
  uint64_t s17 = goldilocks_add( s13 , s15 );            //  s13 + s15
  uint64_t s18 = goldilocks_sub( s13 , s15 );            //  s13 - s15
  uint64_t s19 = goldilocks_add( s14 , s16 );            //  s14 + s16
  uint64_t s20 = goldilocks_sub( s14 , s16 );            //  s14 - s16

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;
  int tgt_stride8 = tgt_stride4 + tgt_stride4;

  tgt[ 0                         ] = m0;                                //  m0         
  tgt[ tgt_stride                ] = goldilocks_sub( s9  , s17 );       //  s9  - s17  
  tgt[ tgt_stride2               ] = goldilocks_sub( s1  , s3  );       //  s1  - s3   
  tgt[ tgt_stride3               ] = goldilocks_add( s12 , s20 );       //  s12 + s20  
  tgt[ tgt_stride4               ] = goldilocks_sub( m2  , m10 );       //  m2  - m10  
  tgt[ tgt_stride5               ] = goldilocks_sub( s11 , s19 );       //  s11 - s19  
  tgt[ tgt_stride6               ] = goldilocks_sub( s2  , s4  );       //  s2  - s4   
  tgt[ tgt_stride7               ] = goldilocks_add( s10 , s18 );       //  s10 + s18  
  tgt[               tgt_stride8 ] = m1;                                //  m1         
  tgt[ tgt_stride  + tgt_stride8 ] = goldilocks_sub( s10 , s18 );       //  s10 - s18  
  tgt[ tgt_stride2 + tgt_stride8 ] = goldilocks_add( s2  , s4  );       //  s2  + s4   
  tgt[ tgt_stride3 + tgt_stride8 ] = goldilocks_add( s11 , s19 );       //  s11 + s19  
  tgt[ tgt_stride4 + tgt_stride8 ] = goldilocks_add( m2  , m10 );       //  m2  + m10  
  tgt[ tgt_stride5 + tgt_stride8 ] = goldilocks_sub( s12 , s20 );       //  s12 - s20  
  tgt[ tgt_stride6 + tgt_stride8 ] = goldilocks_add( s1  , s3  );       //  s1  + s3   
  tgt[ tgt_stride7 + tgt_stride8 ] = goldilocks_add( s9  , s17 );       //  s9  + s17  
}

//--------------------------------------

const uint64_t IDFT16_OMEGA          = 0xefffffff00000001 ;
const uint64_t IDFT16_INV_OMEGA      = 0x0000001000000000 ;
const uint64_t IDFT16_INV_16         = 0xefffffff10000001 ;
const uint64_t IDFT16_J              = 0x0001000000000000 ;
const uint64_t IDFT16_COS_U          = 0xf800000700000001 ;
const uint64_t IDFT16_COS_2U         = 0x0000007fff7fff80 ;
const uint64_t IDFT16_COS_3U         = 0x0007fffffff7f800 ;
const uint64_t IDFT16_MINUS_J_SIN_U  = 0x0800000800000000 ;
const uint64_t IDFT16_MINUS_J_SIN_2U = 0x00000080007fff80 ;
const uint64_t IDFT16_MINUS_J_SIN_3U = 0xfff7ffff0007f801 ;

const uint64_t IDFT16_COS_3U_PLUS_U          = 0xf8080006fff7f801 ;
const uint64_t IDFT16_COS_3U_MINUS_U         = 0x0807fff7fff7f800 ;
const uint64_t IDFT16_J_SIN_3U_MINUS_U       = 0x08080007fff80800 ;
const uint64_t IDFT16_J_SIN_MINUS_3U_MINUS_U = 0x07f800080007f800 ;

void short_inv_DFT_size_16_unscaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  int src_stride2 = src_stride  + src_stride ;
  int src_stride3 = src_stride2 + src_stride ;
  int src_stride4 = src_stride2 + src_stride2;
  int src_stride5 = src_stride4 + src_stride ;
  int src_stride6 = src_stride4 + src_stride2;
  int src_stride7 = src_stride4 + src_stride3;
  int src_stride8 = src_stride4 + src_stride4;

  uint64_t x0  = src[ 0           ];
  uint64_t x1  = src[ src_stride  ];
  uint64_t x2  = src[ src_stride2 ];
  uint64_t x3  = src[ src_stride3 ];
  uint64_t x4  = src[ src_stride4 ];
  uint64_t x5  = src[ src_stride5 ];
  uint64_t x6  = src[ src_stride6 ];
  uint64_t x7  = src[ src_stride7 ];
  uint64_t x8  = src[               src_stride8 ];
  uint64_t x9  = src[ src_stride  + src_stride8 ];
  uint64_t x10 = src[ src_stride2 + src_stride8 ];
  uint64_t x11 = src[ src_stride3 + src_stride8 ];
  uint64_t x12 = src[ src_stride4 + src_stride8 ];
  uint64_t x13 = src[ src_stride5 + src_stride8 ];
  uint64_t x14 = src[ src_stride6 + src_stride8 ];
  uint64_t x15 = src[ src_stride7 + src_stride8 ];

  uint64_t t1  = goldilocks_add( x0  , x8  );       //  x0  + x8 
  uint64_t t2  = goldilocks_add( x4  , x12 );       //  x4  + x12
  uint64_t t3  = goldilocks_add( x2  , x10 );       //  x2  + x10
  uint64_t t4  = goldilocks_sub( x2  , x10 );       //  x2  - x10
  uint64_t t5  = goldilocks_add( x6  , x14 );       //  x6  + x14
  uint64_t t6  = goldilocks_sub( x6  , x14 );       //  x6  - x14
  uint64_t t7  = goldilocks_add( x1  , x9  );       //  x1  + x9 
  uint64_t t8  = goldilocks_sub( x1  , x9  );       //  x1  - x9 
  uint64_t t9  = goldilocks_add( x3  , x11 );       //  x3  + x11
  uint64_t t10 = goldilocks_sub( x3  , x11 );       //  x3  - x11
  uint64_t t11 = goldilocks_add( x5  , x13 );       //  x5  + x13
  uint64_t t12 = goldilocks_sub( x5  , x13 );       //  x5  - x13
  uint64_t t13 = goldilocks_add( x7  , x15 );       //  x7  + x15
  uint64_t t14 = goldilocks_sub( x7  , x15 );       //  x7  - x15
  uint64_t t15 = goldilocks_add( t1  , t2  );       //  t1  + t2 
  uint64_t t16 = goldilocks_add( t3  , t5  );       //  t3  + t5 
  uint64_t t17 = goldilocks_add( t15 , t16 );       //  t15 + t16
  uint64_t t18 = goldilocks_add( t7  , t11 );       //  t7  + t11
  uint64_t t19 = goldilocks_sub( t7  , t11 );       //  t7  - t11
  uint64_t t20 = goldilocks_add( t9  , t13 );       //  t9  + t13
  uint64_t t21 = goldilocks_sub( t9  , t13 );       //  t9  - t13
  uint64_t t22 = goldilocks_add( t18 , t20 );       //  t18 + t20
  uint64_t t23 = goldilocks_add( t8  , t14 );       //  t8  + t14
  uint64_t t24 = goldilocks_sub( t8  , t14 );       //  t8  - t14
  uint64_t t25 = goldilocks_add( t10 , t12 );       //  t10 + t12
  uint64_t t26 = goldilocks_sub( t12 , t10 );       //  t12 - t10   

  uint64_t m0  = goldilocks_add( t17 , t22 );       // t17 + t22
  uint64_t m1  = goldilocks_sub( t17 , t22 );       // t17 - t22
  uint64_t m2  = goldilocks_sub( t15 , t16 );       // t15 - t16
  uint64_t m3  = goldilocks_sub( t1  , t2  );       // t1  - t2 
  uint64_t m4  = goldilocks_sub( x0  , x8  );       // x0  - x8 

  uint64_t m5  = goldilocks_mul( IDFT16_COS_2U , goldilocks_sub(t19 , t21) );
  uint64_t m6  = goldilocks_mul( IDFT16_COS_2U , goldilocks_sub(t4  , t6 ) );
  uint64_t m7  = goldilocks_mul( IDFT16_COS_3U , goldilocks_add(t24 , t26) );
  uint64_t m8  = goldilocks_mul( IDFT16_COS_3U_PLUS_U  , t24 );
  uint64_t m9  = goldilocks_mul( IDFT16_COS_3U_MINUS_U , t26 );
  uint64_t m10 = goldilocks_mul( IDFT16_J , goldilocks_sub(t20 , t18) );
  uint64_t m11 = goldilocks_mul( IDFT16_J , goldilocks_sub(t5  , t3 ) );
  uint64_t m12 = goldilocks_mul( IDFT16_J , goldilocks_sub(x12 , x4 ) );
  uint64_t m13 = goldilocks_mul( IDFT16_MINUS_J_SIN_2U , goldilocks_add( t19 , t21) );
  uint64_t m14 = goldilocks_mul( IDFT16_MINUS_J_SIN_2U , goldilocks_add( t4  , t6 ) );
  uint64_t m15 = goldilocks_mul( IDFT16_MINUS_J_SIN_3U , goldilocks_add( t23 , t25) );
  uint64_t m16 = goldilocks_mul( IDFT16_J_SIN_3U_MINUS_U       , t23 );
  uint64_t m17 = goldilocks_mul( IDFT16_J_SIN_MINUS_3U_MINUS_U , t25 );
  
  uint64_t s1  = goldilocks_add( m3  , m5  );            //  m3  + m5 
  uint64_t s2  = goldilocks_sub( m3  , m5  );            //  m3  - m5 
  uint64_t s3  = goldilocks_add( m11 , m13 );            //  m11 + m13
  uint64_t s4  = goldilocks_sub( m13 , m11 );            //  m13 - m11
  uint64_t s5  = goldilocks_add( m4  , m6  );            //  m4  + m6 
  uint64_t s6  = goldilocks_sub( m4  , m6  );            //  m4  - m6 
  uint64_t s7  = goldilocks_sub( m8  , m7  );            //  m8  - m7 
  uint64_t s8  = goldilocks_sub( m9  , m7  );            //  m9  - m7 
  uint64_t s9  = goldilocks_add( s5  , s7  );            //  s5  + s7 
  uint64_t s10 = goldilocks_sub( s5  , s7  );            //  s5  - s7 
  uint64_t s11 = goldilocks_add( s6  , s8  );            //  s6  + s8 
  uint64_t s12 = goldilocks_sub( s6  , s8  );            //  s6  - s8 
  uint64_t s13 = goldilocks_add( m12 , m14 );            //  m12 + m14
  uint64_t s14 = goldilocks_sub( m12 , m14 );            //  m12 - m14
  uint64_t s15 = goldilocks_add( m15 , m16 );            //  m15 + m16
  uint64_t s16 = goldilocks_sub( m15 , m17 );            //  m15 - m17
  uint64_t s17 = goldilocks_add( s13 , s15 );            //  s13 + s15
  uint64_t s18 = goldilocks_sub( s13 , s15 );            //  s13 - s15
  uint64_t s19 = goldilocks_add( s14 , s16 );            //  s14 + s16
  uint64_t s20 = goldilocks_sub( s14 , s16 );            //  s14 - s16

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;
  int tgt_stride8 = tgt_stride4 + tgt_stride4;

  tgt[ 0                         ] = m0;                               // m0       
  tgt[ tgt_stride                ] = goldilocks_add( s9  , s17 );      // s9  + s17
  tgt[ tgt_stride2               ] = goldilocks_add( s1  , s3  );      // s1  + s3 
  tgt[ tgt_stride3               ] = goldilocks_sub( s12 , s20 );      // s12 - s20
  tgt[ tgt_stride4               ] = goldilocks_add( m2  , m10 );      // m2  + m10
  tgt[ tgt_stride5               ] = goldilocks_add( s11 , s19 );      // s11 + s19
  tgt[ tgt_stride6               ] = goldilocks_add( s2  , s4  );      // s2  + s4 
  tgt[ tgt_stride7               ] = goldilocks_sub( s10 , s18 );      // s10 - s18
  tgt[               tgt_stride8 ] = m1;                               // m1       
  tgt[ tgt_stride  + tgt_stride8 ] = goldilocks_add( s10 , s18 );      // s10 + s18
  tgt[ tgt_stride2 + tgt_stride8 ] = goldilocks_sub( s2  , s4  );      // s2  - s4 
  tgt[ tgt_stride3 + tgt_stride8 ] = goldilocks_sub( s11 , s19 );      // s11 - s19
  tgt[ tgt_stride4 + tgt_stride8 ] = goldilocks_sub( m2  , m10 );      // m2  - m10
  tgt[ tgt_stride5 + tgt_stride8 ] = goldilocks_add( s12 , s20 );      // s12 + s20
  tgt[ tgt_stride6 + tgt_stride8 ] = goldilocks_sub( s1  , s3  );      // s1  - s3 
  tgt[ tgt_stride7 + tgt_stride8 ] = goldilocks_sub( s9  , s17 );      // s9  - s17

}

//------------------

void short_inv_DFT_size_16_rescaled( int src_stride, int tgt_stride, uint64_t *src, uint64_t *tgt ) {

  short_inv_DFT_size_16_unscaled( src_stride, tgt_stride, src, tgt );

  int tgt_stride2 = tgt_stride  + tgt_stride ;
  int tgt_stride3 = tgt_stride2 + tgt_stride ;
  int tgt_stride4 = tgt_stride2 + tgt_stride2;
  int tgt_stride5 = tgt_stride4 + tgt_stride ;
  int tgt_stride6 = tgt_stride4 + tgt_stride2;
  int tgt_stride7 = tgt_stride4 + tgt_stride3;
  int tgt_stride8 = tgt_stride4 + tgt_stride4;

  tgt[ 0                         ] = goldilocks_mul( IDFT16_INV_16 , tgt[ 0                         ] );
  tgt[ tgt_stride                ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride                ] );
  tgt[ tgt_stride2               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride2               ] );
  tgt[ tgt_stride3               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride3               ] );
  tgt[ tgt_stride4               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride4               ] );
  tgt[ tgt_stride5               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride5               ] );
  tgt[ tgt_stride6               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride6               ] );
  tgt[ tgt_stride7               ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride7               ] );
  tgt[               tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[               tgt_stride8 ] );
  tgt[ tgt_stride  + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride  + tgt_stride8 ] );
  tgt[ tgt_stride2 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride2 + tgt_stride8 ] );
  tgt[ tgt_stride3 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride3 + tgt_stride8 ] );
  tgt[ tgt_stride4 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride4 + tgt_stride8 ] );
  tgt[ tgt_stride5 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride5 + tgt_stride8 ] );
  tgt[ tgt_stride6 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride6 + tgt_stride8 ] );
  tgt[ tgt_stride7 + tgt_stride8 ] = goldilocks_mul( IDFT16_INV_16 , tgt[ tgt_stride7 + tgt_stride8 ] );

}

//------------------------------------------------------------------------------
