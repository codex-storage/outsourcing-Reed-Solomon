
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include <time.h>

//#include "goldilocks.h"
//#include "ntt.h"
#include "bundle.h"

//------------------------------------------------------------------------------

const int LOGN    = 20;
const int N       = (1<<LOGN);
const int NREPEAT = 20;

const uint64_t generator_size_32 = 0x185629dcda58878cULL;

//------------------------------------------------------------------------------

void print_time(const char *prefix, clock_t start, clock_t end) {
  double cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  double msec_per_megabyte = 1000 * cpu_time_used * 1024 * 1024 / (8 * N * NREPEAT );
  double megabyte_per_sec  = 1000 / msec_per_megabyte;
  printf("repeating %d %s of size 2^%d took %0.4f seconds (%0.0f MB/sec)\n", NREPEAT, prefix, LOGN, cpu_time_used, megabyte_per_sec);
}

int main() {

  clock_t start, end;
 
  uint64_t *src = (uint64_t*) malloc( 8 * N );     
  uint64_t *tgt = (uint64_t*) malloc( 8 * N );  

  for(int i=0; i<N; i++) { 
    src[i] = 101 + 7*i;
  }   

  uint64_t gen = goldilocks_pow( generator_size_32 , 1<<(32-LOGN) );

  start = clock();
  for(int k=0; k<(NREPEAT/2); k++) { 
    goldilocks_ntt_forward( LOGN, gen, src, tgt );
    goldilocks_ntt_forward( LOGN, gen, tgt, src );
  }
  end = clock();
  print_time("forward NTT", start, end);

  start = clock();
  for(int k=0; k<(NREPEAT/2); k++) { 
    goldilocks_ntt_inverse( LOGN, gen, src, tgt );
    goldilocks_ntt_inverse( LOGN, gen, tgt, src );
  }
  end = clock();
  print_time("inverse NTT", start, end);

  return 0;
}
