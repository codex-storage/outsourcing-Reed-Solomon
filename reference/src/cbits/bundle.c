
// gcc can do better inlineing if everything is in the same file
// this can win like 33% speedup for the naive FFT algorithm...

#include "goldilocks.c"
#include "goldilocks_ext.c"
#include "monolith.c"
#include "ntt.c"
#include "short_dft.c"
