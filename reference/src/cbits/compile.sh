#!/bin/bash

gcc -c -O2 goldilocks.c
gcc -c -O2 goldilocks_ext.c
gcc -c -O2 monolith.c
gcc -c -O2 ntt.c
gcc -c -O2 short_dft.c
gcc -c -O2 bundle.c

gcc -O2 bench_fft.c bundle.c
