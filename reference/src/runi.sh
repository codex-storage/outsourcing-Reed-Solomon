#!/bin/bash

ghci testMain.hs cbits/goldilocks.o cbits/goldilocks_ext.o cbits/monolith.o cbits/ntt.o cbits/short_dft.o
