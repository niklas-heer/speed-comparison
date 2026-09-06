# Standalone alternatives

## Fortran with OpenMP

[leibniz_openmp.f90](leibniz_openmp.f90) adapts François's submitted OpenMP
implementation. It uses four accumulators and an OpenMP array reduction to
distribute the Leibniz terms across threads. This is an educational example
outside the ranked benchmark, whose rules require single-threaded execution.

From the repository root, with GNU Fortran and its OpenMP runtime installed:

```sh
gfortran -fopenmp -Ofast -march=native -funroll-loops -flto \
  src/alt/leibniz_openmp.f90 -o /tmp/leibniz_openmp
cd src
OMP_NUM_THREADS=4 OMP_DYNAMIC=FALSE /tmp/leibniz_openmp
```

The program reads `rounds.txt` from its working directory and prints only the
computed number. It starts with the constant term 1 and adds exactly `rounds`
alternating terms. Compared with the submission, this adaptation handles the
remaining one to three terms after complete groups of four, uses 64-bit
integers for loop indices and denominators, checks input errors, and corrects
the compiler command's source filename.

François reported about an 8× speedup on his machine; that result has not been
independently reproduced here. Any timing comparison should record the CPU,
compiler version, flags, input size, and thread count. Running this same binary
with `OMP_NUM_THREADS=1` and then a larger count helps distinguish thread scaling
from changes to the summation and compiler optimization. Reduction order and
relaxed floating-point optimizations can change the last digits of the result.

To check both debug and optimized builds with 1, 2, and 4 threads, run
`python3 src/alt/check_openmp.py` from the repository root. This requires
`gfortran` and checks short/odd inputs, invalid input, and billion-round
convergence. These checks validate correctness, not the reported speedup.
