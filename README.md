# H5Fortran - wbd's experimental version 

An experimental version of h5fortran I developed for my own person use. If you
are looking for Michael Hirsch's original version you should go
[here](https://github.com/geospace-code/h5fortran).

Note, building this (using fpm) currently requires that you use a an
experimental version of fpm which allows you to set the compiler's incude and
library paths using the FPM_INCLUDE_PATH and FPM_LIBRARY_PATH environment
variables. 

Simple, robust, thin HDF5 polymorphic Fortran read/write interface.  Reading or
writing {real64,real32,int32,int64} from scalar to 7d is as simple as

