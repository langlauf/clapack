####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 2.0                                             #
#  September 30, 1994                                                 #
####################################################################
#
#  The machine (platform) identifier to append to the library names
#
PLAT = _alpha
#  
#  Modify the CC and CFLAGS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
#######################################################
# This is used to compile C libary
CC        = cc
CFLAGS    = -O2
LOADER    = cc
LOADOPTS  =
#######################################################################
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB      = ../../blas$(PLAT).a
#BLASLIB     = -lblas
LAPACKLIB    = lapack$(PLAT).a
TMGLIB       = tmglib$(PLAT).a
EIGSRCLIB    = eigsrc$(PLAT).a
F2CLIB       = ../../F2CLIBS/libF77.a ../../F2CLIBS/libI77.a
