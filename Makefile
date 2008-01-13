# A very basic makefile with just the test target.

F90 = g95

default: fexp.o ; $(F90) -c regex.f90

test: fexp.o test.o ; $(F90) fexp.o test.o -o test

fexp.o : fexp.f90 ; $(F90) -c fexp.f90
test.o : test.f90 ; $(F90) -c test.f90



