# A very basic makefile with just the test target.

F90 = g95

default: fexp.o ; $(F90) -c regex.f90

test: fexp.o test.o regex_test.o
	$(F90) fexp.o test.o -o test
	$(F90) fexp.o regex_test.o -o regex_test

clean:
	rm *.mod *.o test regex_test

fexp.o : fexp.f90 ; $(F90) -c fexp.f90
test.o : test.f90 ; $(F90) -c test.f90
regex_test.o : regex_test.f90 ; $(F90) -c regex_test.f90



