# Makefile encript-app
PROJDIR := $(realpath $(CURDIR)/..)
SOURCEDIR := $(PROJDIR)/src
BUILDDIR := $(PROJDIR)/obj
# Name of app
TARGER = myencriptapp
# Decide whether the commands will be shwon or not
VERBOSE = TRUE
# List of sources and objects
SRC = mathtools.f90 msgtools.f90 dcaoserie.f90 encripttools.f90
OBJ = ${SRC:.f90=.o}
# Compiler and other commands
FC=gfortran #fortran compiler
FFLAGS=-O3 -Wall -Wextra -std=f2008
RM=rm -f


# Hide or not the calls depending of VERBOSE
ifeq ($(VERBOSE),TRUE)
    HIDE =  
else
    HIDE = @
endif


all: encriptapp
mathtools.o: mathtools.f90
	$(FC) $(FFLAGS) -c mathtools.f90

msgtools.o: msgtools.f90
	$(FC) $(FFLAGS) -c msgtools.f90

dcaoserie.o: dcaoserie.f90
	$(FC) $(FFLAGS) -c dcaoserie.f90

encripttools.o: encripttools.f90
	$(FC) $(FFLAGS) -c encripttools.f90

encriptapp: $(OBJ)
	$(FC) $(FFLAGS)  $(OBJ) encriptapp.f90 -o myencriptapp

clean:
	$(RM) *.mod *.o all
