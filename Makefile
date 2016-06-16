all: jewel-2.0.2-vishnu 

# macros
FC := gfortran
LDFLAGS= -lm -fno-align-commons -lz -lrt -ldl -lm
FFLAGS = -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include -fno-align-commons

# libraries and include locations
CFLAGS = -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include
INCLUDEDIR = -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include
LIBRARYDIR = -L/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib
LHAPDF_PATH := /home/rbertens/Documents/CERN/alice/JEWEL/LHAPDF/inst/lib
LIBRARIES = /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5hl_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_hl.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5.a

jewel-2.0.2-vishnu: jewel-2.0.2.o medium-vishnu.o Jetoutputh5.o pythia6425mod.o meix.o 
	$(FC) -o $@ $^ $(LIBRARYDIR) -L$(LHAPDF_PATH) $(INCLUDEDIR) -lLHAPDF $(LIBRARIES) $(LDFLAGS)

#vishnu-test: vishnu-test.o medium-vishnu.o Jetoutputh5.o pythia6425mod.o meix.o 
#	$(FC) -o $@ $^ $(LIBRARYDIR) -L$(LHAPDF_PATH) $(INCLUDEDIR) -lLHAPDF $(LIBRARIES) $(LDFLAGS)


clean:
	rm -f medium-*.o 
	rm -f jewel*.o
	rm -f pythia6425mod.o meix.o
	rm -f *~

#make destroy to clean out working dir for git pushes
destroy:
	rm -f medium-*.o 
	rm -f jewel*.o
	rm -f pythia6425mod.o meix.o Jetoutputh5.o
	rm -f jewel-2.0.2-simple jewel-2.0.2-vac jewel-2.0.2-vishnu
	rm -f *~

.PHONY: all
