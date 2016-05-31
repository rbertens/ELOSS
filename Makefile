all: jewel-2.0.2-vac jewel-2.0.2-simple jewel-2.0.2-vishnu

# path to LHAPDF library
LHAPDF_PATH := /home/rbertens/Documents/CERN/alice/JEWEL/LHAPDF/inst/lib

FC := gfortran

# for vishnu
LDFLAGS= -lm -lz -fno-align-commons -lz -lrt -ldl -lm

CFLAGS = -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include
INCLUDEDIR = -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include
LIBRARYDIR = -L/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib

LIBRARIES = /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5hl_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_hl.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5.a

jewel-2.0.2-vac: jewel-2.0.2.o medium-vac.o pythia6425mod.o meix.o
	$(FC) -o $@ -L$(LHAPDF_PATH) $^ -lLHAPDF

jewel-2.0.2-simple: jewel-2.0.2.o medium-simple.o pythia6425mod.o meix.o
	$(FC) -o $@ -L$(LHAPDF_PATH) $^ -lLHAPDF

medium-vishnu.o: medium-vishnu.f 
	gfortran -c -o medium-vishnu.o medium-vishnu.f -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/include/

jewel-2.0.2-vishnu: jewel-2.0.2.o medium-vishnu.o Jetoutputh5.o pythia6425mod.o meix.o 
	$(FC) -o $@ $^ $(LIBRARYDIR) -L$(LHAPDF_PATH) $(INCLUDEDIR) -lLHAPDF $(LIBRARIES) $(LDFLAGS)

clean:
	rm -f medium-*.o 
	rm -f jewel*.o
	rm -f pythia6425mod.o meix.o
	rm -f *~

destroy:
	rm -f medium-*.o 
	rm -f jewel*.o
	rm -f pythia6425mod.o meix.o Jetoutputh5.o
	rm -f jewel-2.0.2-simple jewel-2.0.2-vac jewel-2.0.2-vishnu
	rm -f *~

.PHONY: all
