all: jewel-2.0.2-vac jewel-2.0.2-simple jewel-2.0.2-vishnu

# path to LHAPDF library
LHAPDF_PATH := /home/rbertens/Documents/CERN/alice/JEWEL/LHAPDF/inst/lib

FC := gfortran
FFLAGS := -g -static

# for vishnu
LDFLAGS= -lm -lz -fno-align-commons
LIBSHDF= -I/home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5hl_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_hl.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5_fortran.a /home/rbertens/Documents/CERN/alice/JEWEL/CHUN_SHEN/again/hdf5-1.8.17/hdf5/lib/libhdf5.a
# end for vishu

jewel-2.0.2-vac: jewel-2.0.2.o medium-vac.o pythia6425mod.o meix.o
	$(FC) -o $@ -L$(LHAPDF_PATH) $^ -lLHAPDF

jewel-2.0.2-simple: jewel-2.0.2.o medium-simple.o pythia6425mod.o meix.o
	$(FC) -o $@ -L$(LHAPDF_PATH) $^ -lLHAPDF

jewel-2.0.2-vishnu: jewel-2.0.2.o Jetoutputh5.o medium-vishnu.o pythia6425mod.o meix.o
	$(FC) -o $@ -L$(LHAPDF_PATH) $^ -lLHAPDF $(LIBSHDF) $(LDFLAGS)

clean:
	rm -f medium-*.o 
	rm -f jewel*.o
	rm -f pythia6425mod.o meix.o
	rm -f *~

.PHONY: all
