#include "Riostream.h"

void vishnu_reader() {
    // simple reader to covert the contents of the output file
    // to a ttree
   TString dir = gSystem->UnixPathName(gInterpreter->GetCurrentMacroName());
   dir.ReplaceAll("vishnu_reader.C","");
   dir.ReplaceAll("/./","/");
   ifstream in;
   in.open(Form("%sbasic.dat",dir.Data()));

   Float_t x,y,z;
   Int_t nlines = 0;
   TFile *f = new TFile("basic.root","RECREATE");
   TNtuple *ntuple = new TNtuple("ntuple","data from ascii file","x:y:z");

   while (1) {
      in >> x >> y >> z;
      if (!in.good()) break;
      cout << " digesting info in the form of" << endl;
      if (nlines < 5) printf("x=%8f, y=%8f, z=%8f\n",x,y,z);
      ntuple->Fill(x,y,z);
      nlines++;
   }
   printf(" -- digested  %d points -- \n",nlines);

   in.close();

   f->Write();
}
