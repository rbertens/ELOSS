#include "Riostream.h"

void vishnu_reader(const char* filename = "vishnu.cvs", const char* outputfile = "vishnu.root") {
    // simple reader to covert the contents of the output file
    // to a ttree
   ifstream in;
   in.open(filename);
   printf("\n .. attempting to open %s for reading .. \n" ,filename);
   if(!in.good()) {
       printf(" something went wrong, cannot open %s ! \n", filename);
       return;
   } else printf("    - file seems OK \n");

   // initialize some variables
   Double_t x,y,z,tau,jeweltemp,hydrotemp,hydroepsilon,hydroentropy,hydroneff,jewelneff;
   Int_t nlines = 0;

   // open a file for writing the output tuple
   TFile *f = new TFile(outputfile,"RECREATE");
   if(!f->IsZombie()) printf("    - opened %s for writing \n", outputfile);

   // aaaaaaand create the tuple
   TNtupleD *ntuple = new TNtupleD("ntuple","data from ascii file","x:y:z:tau:jeweltemp:hydrotemp:hydroepsilon:hydroentropy:hydroneff:jewelneff");

   // first digest the header 
   char header[256];
   in.getline(header,256);
   printf("\n .. digesting header ..  \n     - %s\n",header);

   printf("\n .. and now for the actual work .. \n");
   while (true) {
      in >> x >> y >> z >> tau >> jeweltemp >> hydrotemp >> hydroepsilon >> hydroentropy >> hydroneff >> jewelneff;
      if (!in.good()) break;
      if (nlines < 5) {
          cout << "    - printing first lines as sanity check" << endl;
          printf("x=%.4f, y=%.4f, z=%.4f, tau=%.4f, jt=%.f, ht=%.4f, he=%.4f, hs=%.4f, hne=%.4f, jne=%.4f  \n",x,y,z,tau,jeweltemp,hydrotemp,hydroepsilon,hydroentropy,hydroneff,jewelneff);
      }
      ntuple->Fill(x,y,z,tau,jeweltemp,hydrotemp,hydroepsilon,hydroentropy,hydroneff,jewelneff);
      nlines++;
   }
   printf("\n .. digested  %d points -- \n",nlines);

   in.close();

   f->Write();
   printf("\n -- %s has been closed, have fun \n\n", outputfile);
}
