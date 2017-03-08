#include "Riostream.h"

void plot_event(const char* filename = "vishnu_profile.csv", const char* outputfile = "vishnu_profile.root") {
    // simple reader to covert the contents of the output file
    // into histograms
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
   TH2D* jewel[15];
   TH2D* hydro[15];
   TH2D* parton[15];

   for(int i = 0; i < 15; i++) {
      jewel[i] = new TH2D(Form("neff_jewel_tau=%i", i), Form("N_{eff, jewel}, #tau = %i (fm/#it{c})", i), 101, -10.1, 10.1, 101, -10.1, 10.1);
      hydro[i] = new TH2D(Form("neff_hydro_tau=%i", i), Form("N_{eff, hydro}, #tau = %i (fm/#it{c})", i), 101, -10.1, 10.1, 101, -10.1, 10.1);
      parton[i] = new TH2D(Form("parton_tau=%i", i), Form("parton, #tau = %i (fm/#it{c})", i), 202, -10.1, 10.1, 202, -10.1, 10.1);


      jewel[i]->GetXaxis()->SetTitle("x (fm)");
      jewel[i]->GetYaxis()->SetTitle("y (fm)");
      jewel[i]->GetZaxis()->SetRangeUser(0., 1.5);

      hydro[i]->GetXaxis()->SetTitle("x (fm)");
      hydro[i]->GetYaxis()->SetTitle("y (fm)");
      hydro[i]->GetZaxis()->SetRangeUser(0., 1.5);

   }

   // first digest the header 
   char header[256];
   in.getline(header,256);
   printf("\n .. digesting header ..  \n     - %s\n",header);

   printf("\n .. and now for the actual work .. \n");
   while (true) {
      in >> x >> y >> tau >> hydroneff >> jewelneff;
      if (!in.good()) break;
      if (nlines < 5) {
          cout << "    - printing first lines as sanity check" << endl;
          printf("x=%.4f, y=%.4f, z=%.4f, tau=%.4f, hne=%.4f, jne=%.4f  \n",x,y,z,tau,hydroneff,jewelneff);
      }
      if(tau >= 0 && tau < 15) {
          jewel[int(tau)]->Fill(x,y,jewelneff);
          hydro[int(tau)]->Fill(x,y,hydroneff);
      }
      nlines++;
   }

   printf("\n .. digested  %d points -- \n",nlines);

   in.close();

   ifstream in2;
   in2.open("vishnu.cvs");

   // first digest the header 
   char header2[256];
   in2.getline(header2,256);
   printf("\n .. digesting header ..  \n     - %s\n",header);

   printf("\n .. and now for the actual work .. \n");
   while (true) {
      in2 >> x >> y >> z >> tau >> jeweltemp >> hydrotemp >> hydroepsilon >> hydroentropy >> hydroneff >> jewelneff;
      if (!in2.good()) break;
      if (nlines < 5) {
          cout << "    - printing first lines as sanity check" << endl;
          printf("x=%.4f, y=%.4f, z=%.4f, tau=%.4f, jt=%.f, ht=%.4f, he=%.4f, hs=%.4f, hne=%.4f, jne=%.4f  \n",x,y,z,tau,jeweltemp,hydrotemp,hydroepsilon,hydroentropy,hydroneff,jewelneff);
      }
      if(tau >= 0 && tau < 15) {
          parton[int(tau)]->Fill(x,y,hydroneff);
      }
   }
   in2.close();

   TCanvas* can = new TCanvas("evolution", "evolution");
   can->Divide(5,3);
   gStyle->SetOptStat(0);
   for(int i = 1; i < 6; i++) {
       can->cd(i);
       gPad->SetLogz();
       gStyle->SetPalette(62);
       jewel[2*i-1]->DrawCopy("colz");
       can->cd(i+5);
       gPad->SetLogz();
       hydro[2*i-1]->DrawCopy("colz");
       can->cd(i+10);
       gPad->SetLogz();
       hydro[3*i-1]->DrawCopy("colz");
       parton[3*i-1]->DrawCopy("colz same");


   }

   f->Write();
   printf("\n -- %s has been closed, have fun \n\n", outputfile);
}
