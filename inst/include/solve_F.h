#ifndef SOLVE_F_H
#define SOLVE_F_H

#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <vector>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"
#include "helpers.h"

// solves for F given actual or prescribed catches  

// TODO add maxF from OM

struct SolveFResult {
  std::vector<double> ApicalF;                    // fleet
  std::vector<std::vector<double>> FDeadAtAge;    // age, fleet
  std::vector<std::vector<double>> FRetainAtAge;  // age, fleet
};


// TODO - add wrapper for R export

SolveFResult SolveForF(
    const std::vector<double>& NumberAtAge,       // nAge
    const std::vector<double>& TotalRemovalsFleet,// nFleet
    const std::vector<std::vector<double>>& SelectivityAtAge, // nAge x nFleet
    const std::vector<std::vector<double>>& RetentionAtAge,   // nAge x nFleet
    const std::vector<std::vector<double>>& DiscardMortAtAge, // nAge x nFleet
    const std::vector<double>& FleetWeightAtAge,   // nAge*nFleet
    const std::vector<double>& NaturalMortalityAtAge, // nAge
    double maxF =5.0,
    
    int MaxIt=500,
    double tolF=1E-6,
    int debug=0) {
  
  
  const int nAge = NumberAtAge.size();
  const int nFleet = TotalRemovalsFleet.size();
  
  SolveFResult res;
  
  res.ApicalF.resize(nFleet, 0.0);
  res.FDeadAtAge.assign(nAge, std::vector<double>(nFleet,0.0));
  res.FRetainAtAge.assign(nAge, std::vector<double>(nFleet,0.0));
  
  // calc biomass at age for each fleet (using fleet-specific weight-at-age)
  std::vector<double> BiomassAtAge(nAge*nFleet,0.0);
  std::vector<double> totalFleetBiomass(nFleet,0.0);
  for(int fl=0; fl<nFleet; ++fl){
    double sumB = 0.0;
    for(int age=0; age<nAge; ++age){
      double B = NumberAtAge[age] * FleetWeightAtAge[age*nFleet + fl];
      BiomassAtAge[age*nFleet + fl] = B;
      sumB += B;
    }
    totalFleetBiomass[fl] = sumB;
    
    // initial guess 
    res.ApicalF[fl] = (sumB > 0.0) ? TotalRemovalsFleet[fl] / sumB : 0.01;
    if(res.ApicalF[fl] < 1e-5) res.ApicalF[fl] = 1e-5;
  } 
  
  // optimization loop
  for(int iter=0; iter<MaxIt; ++iter){
    
    // F-at-age
    for(int age=0; age<nAge; ++age){
      for(int fl=0; fl<nFleet; ++fl){
        double Fint = res.ApicalF[fl] * SelectivityAtAge[age][fl];
        double Fret = Fint * RetentionAtAge[age][fl];
        double Fdisc = Fint - Fret;
        double Fdead = Fret + Fdisc * DiscardMortAtAge[age][fl];
        
        res.FDeadAtAge[age][fl] = Fdead;
        res.FRetainAtAge[age][fl] = Fret;
      }
    }
    
    // predicted removals and derivative
    std::vector<double> predRemovals(nFleet,0.0);
    std::vector<double> dct(nFleet,0.0);
    
    for(int fl=0; fl<nFleet; ++fl){
      double sumRem = 0.0;
      double sumDeriv = 0.0;
      for(int age=0; age<nAge; ++age){
        
        // --- sum Z over all fleets ---
        double Z = NaturalMortalityAtAge[age];
        for(int f2=0; f2<nFleet; ++f2) Z += res.FDeadAtAge[age][f2];
        double Fdead_fl = res.FDeadAtAge[age][fl];
        
        double catchFrac = (Fdead_fl / Z) * (1.0 - std::exp(-Z));
        sumRem += catchFrac * BiomassAtAge[age*nFleet + fl];
        
        // derivative dC/dF
        double tmp = (BiomassAtAge[age*nFleet + fl] / Z) * (1 - std::exp(-Z))
          - (Fdead_fl * BiomassAtAge[age*nFleet + fl] / (Z*Z)) * (1 - std::exp(-Z))
          + (Fdead_fl / Z) * std::exp(-Z) * BiomassAtAge[age*nFleet + fl];
          sumDeriv += tmp;
      }
      predRemovals[fl] = sumRem;
      dct[fl] = sumDeriv;
    } 
    
    // update fleet-level F
    bool converged = true;
    for(int fl=0; fl<nFleet; ++fl){
      double diff = predRemovals[fl] - TotalRemovalsFleet[fl];
      double adj = (0.8*dct[fl] > 1e-10) ? 0.8*dct[fl] : 1e-10;
      res.ApicalF[fl] -= diff / adj;
      
      // enforce maxF
      if(res.ApicalF[fl] > maxF) res.ApicalF[fl] = maxF;
      if(res.ApicalF[fl] < 0.0) res.ApicalF[fl] = 0.0;
      
      if(std::abs(diff)/std::max(1e-8,TotalRemovalsFleet[fl]) > tolF) converged = false;
    }
    
    if(converged) break;
    
    if(debug){
      Rcpp::Rcout << "Iter " << iter << ": ApicalF = ";
      for(auto f : res.ApicalF) Rcpp::Rcout << f << " ";
      Rcpp::Rcout << "\n";
    }
  } // end optimization loop
 
 return res; 
}



#endif