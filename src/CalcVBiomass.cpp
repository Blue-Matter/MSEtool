#include <RcppArmadillo.h>
#include "check.h"
using namespace Rcpp;

arma::mat CalcVBiomass_(arma::mat NumberAtAgeArea, // nAge, nArea
                        arma::mat FleetWeightAtAgeFleet, // nAge, nFleet
                        arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                        arma::mat ClosureFleetArea) { // nFleet, nArea) 
  
  int nAge = NumberAtAgeArea.n_rows;
  int nArea = NumberAtAgeArea.n_cols;
  int nFleet = FleetWeightAtAgeFleet.n_cols;
  
  if (nArea==1)
    ClosureFleetArea = ClosureFleetArea.t();
  
  
  CheckDimensions(nAge, FleetWeightAtAgeFleet.n_rows, "FleetWeightAtAgeFleet", "nAge");
  CheckDimensions(nAge, SelectivityAtAgeFleet.n_rows, "SelectivityAtAgeFleet", "nAge");
  CheckDimensions(nFleet, SelectivityAtAgeFleet.n_cols, "SelectivityAtAgeFleet", "nFleet", 1);
  CheckDimensions(nFleet, ClosureFleetArea.n_rows, "ClosureFleetArea", "nFleet");
  CheckDimensions(nArea, ClosureFleetArea.n_cols, "ClosureFleetArea", "nArea", 1);
  
  arma::mat VBiomassFleetArea(nFleet, nArea, arma::fill::zeros);
  for (int area=0; area<nArea; area++) {
    for (int fl=0; fl<nFleet; fl++) {
      VBiomassFleetArea.row(fl).col(area) += arma::sum(NumberAtAgeArea.col(area) %
        SelectivityAtAgeFleet.col(fl) %
        FleetWeightAtAgeFleet.col(fl),0) *
        arma::as_scalar(ClosureFleetArea.row(fl).col(area));
    }
    
  }
  return(VBiomassFleetArea);
}
