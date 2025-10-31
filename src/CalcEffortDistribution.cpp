#include <RcppArmadillo.h>
#include "check.h"

arma::mat CalcEffortDistribution_(arma::mat VBiomassFleetArea, // nFleet, nArea
                                 arma::vec Effort,
                                 int nArea) {  // nFleet
  
  int nFleet = VBiomassFleetArea.n_rows;
  
  // Rcout << "nArea = " << nArea << std::endl;
  // Rcout << "nFleet = " << nFleet << std::endl;
  // Rcout << "Effort = " << Effort << std::endl;
  // Rcout << "VBiomassFleetArea = " << VBiomassFleetArea << std::endl;
  
  CheckLength(Effort.size(), nFleet, "Effort", "nFleet (nrow(VBiomassFleetArea))");
  
  arma::mat EffortFleetArea(nFleet, nArea, arma::fill::zeros);
  if (nArea==1) {
    EffortFleetArea(arma::span(0, nFleet-1), 0) = Effort;
    return(EffortFleetArea);
  }
  
  for (int fl=0; fl<nFleet; fl++) {
    arma::rowvec relvbiomassarea(nArea, arma::fill::zeros);
    double totalVB = arma::accu(VBiomassFleetArea.row(fl));
    if (totalVB > 0) {
      relvbiomassarea = VBiomassFleetArea.row(fl)/totalVB;
    }
    EffortFleetArea.row(fl) = arma::as_scalar(Effort(fl)) * relvbiomassarea;
  }
  return(EffortFleetArea);
}
