#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;


// [[Rcpp::export]]
List SolveForFishingMortality(arma::vec NumberAtAge, // nAge
                              arma::vec TotalRemovalsFleet, // nFleet (biomass)
                              arma::mat SelectivityAtAge,  // nAge, nFleet
                              arma::mat RetentionAtAge,  // nAge, nFleet
                              arma::mat DiscardMortalityAtAge,  // nAge, nFleet
                              arma::mat FleetWeightAtAge,  // nAge, nFleet
                              arma::vec NaturalMortalityAtAge, // nAge
                              int MaxIt=200,
                              double tolF=1E-4,
                              int debug=0) {
  
  int nAge = SelectivityAtAge.n_rows;
  int nFleet = SelectivityAtAge.n_cols;
  
  arma::mat BiomassAtAge(nAge, nFleet);
  
  for (int fl=0; fl<nFleet; fl++) {
    BiomassAtAge.col(fl) = NumberAtAge % FleetWeightAtAge.col(fl);
  }
  
  
  double TotalBiomass = arma::accu(BiomassAtAge);
  
  // initial guess at apicalF
  arma::vec apicalF = TotalRemovalsFleet/TotalBiomass;
  arma::mat FDeadAtAge(nAge, nFleet);
  arma::mat FRetainAtAge(nAge, nFleet);

  for (int i=0; i<=MaxIt; i++) {

    // Calculate F-at-age fleet
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FInteract = apicalF(fl) * SelectivityAtAge.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAge.col(fl);
      arma::vec FDiscard = FInteract - FRetain;
      arma::vec FDeadDiscard = FDiscard % DiscardMortalityAtAge.col(fl);
      arma::vec FDead = FDeadDiscard + FRetain;
      FRetainAtAge.col(fl) = FDead;
      FDeadAtAge.col(fl) = FRetain;
    }
    
    arma::vec ZatAge = arma::sum(FDeadAtAge,1) + NaturalMortalityAtAge;
    
    // derivative of catch wrt ft
    arma::vec dct(nFleet);
    arma::vec predRemovals(nFleet);
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % BiomassAtAge.col(fl);
      arma::vec FDead = FDeadAtAge.col(fl);
      predRemovals(fl) = arma::accu(FDead/ZatAge % (1-exp(-ZatAge)) % BiomassAtAge.col(fl));
      arma::vec temp = PopDeadAtAge / ZatAge - ((FDead % PopDeadAtAge / pow(ZatAge,2))) + FDead/ZatAge % exp(-ZatAge)%BiomassAtAge.col(fl);
      dct(fl) = accu(temp);
    }
    
    apicalF = apicalF - (predRemovals - TotalRemovalsFleet)/(0.8*dct);
    NumericVector diff = as<NumericVector>(Rcpp::wrap((predRemovals - TotalRemovalsFleet)));
    NumericVector totalremovals = as<NumericVector>(Rcpp::wrap((TotalRemovalsFleet)));
    
    LogicalVector converge = (Rcpp::abs(diff)/totalremovals) < tolF;
    LogicalVector ZeroVals = totalremovals <= 1E-5;
    converge[ZeroVals] = 1;
    
    if (debug) {
      Rcout << "i = " << i << std::endl; 
      NumericVector temp = (Rcpp::abs(diff)/totalremovals);
      Rcout << "predRemovals = " << predRemovals << std::endl; 
      Rcout << "TotalRemovalsFleet = " << TotalRemovalsFleet << std::endl; 
      Rcout << "diff = " << diff << std::endl; 
      Rcout << "diff = " << temp  << std::endl; 
      Rcout << "converge = " << converge  << std::endl; 
    }
      
  
    if (all(converge).is_true()) 
      break;
      
  }

  List L = List::create(Named("ApicalFInteract") = apicalF,
                        Named("FDeadAtAge") = FDeadAtAge,
                        Named("FRetainAtAge") = FRetainAtAge);
  return(L);
}