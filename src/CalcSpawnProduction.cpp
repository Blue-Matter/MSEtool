#include <RcppArmadillo.h>
#include "check.h"
using namespace Rcpp;
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec CalcSpawnProduction_(arma::mat NumberAtAgeArea, // nAge, nArea
                              arma::vec FecundityAtAge, // nAge
                              arma::vec MaturityAtAge, // nAge,
                              arma::vec WeightAtAge, // nAge
                              arma::vec NaturalMortalityAtAge, // nAge
                              arma::mat FDeadAtAgeArea, // nAge, nArea
                              arma::vec SpawnMortality, // nAge
                              double SpawnTimeFrac= 0) {
  
  int nAge = NumberAtAgeArea.n_rows;
  int nArea = NumberAtAgeArea.n_cols;
  
  CheckLength(FecundityAtAge.size(), nAge, "FecundityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(MaturityAtAge.size(), nAge, "MaturityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(WeightAtAge.size(), nAge, "WeightAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckLength(NaturalMortalityAtAge.size(), nAge, "NaturalMortalityAtAge", "nAge (nrow(NumberAtAgeArea))");
  CheckDimensions(nAge, FDeadAtAgeArea.n_rows, "FDeadAtAgeArea", "nAge");
  CheckDimensions(nArea, FDeadAtAgeArea.n_cols, "FDeadAtAgeArea", "nArea", 1);
  
  arma::mat SProductionArea(nAge, nArea, arma::fill::zeros); // nAge, nArea
  arma::mat SBiomassArea(nAge, nArea, arma::fill::zeros); // nAge, nArea
  
  for (int area=0; area<nArea; area++) {
    arma::vec NSpawnThisArea = NumberAtAgeArea.col(area);
    
    if (SpawnTimeFrac > 0) {
      arma::vec FDeadThisArea = FDeadAtAgeArea.col(area); // nAge
      arma::vec SpawnMortality2 = (NaturalMortalityAtAge + FDeadThisArea) * SpawnTimeFrac;
      
      bool UseSpawnMortality = arma::all(SpawnMortality > 0);
      
      if (UseSpawnMortality) {
        // Rcout << "Using custom SpawnMortality" << SpawnMortality << std::endl;
        
        NSpawnThisArea = NSpawnThisArea % exp(-SpawnMortality);
      } else {
        NSpawnThisArea = NSpawnThisArea % exp(-SpawnMortality2);  
      }
      
      
    }
  
    SProductionArea.col(area) = NSpawnThisArea % FecundityAtAge;
    SBiomassArea.col(area) = NSpawnThisArea % WeightAtAge % MaturityAtAge; 
  }
  
  arma::vec out(2, arma::fill::zeros);
  out[0] = arma::accu(SProductionArea);
  out[1] = arma::accu(SBiomassArea);
  return(out);
}