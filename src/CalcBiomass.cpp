#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// [[Rcpp::export]]
double CalcBiomass_(arma::mat NumberAtAgeArea, // nAge, nArea
                    arma::vec WeightAtAge){ // nAge
  
  double B = arma::accu(WeightAtAge %  arma::sum(NumberAtAgeArea,1));
  return(B);
}