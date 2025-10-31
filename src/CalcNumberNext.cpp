#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat CalcNumberNext_(arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
                          arma::mat NumberAtAgeAreaNextTS, // nAge, nArea
                          arma::vec Semelparous,  // nAge
                          arma::cube FDeadAtAgeAreaThisTS,  // nAge, nFleet, nArea
                          arma::vec NaturalMortalityAtAgeThisTS,
                          bool plusgroup,
                          int nAge,
                          int nArea) {
  
  arma::mat Nnext = NumberAtAgeAreaNextTS;
  
  arma::mat FDead = arma::sum(FDeadAtAgeAreaThisTS, 1);
  arma::mat Zmortality = FDead.each_col() + NaturalMortalityAtAgeThisTS;
  
  for (int age=0; age<(nAge-1); age++) {
    Nnext.row(age+1) = NumberAtAgeAreaThisTS.row(age) % exp(-Zmortality.row(age)) * (1-Semelparous(age));
  }
  
  if (plusgroup) {
    Nnext.row(nAge-1) = Nnext.row(nAge-1) + (NumberAtAgeAreaThisTS.row(nAge-1) % exp(-Zmortality.row(nAge-1)) * (1-Semelparous(nAge-1)));
  }
  
  return(Nnext);
}