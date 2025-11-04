
#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::cube CalcStockMovement_(arma::cube NumberAtAgeArea,
                              arma::cube Movement, // FromArea, ToArea, nAge 
                              int nAge,
                              int nArea,
                              int TSindex) {
  
  arma::mat NumberAtAgeAreaThisTS =  NumberAtAgeArea.col(TSindex); // nAge, nArea
  arma::mat NumberAtAgeAreaMoved(nAge, nArea, arma::fill::zeros);  // nAge, nArea
  
  for (int age=0; age<nAge; age++) {
    arma::mat moveage = Movement.slice(age);
    for (int toArea=0; toArea<nArea; toArea++) {
      arma::vec Narea(nArea, arma::fill::zeros);
      for (int fromArea=0; fromArea<nArea; fromArea++) {
        Narea(toArea) += NumberAtAgeAreaThisTS(age, fromArea) * moveage(fromArea, toArea);
      }
      NumberAtAgeAreaMoved(age, toArea) = Narea(toArea);
    }
  }
  
  NumberAtAgeArea.col(TSindex) = NumberAtAgeAreaMoved;
  return(NumberAtAgeArea);
}
