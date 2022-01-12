#include <RcppArmadillo.h>
using namespace Rcpp;
//[[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export]]
List grav(arma::vec log_visc, arma::vec log_grav, arma::vec fracs, int nareas) {
  // Fill in gravity matrix
  arma::mat grav = arma::zeros(nareas, nareas);

  for(int af=0; af<nareas; af++) { // area from
    grav(af,af) += log_visc(af);
    for(int at=1; at<nareas; at++) { // area to
      grav(af,at) += log_grav(at-1);
    }
  }

  // Calculate logit fractions (movement to area from area)
  arma::vec gsum(nareas);
  arma::mat mov(nareas, nareas);

  gsum.zeros();

  for(int af=0; af<nareas; af++) {
    for(int at=0; at<nareas; at++) gsum(af) += exp(grav(af,at)); // sum up gravity terms by from area
    for(int at=0; at<nareas; at++) {
      mov(af,at) = exp(grav(af,at))/gsum(af); // calculate logit mov probs by row (area from)
    }
  }

  // Propagate movement matrix
  arma::mat transN(nareas, nareas);
  arma::vec idist(nareas);
  arma::vec Nsum(nareas);

  idist.fill(1.0/nareas);

  for(int tt=0; tt<50; tt++) {
    for(int af=0; af<nareas; af++) {
      for(int at=0; at<nareas; at++) {
        transN(af,at) = idist(af) * mov(af,at);
      }
    }
    for(int at=0; at<nareas; at++) Nsum(at) = sum(transN.col(at));
    idist = Nsum;
  }

  // Mean probability of staying
  double psum = 0;
  for(int aa=0; aa<nareas; aa++) psum += mov(aa,aa)/nareas;

  return List::create(Named("idist") = idist, Named("transN") = transN, Named("grav") = grav,
                      Named("mov") = mov, Named("psum") = psum);
}
