#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

NumericVector ptnorm_(double q,
                      NumericVector mu, 
                      NumericVector sd,
                      double truncsd) {
  
  NumericVector a = (-truncsd*sd) + mu;
  NumericVector b = (truncsd*sd) + mu;
  
  int I = a.length();
  
  NumericVector out(I);
  for (int i = 0; i<I; i++) {
    if (q<a[i]) {
      out[i] = 0;
    } else if (q>b[i]) {
      out[i] = 1;
    } else {
      double p1 = R::pnorm(q, mu[i], sd[i], true, false);
      double p2 = R::pnorm(a[i], mu[i], sd[i], true, false);
      double p3 = R::pnorm(b[i], mu[i], sd[i], true, false);
      out[i] = (p1-p2)/(p3-p2);
    }
  }
  return(out);
}


// [[Rcpp::export]]
arma::cube CalcAgeSizeKey_(NumericMatrix MeanAtAge, // nAge, nTS
                           NumericMatrix SDatAge, // nAge, nTS
                           NumericVector Classes, // nClass
                           double TruncSD,
                           CharacterVector Dist) {

  int nAge = MeanAtAge.nrow();
  int nTS = MeanAtAge.ncol();
  int nClass = Classes.length();

  arma::cube ASK(nAge, nClass, nTS);
  
  double By = Classes[1] - Classes[0];
  NumericVector ClassesLower(nClass);
  
  for (int i=0; i<nClass; i++) {
    ClassesLower[i] = Classes[i]-0.5*By;
  }
  
  for (int ts=0; ts<nTS; ts++) {
    NumericVector sd = SDatAge(_,ts);
    NumericVector mu = MeanAtAge(_,ts);
    
    if (Dist[0]=="lognormal") {
      sd = sd/mu;
      sd[!is_finite(sd)] = 0.05;
      mu = log(mu) - 0.5 * pow(sd,2);
      mu[!is_finite(mu)] = log(1E-6);
      ClassesLower = log(ClassesLower);
    }
  
    NumericVector prob1 =  ptnorm_(ClassesLower[1], mu, sd, TruncSD);
    ASK(arma::span(0, nAge-1), arma::span(0), arma::span(ts)) = as<arma::vec>(wrap(prob1));
    for (int l=1; l<(nClass-1); l++) {
      NumericVector prob2 = ptnorm_(ClassesLower[l+1], mu, sd, TruncSD);
      NumericVector prob3 = ptnorm_(ClassesLower[l], mu, sd, TruncSD);
      ASK(arma::span(0, nAge-1), arma::span(l), arma::span(ts)) = as<arma::vec>(wrap(prob2)) - as<arma::vec>(wrap(prob3));
    }
    NumericVector prob4 = 1 - ptnorm_(ClassesLower[nClass-1], mu, sd, TruncSD);
    ASK(arma::span(0, nAge-1), arma::span(nClass-1), arma::span(ts)) = as<arma::vec>(wrap(prob4));
  }

  return(ASK);

}