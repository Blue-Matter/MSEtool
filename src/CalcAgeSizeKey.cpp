#include <RcppArmadillo.h>
#include "check.h"
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

arma::vec ptnorm_(double q,
                  arma::vec mean, 
                  arma::vec sd,
                  double truncsd) {
  
  arma::vec a = (-truncsd*sd) + mean;
  arma::vec b = (truncsd*sd) + mean;
  
  int I = mean.length();
  
  arma::vec out(I);
  for (int i = 0; i<I; i++) {
    if (q<a[i]) {
      out[i] = 0;
    } else if (q>b[i]) {
      out[i] = 1;
    } else {
      arma::vec p1 = pnorm(q, mean[i], sd[i], true, false);
      arma::vec p2 = pnorm(a[i], mean[i], sd[i], true, false);
      arma::vec p3 = pnorm(b[i], mean[i], sd[i], true, false);
      out[i] = (p1[0]-p2[0])/(p3[0]-p2[0]);
    }
  }

  return(out);
}


// [[Rcpp::export]]
arma::cube CalcAgeSizeKey_(arma::mat MeanAtAge, // nAge, nTS
                           arma::mat SDatAge, // nAge, nTS
                           arma::vec Classes, // nClass
                           double TruncSD,
                           CharacterVector Dist 
) {
  
  
  int nAge = MeanAtAge.n_rows;
  int nTS = MeanAtAge.n_cols;
  int nClass = Classes.size();
  
  arma::cube ASK(nAge, nClass, nTS);
  
  for (int ts=0; ts<nTS; ts++) {

    arma::vec sd = SDatAge.col(ts);
    arma::vec mu = MeanAtAge.col(ts);
    
    if (Dist[0]=="normal") {
      arma::vec classLower = Classes;
    
    } else {
      sd = sd/mu;
      arma::vec classLower = log(Classes);
    }
    

    // make reference to R function 
    ASK(arma::span(0, nAge-1), arma::span(0, 0), arma::span(ts, ts)) = ptnorm_(classLower[1], mean=mu, sd=sd, truncsd=TruncSD) 
      
    for (int l=1; l<(nClass-1); l++) {
      ASK(arma::span(0, nAge-1), arma::span(l, l), arma::span(ts, ts)) 
    }
    
  
    
    
    
  }
  
  return(ASK);
  
}