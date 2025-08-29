#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calcVatAge(NumericMatrix len_at_age,
                     NumericMatrix len_aa_sd,
                     NumericMatrix sel_at_length,
                     int n_age,
                     int nyears,
                     int proyears,
                     NumericVector CAL_binsmid) {
  int totyears = nyears+proyears;
  NumericMatrix V(n_age, totyears);
  int nbins = CAL_binsmid.length();

  for (int yr=0; yr<totyears; yr++) {
    NumericMatrix ALK(n_age, nbins);
    NumericMatrix ALKs(n_age, nbins);
    NumericVector page(n_age);
    for (int age=0; age<n_age; age++) {
      ALK(age, _) = Rcpp::dnorm(CAL_binsmid, len_at_age(age,yr), len_aa_sd(age,yr), false);
      page(age) = sum(ALK(age,_));
    }
    LogicalVector  b1 = page>0;
    if (is_false(all(b1))) {
      ALK(0,0) = 1;
    }

    NumericVector sela(n_age);
    for (int age=0; age<n_age; age++) {
      if (page(age)>0) {
        ALKs(age,_) = ALK(age,_)/page(age);
      }
      for (int len=0; len<nbins; len++) {
        sela(age) += ALKs(age,len)*sel_at_length(len,yr);
      }
    }
    V(_,yr) = sela;

  }

  return(V);

}


