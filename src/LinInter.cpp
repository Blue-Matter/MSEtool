#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int vecminInd(NumericVector x) {
  // Rcpp supports STL-style iterators
  NumericVector::iterator it = std::min_element(x.begin(), x.end());
  // we want the value so dereference
  return it - x.begin();
}

// [[Rcpp::export]]
double LinInterp_cpp(NumericVector x, NumericVector y, double xlev) {

  NumericVector x2 = pow(x-xlev,2);
  int close = vecminInd(x2);
  
  double xsb = x[close];
  int close2 = (xsb < xlev)*2-1;
  NumericVector ind(2);
  ind[0] = close;
  ind[1] = close+close2;
  int len = x.length();
  if (ind[1]>len-1) {
    ind[1] = ind[0];
    ind[0] = ind[0]-1;
  }
  double pos = (xlev-x[ind[0]])/(x[ind[1]]-x[ind[0]]);
  double yout = y[ind[0]]+pos*(y[ind[1]]-y[ind[0]]);
  return yout;
}
