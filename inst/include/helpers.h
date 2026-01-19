#ifndef HELPERS_H
#define HELPERS_H

#include <Rcpp.h>
#include <vector>
#include "array_types.h"
#include "array_views.h"


// Calculate the time step index
inline std::vector<int>
  CalcTSIndex(const Rcpp::NumericVector& Years,
              const Rcpp::NumericVector& YearsAll) {
    Rcpp::IntegerVector matchTS = Rcpp::match(Years, YearsAll);
    const int nTS = Years.size();
    
    std::vector<int> ts_index(nTS);
    
    for (int ts = 0; ts < nTS; ++ts) {
      if (matchTS[ts] == NA_INTEGER) {
        Rcpp::stop("Year not found in `YearsAll`");
      }
      ts_index[ts] = matchTS[ts] - 1; 
    }
    
    return ts_index;
  }


// Extract S4 slots into mutable Array
inline Array2D
Slot2Array2D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot);
  return as_ArrayND<2>(x);   // modifies Hist@slot in place
}

inline Array3D
Slot2Array3D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<3>(x);   
}

inline Array4D
Slot2Array4D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<4>(x);   
}

inline Array5D
Slot2Array5D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = obj.slot(slot); 
  return as_ArrayND<5>(x);   
}


#endif // HELPERS_H
