// accessors for 3, 4, and 5D arrays

#ifndef MSEtool_ARRAY_VIEWS_H
#define MSEtool_ARRAY_VIEWS_H

#include <Rcpp.h>
#include "array3d.h"
#include "array4d.h"
#include "array5d.h"

// Generic slot extractor
inline Rcpp::NumericVector get_array_slot(
    Rcpp::S4& obj,
    const char* slot,
    int expected_ndim
) {
  Rcpp::NumericVector x = obj.slot(slot);
  Rcpp::IntegerVector d = x.attr("dim");
   
  if (d.size() != expected_ndim) {
    Rcpp::stop(
      "%s must be a %dD array (got %dD)",
      slot, expected_ndim, d.size()
    );
  }
   
  return x;
}

// Typed array views
inline Array3D make_Array3D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = get_array_slot(obj, slot, 3);
  Rcpp::IntegerVector d = x.attr("dim");
  return Array3D{ x, d[0], d[1], d[2] };
}

inline Array4D make_Array4D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = get_array_slot(obj, slot, 4);
  Rcpp::IntegerVector d = x.attr("dim");
  return Array4D{ x, d[0], d[1], d[2], d[3] };
} 

inline Array5D make_Array5D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector x = get_array_slot(obj, slot, 5);
  Rcpp::IntegerVector d = x.attr("dim");
  return Array5D{ x, d[0], d[1], d[2], d[3], d[4] };
} 


#endif
