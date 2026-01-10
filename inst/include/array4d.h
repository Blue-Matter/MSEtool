#ifndef MSEtool_ARRAY4D_H
#define MSEtool_ARRAY4D_H

#include <Rcpp.h>

struct Array4D {
  Rcpp::NumericVector x;
  int n1, n2, n3, n4;
  
  inline int map_i(int i) const {
    // Broadcast singleton first dimension
    if (n1 == 1) return 0;
    return i;
  } 
  
  inline void check_bounds(int i, int j, int k, int l) const {
    const int ii = map_i(i);
    
    if (ii < 0 || ii >= n1 ||
        j  < 0 || j  >= n2 ||
        k  < 0 || k  >= n3 ||
        l  < 0 || l  >= n4) {
      Rcpp::stop("Array4D index out of bounds: "
                   "(%d, %d, %d, %d) not in "
                   "[0,%d)x[0,%d)x[0,%d)x[0,%d) "
                   "(n1 == 1 allows any i)",
                   i, j, k, l, n1, n2, n3, n4);
    }
  }
   
  // Column-major (R-compatible) indexing
  inline int idx(int i, int j, int k, int l) const {
    const int ii = map_i(i);
    return ii + n1 * (j + n2 * (k + n3 * l));
  }
  
  inline double& operator()(int i, int j, int k, int l) {
    check_bounds(i, j, k, l);
    return x[idx(i, j, k, l)];
  }
  
  inline double operator()(int i, int j, int k, int l) const {
    check_bounds(i, j, k, l);
    return x[idx(i, j, k, l)];
  }
};

#endif