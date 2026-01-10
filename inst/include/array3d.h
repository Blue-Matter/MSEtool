#ifndef MSEtool_ARRAY3D_H
#define MSEtool_ARRAY3D_H

#include <Rcpp.h>

struct Array3D {
  Rcpp::NumericVector x;
  int n1, n2, n3;
  
  inline int map_i(int i) const {
    // Broadcast singleton first dimension
    if (n1 == 1) return 0;
    return i;
  }
  
  inline void check_bounds(int i, int j, int k) const {
    const int ii = map_i(i);
    
    if (ii < 0 || ii >= n1 ||
        j  < 0 || j  >= n2 ||
        k  < 0 || k  >= n3) {
      Rcpp::stop("Array3D index out of bounds: "
                   "(%d, %d, %d) not in "
                   "[0,%d)x[0,%d)x[0,%d) "
                   "(n1 == 1 allows any i)",
                   i, j, k, n1, n2, n3);
    }
  }
  
  // Column-major (R-compatible) indexing
  inline int idx(int i, int j, int k) const {
    const int ii = map_i(i);
    return ii + n1 * (j + n2 * k);
  }
  
  inline double& operator()(int i, int j, int k) {
    check_bounds(i, j, k);
    return x[idx(i, j, k)];
  }
  
  inline double operator()(int i, int j, int k) const {
    check_bounds(i, j, k);
    return x[idx(i, j, k)];
  }
};

#endif
