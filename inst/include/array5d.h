#ifndef MSEtool_ARRAY5D_H
#define MSEtool_ARRAY5D_H

#include <Rcpp.h>

struct Array5D {
  Rcpp::NumericVector x;
  int n1, n2, n3, n4, n5;
  
  inline int map_i(int i) const {
    // Broadcast singleton first dimension
    if (n1 == 1) return 0;
    return i;
  } 
  
  inline void check_bounds(int i, int j, int k, int l, int m) const {
    const int ii = map_i(i);
     
    if (ii < 0 || ii >= n1 ||
        j  < 0 || j  >= n2 ||
        k  < 0 || k  >= n3 ||
        l  < 0 || l  >= n4 ||
        m  < 0 || m  >= n5) {
      Rcpp::stop("Array5D index out of bounds: "
                   "(%d, %d, %d, %d, %d) not in "
                   "[0,%d)x[0,%d)x[0,%d)x[0,%d)x[0,%d) "
                   "(n1 == 1 allows any i)",
                   i, j, k, l, m, n1, n2, n3, n4, n5);
    }
  }
   
  // Column-major (R-compatible) indexing
  inline int idx(int i, int j, int k, int l, int m) const {
    const int ii = map_i(i);
    return ii + n1 * (j + n2 * (k + n3 * (l + n4 * m)));
  } 
  
  inline double& operator()(int i, int j, int k, int l, int m) {
    check_bounds(i, j, k, l, m);
    return x[idx(i, j, k, l, m)];
  } 
  
  inline double operator()(int i, int j, int k, int l, int m) const {
    check_bounds(i, j, k, l, m);
    return x[idx(i, j, k, l, m)];
  } 
};

#endif
