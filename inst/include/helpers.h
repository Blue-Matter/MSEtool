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

// Extract + clone S4 slots into mutable Array

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

// Clone Stock List
// inline Rcpp::List clone_StockList(const Rcpp::List& src) {
//   const int n = src.size();
//   Rcpp::List out(n);
//   for (int i = 0; i < n; ++i) {
//     out[i] = Rcpp::clone(Rcpp::NumericVector(src[i]));
//   } 
//   return out;
// }
// 
// inline Rcpp::List clone_StockSlot(const Rcpp::S4& obj,
//                                   const char* slot) {
//   return clone_StockList(obj.slot(slot));
// }


// Non-mutable S4 slot arrays
template <size_t N>
inline ConstArrayViewND<N>
GetMisc_ConstArrayView(Rcpp::S4& obj, const char* name)
{
  const Rcpp::List Misc = obj.slot("Misc");
  
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
   
  Rcpp::NumericVector src = Misc[name];
   
  if (!src.hasAttribute("dim")) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " has no dim attribute");
  }
   
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != N) {
    Rcpp::stop(std::string("Hist@Misc$") + name +
      " is not a " + std::to_string(N) + "D array");
  } 
  
  return as_ConstArrayViewND<N>(src);
} 








// StockList Views
inline ArrayView3D
view_StockList3D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<3>(x);
}

inline ConstArrayView3D
view_StockList3D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<3>(x);
}

inline ArrayView4D
view_StockList4D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<4>(x);
}

inline ConstArrayView4D
view_StockList4D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<4>(x);
}

inline ArrayView5D
view_StockList5D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<5>(x);
}


inline ConstArrayView5D
view_StockList5D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<5>(x);
}


// Calculate nSim for generic set of lists or arrays
inline void update_nSim(int& nSim, int candidate) {
  if (candidate > nSim) nSim = candidate;
}

template <size_t N>
inline void infer_nSim_from(int& nSim, const ArrayND<N>& x) {
  update_nSim(nSim, x.dim[0]);
}

inline void infer_nSim_from(int& nSim, const Rcpp::List& L) {
  for (int i = 0; i < L.size(); ++i) {
    if (Rcpp::is<Rcpp::NumericVector>(L[i])) {
      Rcpp::NumericVector arr = L[i];
      if (!arr.hasAttribute("dim")) continue;
      Rcpp::IntegerVector dim = arr.attr("dim");
      if (dim.size() >= 1) {
        update_nSim(nSim, dim[0]);
      }
    } 
  }
}

template <typename... Args>
inline int infer_nSim(const Args&... args) {
  int nSim = 0;
  (infer_nSim_from(nSim, args), ...);
  if (nSim == 0)
    Rcpp::stop("infer_nSim(): could not infer nSim from inputs");
  return nSim;
}

template <size_t N>
inline void infer_nSim_from(int& nSim, const ArrayViewND<N>& x) {
  update_nSim(nSim, x.dim[0]);
}

template <size_t N>
inline void infer_nSim_from(int& nSim, const ConstArrayViewND<N>& x) {
  update_nSim(nSim, x.dim[0]);
}

template <size_t N>
inline void infer_nSim_from(int& nSim,
                            const std::vector<ArrayND<N>>& v) {
  for (const auto& x : v) {
    update_nSim(nSim, x.dim[0]);
  }
}

template <size_t N>
inline void infer_nSim_from(int& nSim,
                            const std::vector<ConstArrayViewND<N>>& v) {
  for (const auto& x : v) {
    update_nSim(nSim, x.dim[0]);
  }
}



#endif // HELPERS_H

