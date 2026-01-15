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
  Rcpp::NumericVector src = obj.slot(slot);
  Rcpp::NumericVector x   = Rcpp::clone(src);
  obj.slot(slot) = x;              
  return as_ArrayND<2>(x);
}

inline Array3D
Slot2Array3D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector src = obj.slot(slot);
  Rcpp::NumericVector x   = Rcpp::clone(src);
  obj.slot(slot) = x;              
  return as_ArrayND<3>(x);
}

inline Array4D
Slot2Array4D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector src = obj.slot(slot);
  Rcpp::NumericVector x   = Rcpp::clone(src);
  obj.slot(slot) = x;           
  return as_ArrayND<4>(x);
}

inline Array5D
Slot2Array5D(Rcpp::S4& obj, const char* slot) {
  Rcpp::NumericVector src = obj.slot(slot);
  Rcpp::NumericVector x   = Rcpp::clone(src);
  obj.slot(slot) = x;
  return as_ArrayND<5>(x);
}


// Clone Stock List
inline Array3D 
clone_StockList3D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector src = StockList[st];
  Rcpp::NumericVector x = Rcpp::clone(src);
  StockList[st] = x;  
  return as_ArrayND<3>(x);
}

inline Array4D 
clone_StockList4D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector src = StockList[st];
  Rcpp::NumericVector x = Rcpp::clone(src);
  StockList[st] = x;  
  return as_ArrayND<4>(x);
}

inline Array5D 
clone_StockList5D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector src = StockList[st];
  Rcpp::NumericVector x = Rcpp::clone(src);
  StockList[st] = x;  
  return as_ArrayND<5>(x);
}

// Extract from Misc 
inline Rcpp::List
GetMisc_List(Rcpp::S4& obj, const char* name){
  Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
  return Misc[name];
}

inline Array2D
GetMisc_2DArray(Rcpp::S4& obj, const char* name){
  Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
  Rcpp::NumericVector src = Misc[name];
  if (!src.hasAttribute("dim")) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " has no dim attribute");
  }
  
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != 2) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " is not a 3D array");
  }
  
  // clone to ensure mutability + copy-on-write safety
  Rcpp::NumericVector x = Rcpp::clone(src);
  Misc[name] = x;
  
  return as_ArrayND<2>(x);
}

inline Array3D
GetMisc_3DArray(Rcpp::S4& obj, const char* name){
  Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
  Rcpp::NumericVector src = Misc[name];
  if (!src.hasAttribute("dim")) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " has no dim attribute");
  }
  
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != 3) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " is not a 3D array");
  }
  
  // clone to ensure mutability + copy-on-write safety
  Rcpp::NumericVector x = Rcpp::clone(src);
  Misc[name] = x;
  
  return as_ArrayND<3>(x);
}

inline Array4D
GetMisc_4DArray(Rcpp::S4& obj, const char* name){
  Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
  Rcpp::NumericVector src = Misc[name];
  if (!src.hasAttribute("dim")) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " has no dim attribute");
  }
  
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != 4) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " is not a 4D array");
  }
  
  // clone to ensure mutability + copy-on-write safety
  Rcpp::NumericVector x = Rcpp::clone(src);
  Misc[name] = x;
  
  return as_ArrayND<4>(x);
}

inline Array5D
GetMisc_5DArray(Rcpp::S4& obj, const char* name){
  Rcpp::List Misc = obj.slot("Misc");
  if (!Misc.containsElementNamed(name)) {
    Rcpp::stop("Hist@Misc$%s not found", name);
  }
  Rcpp::NumericVector src = Misc[name];
  if (!src.hasAttribute("dim")) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " has no dim attribute");
  }
  
  Rcpp::IntegerVector dim = src.attr("dim");
  if (dim.size() != 5) {
    Rcpp::stop(std::string("Hist@Misc$") + name + " is not a 5D array");
  }
  
  // clone to ensure mutability + copy-on-write safety
  Rcpp::NumericVector x = Rcpp::clone(src);
  Misc[name] = x;
  
  return as_ArrayND<5>(x);
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



#endif // HELPERS_H

