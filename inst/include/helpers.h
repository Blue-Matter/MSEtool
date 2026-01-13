#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"

// Calculate the time step index
inline std::vector<int>
CalcTSIndex(const Rcpp::NumericVector& Years,
                   const Rcpp::NumericVector& YearsAll)
{
  Rcpp::IntegerVector matchTS = Rcpp::match(Years, YearsAll);
  int nTS = Years.size();
  
  std::vector<int> ts_index(nTS);
  
  for (int ts = 0; ts < nTS; ++ts) {
    if (matchTS[ts] == NA_INTEGER) {
      Rcpp::stop("Year not found in `YearsAll`");
    }
    ts_index[ts] = matchTS[ts] - 1; 
  }
  
  return ts_index;
}

// ---------------------------------------------------------
// Extract mutable objects from Hist or Lists
// ---------------------------------------------------------

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