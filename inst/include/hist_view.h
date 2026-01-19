#ifndef HIST_VIEW_H
#define HIST_VIEW_H

#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"

// Lightweight structural guards
inline void check_rank(SEXP x, int expected, const char* name) {
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_isNull(dim) || Rf_length(dim) != expected) {
    Rcpp::stop("%s must be a %dD array", name, expected);
  }
}

inline void check_size(int got, int expected, const char* name) {
  if (got != expected) {
    Rcpp::stop("%s has wrong size (%d != %d)", name, got, expected);
  }
} 


struct HistView {
  
  // Dimensions
  const int nSim;
  const int nStock;
  const int nFleet;
  const int nArea;
  
  // Mutable Hist slots
  Array3D Biomass;        // sim, stock, year
  Array3D SBiomass; 
  Array3D SProduction;
  Array3D Effort;         // sim, year, fleet
  Array4D Distribution;  // sim, year, fleet, area
   
  std::vector<Array4D> Number;            // [stock] sim, age, year, area
  std::vector<Array5D> LandingsAtAge;     // [stock] sim, age, year, fleet, area
  std::vector<Array5D> DiscardsAtAge;
  std::vector<std::vector<Array4D>> LandingsAtSize;   // [stock][fleet] sim, length, year, area
  std::vector<std::vector<Array4D>> DiscardsAtSize;
  std::vector<Array4D> FDead;              // sim, age, year, fleet
  std::vector<Array4D> FRetain;
  std::vector<Array5D> FDeadArea;           // sim, age, year, fleet, area
  std::vector<Array5D> FRetainArea;
  
  // Extract non-mutable objects to from Hist@Misc
  
  // Stock 
  std::vector<ConstArrayView3D> Length;
  std::vector<ConstArrayView3D> Weight;
  std::vector<ConstArrayView3D> NaturalMortality;
  std::vector<ConstArrayView3D> Maturity;
  std::vector<ConstArrayView3D> Semelparous;
  std::vector<ConstArrayView3D> Fecundity;
  
  ConstArrayView1D SPFrom;
  ConstArrayView2D RelSize;
  ConstArrayView2D SpawnTimeFrac;
   
  // Fleet 
  std::vector<ConstArrayView4D> WeightFleet;   // [stock] sim, age, fleet, year
  std::vector<ConstArrayView5D> SelAge;        // [stock] sim, age, year, fleet, area
  std::vector<ConstArrayView5D> RetAge;
  std::vector<ConstArrayView5D> DiscMort;
  
  std::vector<std::vector<ConstArrayView4D>> SelSize; // [stock][fleet] sim, length, year, area
  std::vector<std::vector<ConstArrayView4D>> RetSize;
  
  ConstArrayView4D q;             // sim, stock, year, fleet
  ConstArrayView5D Closure;       // sim, stock, year, fleet, area
  ConstArrayView3D Targeting;     // sim, year, fleet
  
  // Constructor
  HistView(Rcpp::S4& Hist,
           int nSim_,
           int nStock_,
           int nFleet_,
           int nArea_);
};


inline HistView::HistView(Rcpp::S4& Hist,
                          int nSim_,
                          int nStock_,
                          int nFleet_,
                          int nArea_)
  : nSim(nSim_),
    nStock(nStock_),
    nFleet(nFleet_),
    nArea(nArea_),
    Biomass(Slot2Array3D(Hist, "Biomass")),
    SBiomass(Slot2Array3D(Hist, "SBiomass")),
    SProduction(Slot2Array3D(Hist, "SProduction")),
    Effort(Slot2Array3D(Hist, "Effort")),
    Distribution(Slot2Array4D(Hist, "Distribution")),
    SPFrom(GetMisc_ConstArrayView<1>(Hist, "SPFrom")),
    RelSize(GetMisc_ConstArrayView<2>(Hist, "RelSize")),
    SpawnTimeFrac(GetMisc_ConstArrayView<2>(Hist, "SpawnTimeFrac")),
    q(GetMisc_ConstArrayView<4>(Hist, "Catchability")),
    Closure(GetMisc_ConstArrayView<5>(Hist, "Closure")),
    Targeting(GetMisc_ConstArrayView<3>(Hist, "Targeting"))
{

  if (Biomass.dim[0] != nSim) {
    Rcpp::stop("Biomass nSim mismatch");
  }
  
  // Mutable stock slots
  Rcpp::List NumberList         = Hist.slot("Number");
  Rcpp::List LandAgeList        = Hist.slot("LandingsAtAge");
  Rcpp::List DiscAgeList        = Hist.slot("DiscardsAtAge");
  Rcpp::List LandSizeList       = Hist.slot("LandingsAtSize");
  Rcpp::List DiscSizeList       = Hist.slot("DiscardsAtSize");
  Rcpp::List FDeadList          = Hist.slot("FDead");
  Rcpp::List FRetainList        = Hist.slot("FRetain");
  Rcpp::List FDeadAreaList      = Hist.slot("FDeadArea");
  Rcpp::List FRetainAreaList    = Hist.slot("FRetainArea");
   
  check_size(NumberList.size(), nStock, "Number");
  check_size(LandAgeList.size(), nStock, "LandingsAtAge");
   
  Number.reserve(nStock);
  LandingsAtAge.reserve(nStock);
  DiscardsAtAge.reserve(nStock);
  LandingsAtSize.reserve(nStock);
  DiscardsAtSize.reserve(nStock);
  FDead.reserve(nStock);
  FRetain.reserve(nStock);
  FDeadArea.reserve(nStock);
  FRetainArea.reserve(nStock);
   
  for (int st = 0; st < nStock; ++st) {
     
    check_rank(NumberList[st], 4, "Number");
    Number.emplace_back(as_ArrayND<4>(NumberList[st]));
    LandingsAtAge.emplace_back(as_ArrayND<5>(LandAgeList[st]));
    DiscardsAtAge.emplace_back(as_ArrayND<5>(DiscAgeList[st]));
    FDead.emplace_back(as_ArrayND<4>(FDeadList[st]));
    FRetain.emplace_back(as_ArrayND<4>(FRetainList[st]));
    FDeadArea.emplace_back(as_ArrayND<5>(FDeadAreaList[st]));
    FRetainArea.emplace_back(as_ArrayND<5>(FRetainAreaList[st]));
     
    // Size comps per fleet
    Rcpp::List LS = LandSizeList[st];
    Rcpp::List DS = DiscSizeList[st];
    check_size(LS.size(), nFleet, "LandingsAtSize[[st]]");
    check_size(DS.size(), nFleet, "DiscardsAtSize[[st]]");
    
    std::vector<Array4D> ls, ds;
    ls.reserve(nFleet);
    ds.reserve(nFleet);
     
    for (int fl = 0; fl < nFleet; ++fl) {
      ls.emplace_back(as_ArrayND<4>(LS[fl]));
      ds.emplace_back(as_ArrayND<4>(DS[fl]));
    }
     
    LandingsAtSize.emplace_back(std::move(ls));
    DiscardsAtSize.emplace_back(std::move(ds));
  }
  
  // Stock misc
  const Rcpp::List Misc = Hist.slot("Misc");

  auto stock_list3 = [&](const char* nm) {
    Rcpp::List L = Misc[nm];
    check_size(L.size(), nStock, nm);
    return L;
  };

  Rcpp::List LengthList  = stock_list3("LengthList");
  Rcpp::List WeightList  = stock_list3("WeightList");
  Rcpp::List NatMList    = stock_list3("NaturalMortalityList");
  Rcpp::List MatList     = stock_list3("MaturityList");
  Rcpp::List SemList     = stock_list3("SemelparousList");
  Rcpp::List FecList     = stock_list3("FecundityList");

  for (int st = 0; st < nStock; ++st) {
    Length.emplace_back(view_ConstStockList3D(LengthList, st));
    Weight.emplace_back(view_ConstStockList3D(WeightList, st));
    NaturalMortality.emplace_back(view_ConstStockList3D(NatMList, st));
    Maturity.emplace_back(view_ConstStockList3D(MatList, st));
    Semelparous.emplace_back(view_ConstStockList3D(SemList, st));
    Fecundity.emplace_back(view_ConstStockList3D(FecList, st));
  }

  // Fleet misc
  Rcpp::List WeightFleetList = Misc["WeightFleetList"];
  Rcpp::List SelAgeList      = Misc["SelAgeList"];
  Rcpp::List SelSizeList     = Misc["SelSizeList"];
  Rcpp::List RetAgeList      = Misc["RetAgeList"];
  Rcpp::List RetSizeList     = Misc["RetSizeList"];
  Rcpp::List DiscMortList    = Misc["DiscMortList"];
 
  check_size(WeightFleetList.size(), nStock, "WeightFleetList");
  check_size(SelAgeList.size(), nStock, "SelAgeList");
 
  for (int st = 0; st < nStock; ++st) {
    WeightFleet.emplace_back(view_ConstStockList4D(WeightFleetList, st));
    SelAge.emplace_back(view_ConstStockList5D(SelAgeList, st));
    RetAge.emplace_back(view_ConstStockList5D(RetAgeList, st));
    DiscMort.emplace_back(view_ConstStockList5D(DiscMortList, st));
     
    Rcpp::List SS = SelSizeList[st];
    Rcpp::List RS = RetSizeList[st];
    check_size(SS.size(), nFleet, "SelSize[[st]]");
    check_size(RS.size(), nFleet, "RetSize[[st]]");
     
    std::vector<ConstArrayView4D> ss, rs;
    ss.reserve(nFleet);
    rs.reserve(nFleet);
     
    for (int fl = 0; fl < nFleet; ++fl) {
      ss.emplace_back(as_ConstArrayViewND<4>(SS[fl]));
      rs.emplace_back(as_ConstArrayViewND<4>(RS[fl]));
    }
     
    SelSize.emplace_back(std::move(ss));
    RetSize.emplace_back(std::move(rs));
  }
} 


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
view_ConstStockList3D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<3>(x);
}


inline ArrayView4D
view_StockList4D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<4>(x);
}

inline ConstArrayView4D
view_ConstStockList4D(const Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ConstArrayViewND<4>(x);
}


inline ArrayView5D
view_StockList5D(Rcpp::List& StockList, int st) {
  Rcpp::NumericVector x = StockList[st];
  return as_ArrayViewND<5>(x);
}

inline ConstArrayView5D
view_ConstStockList5D(const Rcpp::List& StockList, int st) {
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



#endif
