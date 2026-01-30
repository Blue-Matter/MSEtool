#ifndef HIST_VIEW_H
#define HIST_VIEW_H

#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"

// Lightweight structural guards
inline void check_rank(SEXP x, int expected, const char* name) {
  if (!Rf_isNumeric(x) && !Rf_isInteger(x)) {
    Rcpp::stop(std::string(name) + " must be numeric");
  }
  
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_isNull(dim) || Rf_length(dim) != expected) {
    Rcpp::stop(std::string(name) + " must be a " + std::to_string(expected) + "D array");
  }
}

inline void check_size(int got, int expected, const char* name) {
  if (got != expected) {
    Rcpp::stop(std::string(name) + " has wrong size (" + 
      std::to_string(got) + " != " + std::to_string(expected) + ")");
  }
} 


inline void validate_Hist_misc(Rcpp::S4& Hist) {
  const Rcpp::List Misc = Hist.slot("Misc");
  
  auto require_misc = [&](const char* nm, int rank) {
    if (!Misc.containsElementNamed(nm))
      Rcpp::stop(std::string("Hist@Misc$") + nm + " is missing");
    check_rank(Misc[nm], rank, nm);
  };
   
  require_misc("SP0", 3);
  require_misc("R0", 3);
  require_misc("RecDist", 4);
  require_misc("SPFrom", 1);
  require_misc("PlusGroup", 1);
  require_misc("RelSize", 2);
  require_misc("SpawnTimeFrac", 2);
  require_misc("Catchability", 4);
  require_misc("Closure", 5);
  require_misc("Targeting", 3);
} 


struct HistView {
  
  // Dimensions
  const int nSim;
  const int nStock;
  const int nFleet;
  const int nArea;
  
  
  ConstArrayView3D SP0;           // sim, stock, year
  ConstArrayView3D R0;            // sim, stock, year
  ConstArrayView4D RecDist;       // sim, stock, year, area
  ConstArrayView1D SPFrom;        // stock
  ConstArrayView1D PlusGroup;        // stock
  ConstArrayView2D RelSize;       // sim, area
  ConstArrayView2D SpawnTimeFrac; // sim, stock
  ConstArrayView4D q;             // sim, stock, year, fleet
  ConstArrayView5D Closure;       // sim, stock, year, fleet, area
  ConstArrayView3D Targeting;     // sim, year, fleet
  
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
  std::vector<ConstArrayView5D> Movement;
  
  // SRR parameters
  std::vector<std::vector<ConstArrayView2D>> SRR_Pars;
  std::vector<ConstArrayView2D> RecDevs;
  std::vector<int> RecLag; // [stock] 
  std::vector<int> SRR_Model; // [stock] SRR model code: 0,1,2
  
  // Fleet 
  std::vector<ConstArrayView4D> WeightFleet;   // [stock] sim, age, fleet, year
  std::vector<ConstArrayView5D> SelAge;        // [stock] sim, age, year, fleet, area
  std::vector<ConstArrayView5D> RetAge;
  std::vector<ConstArrayView5D> DiscMort;
  
  std::vector<std::vector<ConstArrayView4D>> SelSize; // [stock][fleet] sim, length, year, area
  std::vector<std::vector<ConstArrayView4D>> RetSize;
  
  double maxF;
  
  // Constructor
public:
  HistView(Rcpp::S4& Hist,
           int nSim_,
           int nStock_,
           int nFleet_,
           int nArea_);
  
private:
  HistView(std::nullptr_t,
           Rcpp::S4& Hist,
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
  : HistView(
      nullptr,
      ([&]() -> Rcpp::S4& {
        
        validate_Hist_misc(Hist);
        
        check_rank(Hist.slot("Biomass"), 3, "Biomass");
        check_rank(Hist.slot("SBiomass"), 3, "SBiomass");
        check_rank(Hist.slot("SProduction"), 3, "SProduction");
        check_rank(Hist.slot("Effort"), 3, "Effort");
        check_rank(Hist.slot("Distribution"), 4, "Distribution");
         
        return Hist;
      })(),
      nSim_,
      nStock_,
      nFleet_,
      nArea_
  )
{}


inline HistView::HistView(std::nullptr_t,
                          Rcpp::S4& Hist,
                          int nSim_,
                          int nStock_,
                          int nFleet_,
                          int nArea_)
  : nSim(nSim_),
    nStock(nStock_),
    nFleet(nFleet_),
    nArea(nArea_),
    
    // ---- non-default-constructible views (MUST be here) ----
    SP0(GetMisc_ConstArrayView<3>(Hist, "SP0")),
    R0(GetMisc_ConstArrayView<3>(Hist, "R0")),
    RecDist(GetMisc_ConstArrayView<4>(Hist, "RecDist")),
    SPFrom(GetMisc_ConstArrayView<1>(Hist, "SPFrom")),
    PlusGroup(GetMisc_ConstArrayView<1>(Hist, "PlusGroup")),
    RelSize(GetMisc_ConstArrayView<2>(Hist, "RelSize")),
    SpawnTimeFrac(GetMisc_ConstArrayView<2>(Hist, "SpawnTimeFrac")),
    q(GetMisc_ConstArrayView<4>(Hist, "Catchability")),
    Closure(GetMisc_ConstArrayView<5>(Hist, "Closure")),
    Targeting(GetMisc_ConstArrayView<3>(Hist, "Targeting"))
{
  
  Biomass = Slot2Array3D(Hist, "Biomass");
  SBiomass = Slot2Array3D(Hist, "SBiomass");
  SProduction = Slot2Array3D(Hist, "SProduction");
  Effort = Slot2Array3D(Hist, "Effort");
  Distribution = Slot2Array4D(Hist, "Distribution");
  
  const Rcpp::List Misc = Hist.slot("Misc");
  
  if (Biomass.dim[0] != nSim) {
    Rcpp::stop("Biomass nSim mismatch");
  } 
  
  if (RecDist.dim[1] != nStock)
    Rcpp::stop("RecDist stock dimension mismatch");
  if (RecDist.dim[3] != nArea)
    Rcpp::stop("RecDist area dimension mismatch");
  
  if (SP0.dim[1] != nStock)
    Rcpp::stop("SP0 stock dimension mismatch");
  if (R0.dim[1] != nStock)
    Rcpp::stop("R0 stock dimension mismatch");

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
  Rcpp::List MovementList     = stock_list3("MovementList");
  
  
  for (int st = 0; st < nStock; ++st) {
    check_rank(LengthList[st], 3, "LengthList[[st]]");
    Length.emplace_back(view_ConstStockList3D(LengthList, st));
    
    check_rank(WeightList[st], 3, "WeightList[[st]]");
    Weight.emplace_back(view_ConstStockList3D(WeightList, st));
    
    check_rank(NatMList[st], 3, "NatMList[[st]]");
    NaturalMortality.emplace_back(view_ConstStockList3D(NatMList, st));
    
    check_rank(MatList[st], 3, "MatList[[st]]");
    Maturity.emplace_back(view_ConstStockList3D(MatList, st));
    
    check_rank(SemList[st], 3, "SemList[[st]]");
    Semelparous.emplace_back(view_ConstStockList3D(SemList, st));
    
    check_rank(FecList[st], 3, "FecList[[st]]");
    Fecundity.emplace_back(view_ConstStockList3D(FecList, st));
    
    check_rank(MovementList[st], 5, "MovementList[[st]]");
    Movement.emplace_back(view_ConstStockList5D(MovementList, st));
    
  }
  
  // SRR Parameters
  if (!Misc.containsElementNamed("SRR_Pars")) {
    Rcpp::stop("Hist@Misc$SRR_Pars is missing");
  }
  Rcpp::List SRR_Pars_List = Misc["SRR_Pars"];
  check_size(SRR_Pars_List.size(), nStock, "SRR_Pars");
  
  if (!Misc.containsElementNamed("RecDevs")) {
    Rcpp::stop("Hist@RecDevs is missing");
  }
  Rcpp::List RecDevsList = Misc["RecDevs"];
  check_size(RecDevsList.size(), nStock, "RecDevs");
  
  if (!Misc.containsElementNamed("RecLag")) {
    Rcpp::stop("Hist@RecLag is missing");
  }
  Rcpp::IntegerVector RecLagVec = Misc["RecLag"];
  check_size(RecLagVec.size(), nStock, "RecLag");
  
  if (!Misc.containsElementNamed("SRR_Model")) {
    Rcpp::stop("Hist@SRR_Model is missing");
  }
  Rcpp::IntegerVector SRR_Model_Vec = Misc["SRR_Model"];
  check_size(SRR_Model_Vec.size(), nStock, "SRR_Model");
  
  SRR_Pars.reserve(nStock);
  RecDevs.reserve(nStock);
  SRR_Model.reserve(nStock);
  RecLag.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Rcpp::List pars_st = SRR_Pars_List[st];
    const int nPar = pars_st.size();
    std::vector<ConstArrayView2D> pars;
    pars.reserve(nPar);
    for (int p = 0; p < nPar; ++p) {
      check_rank(pars_st[p], 2, "SRR_Pars[[st]][[p]]"); // expect sim x yea
      ConstArrayView2D view = as_ConstArrayViewND<2>(pars_st[p]);
      pars.emplace_back(view);
    }
    SRR_Pars.emplace_back(std::move(pars));
    
    check_rank(RecDevsList[st], 2, "RecDevs[[st]]");
    RecDevs.emplace_back(as_ConstArrayViewND<2>(RecDevsList[st]));
    
    if (RecLagVec[st] < 0)
      Rcpp::stop("RecLag must be non-negative for stock " + std::to_string(st+1));
    RecLag.emplace_back(RecLagVec[st]);
    
    // SRR model
    const int model = SRR_Model_Vec[st];
    if (model < 0 || model > 2) {
      Rcpp::stop("Invalid SRR_Model for stock " + std::to_string(st+1) +
        " (got " + std::to_string(model) + ", expected 0, 1, or 2)");
    }
    
    if (model == 0 && pars_st.size() != 1) {
      Rcpp::stop("Beverton-Holt requires 1 SRR parameter (h) for stock " + std::to_string(st+1));
    }
    
    if (model == 1 && pars_st.size() != 1) {
      Rcpp::stop("Ricker requires 1 SRR parameter for stock " + std::to_string(st+1));
    }
    
    if (model == 2 && pars_st.size() != 1) {
      Rcpp::stop("Hockey-stick requires 1 SRR parameter (Shinge) for stock " + std::to_string(st+1));
    }
    SRR_Model.emplace_back(model);
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
  
  
  if (!Misc.containsElementNamed("maxF")) {
    Rcpp::stop("Hist@Misc$maxF is missing");
  }
  maxF = Rcpp::as<double>(Misc["maxF"]);
  if (!std::isfinite(maxF) || maxF < 0.0) {
    Rcpp::stop("Invalid maxF in Hist@Misc");
  }
  
  
} 



#endif
