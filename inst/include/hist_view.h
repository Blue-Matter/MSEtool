#ifndef HIST_VIEW_H
#define HIST_VIEW_H

#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"

inline void check_rank(SEXP x, int expected, const char* name) {
  if (!Rf_isNumeric(x) && !Rf_isInteger(x))
    Rcpp::stop(std::string(name) + " must be numeric");
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (Rf_isNull(dim) || Rf_length(dim) != expected)
    Rcpp::stop(std::string(name) + " must be a " + std::to_string(expected) + "D array");
}

inline void check_size(int got, int expected, const char* name) {
  if (got != expected)
    Rcpp::stop(std::string(name) + " has wrong size (" +
      std::to_string(got) + " != " + std::to_string(expected) + ")");
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
  require_misc("Spatial_Targeting", 3);
}

struct HistView {
  
  const int nSim, nStock, nFleet, nArea;
  
  // Read-only Misc views 
  ConstArrayView3D SP0;
  ConstArrayView3D R0;
  ConstArrayView4D RecDist;
  ConstArrayView1D SPFrom;
  ConstArrayView1D PlusGroup;
  ConstArrayView2D RelSize;
  ConstArrayView2D SpawnTimeFrac;
  ConstArrayView4D q;
  ConstArrayView4D StockTargeting;
  ConstArrayView5D Closure;
  ConstArrayView3D Spatial_Targeting;
  
  std::vector<bool> UseDensity;
  
  // Mutable slots 
  std::vector<Array4D> Number;
  Array3D Biomass, SBiomass, SProduction;
  Array4D Interactions, Landings, Discards;
  std::vector<Array5D> InteractAtAge, LandingsAtAge, DiscardsAtAge;
  std::vector<std::vector<Array4D>> LandingsAtSize, DiscardsAtSize;
  Array3D Effort;
  Array4D Distribution;
  Array4D FInteract, FDead, FRetain;
  std::vector<Array5D> FInteractArea, FDeadArea, FRetainArea;
  
  // Read-only stock arrays 
  std::vector<ConstArrayView3D> Length, Weight, NaturalMortality;
  std::vector<ConstArrayView3D> Maturity, Semelparous, Fecundity;
  std::vector<ConstArrayView5D> Movement;

  std::vector<std::vector<ConstArrayView2D>> SRR_Pars;
  std::vector<ConstArrayView2D> RecDevs;
  std::vector<int> RecLag, SRR_Model;

  std::vector<ConstArrayView4D> WeightFleet;
  std::vector<ConstArrayView5D> SelAge, RetAge, DiscMort;
  std::vector<std::vector<ConstArrayView4D>> SelSize, RetSize;

  double maxF;

public:
  HistView(Rcpp::S4& Hist, int nSim_, int nStock_, int nFleet_, int nArea_);

private:
  HistView(std::nullptr_t, Rcpp::S4& Hist, int nSim_, int nStock_, int nFleet_, int nArea_);
}; 

inline HistView::HistView(Rcpp::S4& Hist, int nSim_, int nStock_, int nFleet_, int nArea_)
  : HistView(nullptr,
    ([&]() -> Rcpp::S4& { 
      validate_Hist_misc(Hist);
      check_rank(Hist.slot("Biomass"),     3, "Biomass");
      check_rank(Hist.slot("SBiomass"),    3, "SBiomass");
      check_rank(Hist.slot("SProduction"), 3, "SProduction");
      check_rank(Hist.slot("Interactions"),4, "Interactions");
      check_rank(Hist.slot("Landings"),    4, "Landings");
      check_rank(Hist.slot("Discards"),    4, "Discards");
      check_rank(Hist.slot("FInteract"),   4, "FInteract");
      check_rank(Hist.slot("FDead"),       4, "FDead");
      check_rank(Hist.slot("FRetain"),     4, "FRetain");
      check_rank(Hist.slot("Effort"),      3, "Effort");
      check_rank(Hist.slot("Distribution"),4, "Distribution");
      return Hist;
    })(),
    nSim_, nStock_, nFleet_, nArea_)
{}

inline HistView::HistView(std::nullptr_t, Rcpp::S4& Hist,
                          int nSim_, int nStock_, int nFleet_, int nArea_)
  : nSim(nSim_), nStock(nStock_), nFleet(nFleet_), nArea(nArea_),
    
    // ConstArrayViewND  
    SP0(              GetMisc_ConstArrayView<3>(Hist, "SP0")),
    R0(               GetMisc_ConstArrayView<3>(Hist, "R0")),
    RecDist(          GetMisc_ConstArrayView<4>(Hist, "RecDist")),
    SPFrom(           GetMisc_ConstArrayView<1>(Hist, "SPFrom")),
    PlusGroup(        GetMisc_ConstArrayView<1>(Hist, "PlusGroup")),
    RelSize(          GetMisc_ConstArrayView<2>(Hist, "RelSize")),
    SpawnTimeFrac(    GetMisc_ConstArrayView<2>(Hist, "SpawnTimeFrac")),
    q(                GetMisc_ConstArrayView<4>(Hist, "Catchability")),
    StockTargeting(                GetMisc_ConstArrayView<4>(Hist, "StockTargeting")),
    Closure(          GetMisc_ConstArrayView<5>(Hist, "Closure")),
    Spatial_Targeting(GetMisc_ConstArrayView<3>(Hist, "Spatial_Targeting"))
{
  // Mutable slots
  Biomass     = Slot2Array3D(Hist, "Biomass");
  SBiomass    = Slot2Array3D(Hist, "SBiomass");
  SProduction = Slot2Array3D(Hist, "SProduction");
  Interactions= Slot2Array4D(Hist, "Interactions");
  Landings    = Slot2Array4D(Hist, "Landings");
  Discards    = Slot2Array4D(Hist, "Discards");
  Effort      = Slot2Array3D(Hist, "Effort");
  Distribution= Slot2Array4D(Hist, "Distribution");
  FInteract   = Slot2Array4D(Hist, "FInteract");
  FDead       = Slot2Array4D(Hist, "FDead");
  FRetain     = Slot2Array4D(Hist, "FRetain");
   
  const Rcpp::List Misc = Hist.slot("Misc");
   
  if (Biomass.dim[0] != nSim)   Rcpp::stop("Biomass nSim mismatch");
  if (RecDist.dim[1] != nStock) Rcpp::stop("RecDist stock dimension mismatch");
  if (RecDist.dim[3] != nArea)  Rcpp::stop("RecDist area dimension mismatch");
  if (SP0.dim[1] != nStock)     Rcpp::stop("SP0 stock dimension mismatch");
  if (R0.dim[1] != nStock)      Rcpp::stop("R0 stock dimension mismatch");
   
  Rcpp::List NumberList        = Hist.slot("Number");
  Rcpp::List InteractAgeList   = Hist.slot("InteractAtAge");
  Rcpp::List LandAgeList       = Hist.slot("LandingsAtAge");
  Rcpp::List DiscAgeList       = Hist.slot("DiscardsAtAge");
  Rcpp::List LandSizeList      = Hist.slot("LandingsAtSize");
  Rcpp::List DiscSizeList      = Hist.slot("DiscardsAtSize");
  Rcpp::List FInteractAreaList = Hist.slot("FInteractArea");
  Rcpp::List FDeadAreaList     = Hist.slot("FDeadArea");
  Rcpp::List FRetainAreaList   = Hist.slot("FRetainArea");
   
  check_size(NumberList.size(),      nStock, "Number");
  check_size(InteractAgeList.size(), nStock, "InteractAtAge");
  check_size(LandAgeList.size(),     nStock, "LandingsAtAge");
  check_size(DiscAgeList.size(),     nStock, "DiscardsAtAge");
   
  Number.reserve(nStock);
  InteractAtAge.reserve(nStock);  LandingsAtAge.reserve(nStock);
  DiscardsAtAge.reserve(nStock);  LandingsAtSize.reserve(nStock);
  DiscardsAtSize.reserve(nStock); FInteractArea.reserve(nStock);
  FDeadArea.reserve(nStock);      FRetainArea.reserve(nStock);
   
  for (int st = 0; st < nStock; ++st) {
    check_rank(NumberList[st], 4, "Number");
    Number.emplace_back(       as_ArrayND<4>(NumberList[st],        "Number"));
    InteractAtAge.emplace_back(as_ArrayND<5>(InteractAgeList[st],   "InteractAtAge"));
    LandingsAtAge.emplace_back(as_ArrayND<5>(LandAgeList[st],       "LandingsAtAge"));
    DiscardsAtAge.emplace_back(as_ArrayND<5>(DiscAgeList[st],       "DiscardsAtAge"));
    FInteractArea.emplace_back(as_ArrayND<5>(FInteractAreaList[st], "FInteractArea"));
    FDeadArea.emplace_back(    as_ArrayND<5>(FDeadAreaList[st],     "FDeadArea"));
    FRetainArea.emplace_back(  as_ArrayND<5>(FRetainAreaList[st],   "FRetainArea"));
     
    Rcpp::List LS = LandSizeList[st];
    Rcpp::List DS = DiscSizeList[st];
    check_size(LS.size(), nFleet, "LandingsAtSize[[st]]");
    check_size(DS.size(), nFleet, "DiscardsAtSize[[st]]");
     
    std::vector<Array4D> ls, ds;
    ls.reserve(nFleet); ds.reserve(nFleet);
    for (int fl = 0; fl < nFleet; ++fl) {
      ls.emplace_back(as_ArrayND<4>(LS[fl], "LandingsAtSize"));
      ds.emplace_back(as_ArrayND<4>(DS[fl], "DiscardsAtSize"));
    } 
    LandingsAtSize.emplace_back(std::move(ls));
    DiscardsAtSize.emplace_back(std::move(ds));
  }
   
  // Stock Misc lists — ConstArrayViewND anchors each element internally
  auto require_stock_list = [&](const char* nm) {
    if (!Misc.containsElementNamed(nm))
      Rcpp::stop(std::string("Hist@Misc$") + nm + " is missing");
    Rcpp::List L = Misc[nm];
    check_size(L.size(), nStock, nm);
    return L;
  }; 
  
  Rcpp::List LengthList   = require_stock_list("LengthList");
  Rcpp::List WeightList   = require_stock_list("WeightList");
  Rcpp::List NatMList     = require_stock_list("NaturalMortalityList");
  Rcpp::List MatList      = require_stock_list("MaturityList");
  Rcpp::List SemList      = require_stock_list("SemelparousList");
  Rcpp::List FecList      = require_stock_list("FecundityList");
  Rcpp::List MovementList = require_stock_list("MovementList");
  
  Length.reserve(nStock);  Weight.reserve(nStock);
  NaturalMortality.reserve(nStock); Maturity.reserve(nStock);
  Semelparous.reserve(nStock); Fecundity.reserve(nStock);
  Movement.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    check_rank(LengthList[st],   3, "LengthList[[st]]");
    check_rank(WeightList[st],   3, "WeightList[[st]]");
    check_rank(NatMList[st],     3, "NatMList[[st]]");
    check_rank(MatList[st],      3, "MatList[[st]]");
    check_rank(SemList[st],      3, "SemList[[st]]");
    check_rank(FecList[st],      3, "FecList[[st]]");
    check_rank(MovementList[st], 5, "MovementList[[st]]");
    
    // view_ConstStockListND calls as_ConstArrayViewND(SEXP) which anchors internally
    Length.emplace_back(          view_ConstStockList3D(LengthList,   st));
    Weight.emplace_back(          view_ConstStockList3D(WeightList,   st));
    NaturalMortality.emplace_back(view_ConstStockList3D(NatMList,     st));
    Maturity.emplace_back(        view_ConstStockList3D(MatList,      st));
    Semelparous.emplace_back(     view_ConstStockList3D(SemList,      st));
    Fecundity.emplace_back(       view_ConstStockList3D(FecList,      st));
    Movement.emplace_back(        view_ConstStockList5D(MovementList, st));
  }
  
  // SRR
  if (!Misc.containsElementNamed("SRR_Pars"))  Rcpp::stop("Hist@Misc$SRR_Pars missing");
  if (!Misc.containsElementNamed("RecDevs"))   Rcpp::stop("Hist@Misc$RecDevs missing");
  if (!Misc.containsElementNamed("RecLag"))    Rcpp::stop("Hist@Misc$RecLag missing");
  if (!Misc.containsElementNamed("SRR_Model")) Rcpp::stop("Hist@Misc$SRR_Model missing");
  
  Rcpp::List SRR_Pars_List   = Misc["SRR_Pars"];
  Rcpp::List RecDevsList     = Misc["RecDevs"];
  Rcpp::IntegerVector RecLagVec     = Misc["RecLag"];
  Rcpp::IntegerVector SRR_Model_Vec = Misc["SRR_Model"];
  
  check_size(SRR_Pars_List.size(), nStock, "SRR_Pars");
  check_size(RecDevsList.size(),   nStock, "RecDevs");
  check_size(RecLagVec.size(),     nStock, "RecLag");
  check_size(SRR_Model_Vec.size(), nStock, "SRR_Model");
  
  SRR_Pars.reserve(nStock); RecDevs.reserve(nStock);
  SRR_Model.reserve(nStock); RecLag.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Rcpp::List pars_st = SRR_Pars_List[st];
    const int nPar = pars_st.size();
    std::vector<ConstArrayView2D> pars;
    pars.reserve(nPar);
    for (int p = 0; p < nPar; ++p) {
      check_rank(pars_st[p], 2, "SRR_Pars[[st]][[p]]");
      pars.emplace_back(as_ConstArrayViewND<2>(pars_st[p], "SRR_Pars"));
    }
    SRR_Pars.emplace_back(std::move(pars));
    
    check_rank(RecDevsList[st], 2, "RecDevs[[st]]");
    RecDevs.emplace_back(as_ConstArrayViewND<2>(RecDevsList[st], "RecDevs"));
    
    if (RecLagVec[st] < 0)
      Rcpp::stop("RecLag must be non-negative for stock " + std::to_string(st+1));
    RecLag.emplace_back(RecLagVec[st]);
    
    const int model = SRR_Model_Vec[st];
    if (model < 0 || model > 2)
      Rcpp::stop("Invalid SRR_Model for stock " + std::to_string(st+1));
    if (pars_st.size() != 1)
      Rcpp::stop("SRR requires exactly 1 parameter array for stock " + std::to_string(st+1));
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
  check_size(SelAgeList.size(),      nStock, "SelAgeList");
  
  WeightFleet.reserve(nStock); SelAge.reserve(nStock);
  RetAge.reserve(nStock);      DiscMort.reserve(nStock);
  SelSize.reserve(nStock);     RetSize.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    WeightFleet.emplace_back(view_ConstStockList4D(WeightFleetList, st));
    SelAge.emplace_back(     view_ConstStockList5D(SelAgeList,      st));
    RetAge.emplace_back(     view_ConstStockList5D(RetAgeList,      st));
    DiscMort.emplace_back(   view_ConstStockList5D(DiscMortList,    st));
    
    Rcpp::List SS = SelSizeList[st];
    Rcpp::List RS = RetSizeList[st];
    check_size(SS.size(), nFleet, "SelSize[[st]]");
    check_size(RS.size(), nFleet, "RetSize[[st]]");
    
    std::vector<ConstArrayView4D> ss, rs;
    ss.reserve(nFleet); rs.reserve(nFleet);
    for (int fl = 0; fl < nFleet; ++fl) {
      ss.emplace_back(as_ConstArrayViewND<4>(SS[fl], "SelSize"));
      rs.emplace_back(as_ConstArrayViewND<4>(RS[fl], "RetSize"));
    }
    SelSize.emplace_back(std::move(ss));
    RetSize.emplace_back(std::move(rs));
  }
  
  if (!Misc.containsElementNamed("maxF")) Rcpp::stop("Hist@Misc$maxF is missing");
  maxF = Rcpp::as<double>(Misc["maxF"]);
  if (!std::isfinite(maxF) || maxF < 0.0) Rcpp::stop("Invalid maxF in Hist@Misc");
  
  
  Rcpp::LogicalVector modeVec = Misc["Mode"];
  UseDensity = Rcpp::as<std::vector<bool>>(modeVec);
  
  
}

#endif