#ifndef HIST_VIEW_H
#define HIST_VIEW_H

#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"

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
  bool StockTargetingFlag;
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
  
  HistView(Rcpp::S4& Hist, int nSim_, int nStock_, int nFleet_, int nArea_);
}; 


inline HistView::HistView(Rcpp::S4& Hist, int nSim_, int nStock_, int nFleet_, int nArea_)
  : nSim(nSim_), nStock(nStock_), nFleet(nFleet_), nArea(nArea_),
    
    SP0(              GetMisc_ConstArrayView<3>(Hist, "SP0")),
    R0(               GetMisc_ConstArrayView<3>(Hist, "R0")),
    RecDist(          GetMisc_ConstArrayView<4>(Hist, "RecDist")),
    SPFrom(           GetMisc_ConstArrayView<1>(Hist, "SPFrom")),
    PlusGroup(        GetMisc_ConstArrayView<1>(Hist, "PlusGroup")),
    RelSize(          GetMisc_ConstArrayView<2>(Hist, "RelSize")),
    SpawnTimeFrac(    GetMisc_ConstArrayView<2>(Hist, "SpawnTimeFrac")),
    q(                GetMisc_ConstArrayView<4>(Hist, "Catchability")),
    StockTargeting(   GetMisc_ConstArrayView<4>(Hist, "StockTargeting")),
    Closure(          GetMisc_ConstArrayView<5>(Hist, "Closure")),
    Spatial_Targeting(GetMisc_ConstArrayView<3>(Hist, "Spatial_Targeting"))
{
  // Mutable slots
  Biomass      = Slot2Array3D(Hist, "Biomass");
  SBiomass     = Slot2Array3D(Hist, "SBiomass");
  SProduction  = Slot2Array3D(Hist, "SProduction");
  Interactions = Slot2Array4D(Hist, "Interactions");
  Landings     = Slot2Array4D(Hist, "Landings");
  Discards     = Slot2Array4D(Hist, "Discards");
  Effort       = Slot2Array3D(Hist, "Effort");
  Distribution = Slot2Array4D(Hist, "Distribution");
  FInteract    = Slot2Array4D(Hist, "FInteract");
  FDead        = Slot2Array4D(Hist, "FDead");
  FRetain      = Slot2Array4D(Hist, "FRetain");
  
  const Rcpp::List Misc = Hist.slot("Misc");
  
  // Per-stock slot lists
  Rcpp::List NumberList        = Hist.slot("Number");
  Rcpp::List InteractAgeList   = Hist.slot("InteractAtAge");
  Rcpp::List LandAgeList       = Hist.slot("LandingsAtAge");
  Rcpp::List DiscAgeList       = Hist.slot("DiscardsAtAge");
  Rcpp::List LandSizeList      = Hist.slot("LandingsAtSize");
  Rcpp::List DiscSizeList      = Hist.slot("DiscardsAtSize");
  Rcpp::List FInteractAreaList = Hist.slot("FInteractArea");
  Rcpp::List FDeadAreaList     = Hist.slot("FDeadArea");
  Rcpp::List FRetainAreaList   = Hist.slot("FRetainArea");
  
  Number.reserve(nStock);
  InteractAtAge.reserve(nStock);  LandingsAtAge.reserve(nStock);
  DiscardsAtAge.reserve(nStock);  LandingsAtSize.reserve(nStock);
  DiscardsAtSize.reserve(nStock); FInteractArea.reserve(nStock);
  FDeadArea.reserve(nStock);      FRetainArea.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Number.emplace_back(       as_ArrayND<4>(NumberList[st]));
    InteractAtAge.emplace_back(as_ArrayND<5>(InteractAgeList[st]));
    LandingsAtAge.emplace_back(as_ArrayND<5>(LandAgeList[st]));
    DiscardsAtAge.emplace_back(as_ArrayND<5>(DiscAgeList[st]));
    FInteractArea.emplace_back(as_ArrayND<5>(FInteractAreaList[st]));
    FDeadArea.emplace_back(    as_ArrayND<5>(FDeadAreaList[st]));
    FRetainArea.emplace_back(  as_ArrayND<5>(FRetainAreaList[st]));
    
    Rcpp::List LS = LandSizeList[st];
    Rcpp::List DS = DiscSizeList[st];
    
    std::vector<Array4D> ls, ds;
    ls.reserve(nFleet); ds.reserve(nFleet);
    for (int fl = 0; fl < nFleet; ++fl) {
      ls.emplace_back(as_ArrayND<4>(LS[fl]));
      ds.emplace_back(as_ArrayND<4>(DS[fl]));
    } 
    LandingsAtSize.emplace_back(std::move(ls));
    DiscardsAtSize.emplace_back(std::move(ds));
  }
  
  // Stock Misc lists
  const Rcpp::List LengthList   = Misc["LengthList"];
  const Rcpp::List WeightList   = Misc["WeightList"];
  const Rcpp::List NatMList     = Misc["NaturalMortalityList"];
  const Rcpp::List MatList      = Misc["MaturityList"];
  const Rcpp::List SemList      = Misc["SemelparousList"];
  const Rcpp::List FecList      = Misc["FecundityList"];
  const Rcpp::List MovementList = Misc["MovementList"];
  
  Length.reserve(nStock);           Weight.reserve(nStock);
  NaturalMortality.reserve(nStock); Maturity.reserve(nStock);
  Semelparous.reserve(nStock);      Fecundity.reserve(nStock);
  Movement.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Length.emplace_back(          view_ConstStockList3D(LengthList,   st));
    Weight.emplace_back(          view_ConstStockList3D(WeightList,   st));
    NaturalMortality.emplace_back(view_ConstStockList3D(NatMList,     st));
    Maturity.emplace_back(        view_ConstStockList3D(MatList,      st));
    Semelparous.emplace_back(     view_ConstStockList3D(SemList,      st));
    Fecundity.emplace_back(       view_ConstStockList3D(FecList,      st));
    Movement.emplace_back(        view_ConstStockList5D(MovementList, st));
  }
  
  // SRR
  const Rcpp::List SRR_Pars_List          = Misc["SRR_Pars"];
  const Rcpp::List RecDevsList            = Misc["RecDevs"];
  const Rcpp::IntegerVector RecLagVec     = Misc["RecLag"];
  const Rcpp::IntegerVector SRR_Model_Vec = Misc["SRR_Model"];
  
  SRR_Pars.reserve(nStock); RecDevs.reserve(nStock);
  SRR_Model.reserve(nStock); RecLag.reserve(nStock);
  
  for (int st = 0; st < nStock; ++st) {
    Rcpp::List pars_st = SRR_Pars_List[st];
    const int nPar = pars_st.size();
    std::vector<ConstArrayView2D> pars;
    pars.reserve(nPar);
    for (int p = 0; p < nPar; ++p)
      pars.emplace_back(as_ConstArrayViewND<2>(pars_st[p]));
    SRR_Pars.emplace_back(std::move(pars));
    
    RecDevs.emplace_back(as_ConstArrayViewND<2>(RecDevsList[st]));
    RecLag.emplace_back(RecLagVec[st]);
    SRR_Model.emplace_back(SRR_Model_Vec[st]);
  }
  
  // Fleet misc
  const Rcpp::List WeightFleetList = Misc["WeightFleetList"];
  const Rcpp::List SelAgeList      = Misc["SelAgeList"];
  const Rcpp::List RetAgeList      = Misc["RetAgeList"];
  const Rcpp::List DiscMortList    = Misc["DiscMortList"];
  WeightFleet.reserve(nStock); 
  SelAge.reserve(     nStock);
  RetAge.reserve(     nStock);      
  DiscMort.reserve(   nStock);
  
  // const Rcpp::List SelSizeList     = Misc["SelSizeList"];
  // const Rcpp::List RetSizeList     = Misc["RetSizeList"];
  // SelSize.reserve(    nStock);     
  // RetSize.reserve(    nStock);
  
  for (int st = 0; st < nStock; ++st) {
    WeightFleet.emplace_back(view_ConstStockList4D(WeightFleetList, st));
    SelAge.emplace_back(     view_ConstStockList5D(SelAgeList,      st));
    RetAge.emplace_back(     view_ConstStockList5D(RetAgeList,      st));
    DiscMort.emplace_back(   view_ConstStockList5D(DiscMortList,    st));
    
    // const Rcpp::List SS = SelSizeList[st];
    // const Rcpp::List RS = RetSizeList[st];
    
    // std::vector<ConstArrayView4D> ss, rs;
    // ss.reserve(nFleet); rs.reserve(nFleet);
    // for (int fl = 0; fl < nFleet; ++fl) {
    //   ss.emplace_back(as_ConstArrayViewND<4>(SS[fl]));
    //   rs.emplace_back(as_ConstArrayViewND<4>(RS[fl]));
    // }
    // SelSize.emplace_back(std::move(ss));
    // RetSize.emplace_back(std::move(rs));
  }
  
  maxF = Rcpp::as<double>(Misc["maxF"]);
  
  UseDensity         = Rcpp::as<std::vector<bool>>(Misc["Mode"]);
  StockTargetingFlag = Rcpp::as<bool>(Misc["StockTargetingFlag"]);
}

#endif