#ifndef EXTRACT_SIM_STATE_H
#define EXTRACT_SIM_STATE_H

#include <Rcpp.h>
#include "fishery_sim_state.h"
#include "helpers.h"



// extract a single sim state from Hist
inline FisherySimState extract_sim_state(
    Rcpp::S4& Hist,
    int sim,
    int nStock,
    int nFleet,
    int nArea
) {
  
  FisherySimState st(nStock, nFleet);
  
  // -------------------------
  // Top-level mutable arrays
  // -------------------------
   
  st.Biomass      = slice_sim(Slot2Array3D(Hist, "Biomass"), sim);
  st.SBiomass     = slice_sim(Slot2Array3D(Hist, "SBiomass"), sim);
  st.SProduction  = slice_sim(Slot2Array3D(Hist, "SProduction"), sim);
   
  st.Effort       = slice_sim(Slot2Array3D(Hist, "Effort"), sim);
  st.Distribution = slice_sim(Slot2Array4D(Hist, "Distribution"), sim);
   
  // -------------------------
  // Stock-level mutable lists
  // -------------------------
   
  const Rcpp::List NumberList        = Hist.slot("Number");
  const Rcpp::List FDeadList         = Hist.slot("FDead");
  const Rcpp::List FRetainList       = Hist.slot("FRetain");
  const Rcpp::List FDeadAreaList     = Hist.slot("FDeadArea");
  const Rcpp::List FRetainAreaList   = Hist.slot("FRetainArea");
  const Rcpp::List LandAAList        = Hist.slot("LandingsAtAge");
  const Rcpp::List DiscAAList        = Hist.slot("DiscardsAtAge");
  const Rcpp::List LandSizeList      = Hist.slot("LandingsAtSize");
  const Rcpp::List DiscSizeList      = Hist.slot("DiscardsAtSize");
   
  st.Number.reserve(nStock);
  st.FDead.reserve(nStock);
  st.FRetain.reserve(nStock);
  st.FDeadArea.reserve(nStock);
  st.FRetainArea.reserve(nStock);
  st.LandingsAtAge.reserve(nStock);
  st.DiscardsAtAge.reserve(nStock);
  st.LandingsAtSize.reserve(nStock);
  st.DiscardsAtSize.reserve(nStock);

  for (int st_i = 0; st_i < nStock; ++st_i) {
     
    st.Number.emplace_back(slice_sim(as_ArrayND<4>(NumberList[st_i]), sim));
    st.FDead.emplace_back(slice_sim(as_ArrayND<4>(FDeadList[st_i]), sim));
    st.FRetain.emplace_back(slice_sim(as_ArrayND<4>(FRetainList[st_i]), sim));
    st.FDeadArea.emplace_back(slice_sim(as_ArrayND<5>(FDeadAreaList[st_i]), sim));
    st.FRetainArea.emplace_back(slice_sim(as_ArrayND<5>(FRetainAreaList[st_i]), sim));
     
    st.LandingsAtAge.emplace_back(slice_sim(as_ArrayND<5>(LandAAList[st_i]), sim));
    st.DiscardsAtAge.emplace_back(slice_sim(as_ArrayND<5>(DiscAAList[st_i]), sim));
     
    // --- size comps ---
    Rcpp::List Lsz = LandSizeList[st_i];
    Rcpp::List Dsz = DiscSizeList[st_i];

    if (Lsz.size() != nFleet || Dsz.size() != nFleet) {
      Rcpp::stop("Size comps: wrong fleet dim (stock %d)", st_i + 1);
    }

    if (Lsz.size() != nFleet)
      Rcpp::stop("LandingsAtSize: wrong fleet dim (stock %d)", st_i + 1);
    
    if (Dsz.size() != nFleet)
      Rcpp::stop("DiscardsAtSize: wrong fleet dim (stock %d)", st_i + 1);
    
    std::vector<ArrayND<3>> Lsz_f;
    std::vector<ArrayND<3>> Dsz_f;
    Lsz_f.reserve(nFleet);
    Dsz_f.reserve(nFleet);
    
    for (int fl = 0; fl < nFleet; ++fl) {
      Lsz_f.emplace_back(slice_sim(as_ArrayND<4>(Lsz[fl]), sim));
      Dsz_f.emplace_back(slice_sim(as_ArrayND<4>(Dsz[fl]), sim));
    }
    
    st.LandingsAtSize[st_i] = std::move(Lsz_f);
    st.DiscardsAtSize[st_i] = std::move(Dsz_f);
  } 
  
  // -------------------------
  // Const biological objects
  // -------------------------
  const Rcpp::List Misc = Hist.slot("Misc");
  
  st.SPFrom        = as_ConstArrayViewND<1>(Misc["SPFrom"]);
  st.RelSize       = slice_sim_view_broadcast(as_ConstArrayViewND<2>(Misc["RelSize"]), sim);
  st.SpawnTimeFrac = slice_sim_view_broadcast(as_ConstArrayViewND<2>(Misc["SpawnTimeFrac"]), sim);

  st.q         = slice_sim_view_broadcast(as_ConstArrayViewND<4>(Misc["Catchability"]), sim);
  st.Closure   = slice_sim_view_broadcast(as_ConstArrayViewND<5>(Misc["Closure"]), sim);
  st.Targeting = slice_sim_view_broadcast(as_ConstArrayViewND<3>(Misc["Targeting"]), sim);

  const Rcpp::List LengthList = Misc["LengthList"];
  const Rcpp::List WeightList = Misc["WeightList"];
  const Rcpp::List NatMList   = Misc["NaturalMortalityList"];
  const Rcpp::List MatList    = Misc["MaturityList"];
  const Rcpp::List SemList    = Misc["SemelparousList"];
  const Rcpp::List FecList    = Misc["FecundityList"];
   
  const Rcpp::List WFList   = Misc["WeightFleetList"];
  const Rcpp::List SelAList = Misc["SelAgeList"];
  const Rcpp::List SelSList = Misc["SelSizeList"];
  const Rcpp::List RetAList = Misc["RetAgeList"];
  const Rcpp::List RetSList = Misc["RetSizeList"];
  const Rcpp::List DMList   = Misc["DiscMortList"];
  
  for (int st_i = 0; st_i < nStock; ++st_i) {
    
    st.Length.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(LengthList[st_i]), sim));
    st.Weight.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(WeightList[st_i]), sim));
    st.NaturalMortality.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(NatMList[st_i]), sim));
    st.Maturity.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(MatList[st_i]), sim));
    st.Semelparous.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(SemList[st_i]), sim));
    st.Fecundity.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<3>(FecList[st_i]), sim));
  
    st.WeightFleet.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<4>(WFList[st_i]), sim));
    st.SelAge.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<5>(SelAList[st_i]), sim));
    st.RetAge.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<5>(RetAList[st_i]), sim));
    st.DiscMort.emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<5>(DMList[st_i]), sim));

    const Rcpp::List SelS_st = SelSList[st_i];
    const Rcpp::List RetS_st = RetSList[st_i];
    
    if (SelS_st.size() != nFleet || RetS_st.size() != nFleet) {
      Rcpp::stop("SelSize/RetSize: wrong fleet dim (stock %d)", st_i + 1);
    }
   
    for (int fl = 0; fl < nFleet; ++fl) {
      st.SelSize[st_i].emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<4>(SelS_st[fl]), sim));
      st.RetSize[st_i].emplace_back(slice_sim_view_broadcast(as_ConstArrayViewND<4>(RetS_st[fl]), sim));
    }

  }

   
  return st;
}


 
#endif
