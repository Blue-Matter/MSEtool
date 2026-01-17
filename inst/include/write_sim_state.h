#ifndef WRITE_SIM_STATE_H
#define WRITE_SIM_STATE_H

#include <Rcpp.h>
#include "fishery_sim_state.h"
#include "array_nd.h"
#include "array_views.h"
#include "helpers.h"

inline void write_sim_state(
    Rcpp::S4& Hist,
    const FisherySimState& st,
    int sim,
    int nStock,
    int nFleet
) {
  
  // -------------------------
  // Top-level arrays
  // -------------------------
  
  Array3D Biomass = Slot2Array3D(Hist, "Biomass");
  Biomass.set_slice(sim, st.Biomass);
  
  Array3D SBiomass = Slot2Array3D(Hist, "SBiomass");
  SBiomass.set_slice(sim, st.SBiomass);
  
  Array3D SProduction = Slot2Array3D(Hist, "SProduction");
  SProduction.set_slice(sim, st.SProduction);
  
  Array3D Effort = Slot2Array3D(Hist, "Effort");
  Effort.set_slice(sim, st.Effort);
  
  Array4D Distribution = Slot2Array4D(Hist, "Distribution");
  Distribution.set_slice(sim, st.Distribution);
  
  // -------------------------
  // Stock-level lists
  // -------------------------
   
  Rcpp::List NumberList      = Hist.slot("Number");
  Rcpp::List FDeadList       = Hist.slot("FDead");
  Rcpp::List FRetainList     = Hist.slot("FRetain");
  Rcpp::List FDeadAreaList   = Hist.slot("FDeadArea");
  Rcpp::List FRetainAreaList = Hist.slot("FRetainArea");
  Rcpp::List LandAAList      = Hist.slot("LandingsAtAge");
  Rcpp::List DiscAAList      = Hist.slot("DiscardsAtAge");
  Rcpp::List LandSizeList    = Hist.slot("LandingsAtSize");
  Rcpp::List DiscSizeList    = Hist.slot("DiscardsAtSize");
   
   for (int st_i = 0; st_i < nStock; ++st_i) {
     auto NumberA = as_ArrayND<4>(NumberList[st_i]);
     set_slice_age(NumberA, sim, st.Number[st_i]);       // [stock] sim, age, year, area
     
     auto FDeadA = as_ArrayND<4>(FDeadList[st_i]);
     set_slice_age(FDeadA, sim, st.FDead[st_i]);         // [stock] sim, age, year, area
     
     auto FRetainA = as_ArrayND<4>(FRetainList[st_i]);
     set_slice_age(FRetainA, sim, st.FRetain[st_i]);     // [stock] sim, age, year, area
     
     auto FDeadAreaA = as_ArrayND<5>(FDeadAreaList[st_i]);
     set_slice_age_fleet(FDeadAreaA, sim, st.FDeadArea[st_i]);    // [stock] sim, age, year, fleet, area
     
     auto FRetainAreaA = as_ArrayND<5>(FRetainAreaList[st_i]);
     set_slice_age_fleet(FRetainAreaA, sim, st.FRetainArea[st_i]);    // [stock] sim, age, year, fleet, area
     
     auto LandAAA = as_ArrayND<5>(LandAAList[st_i]);
     set_slice_age_fleet(LandAAA, sim, st.LandingsAtAge[st_i]);    // [stock] sim, age, year, fleet, area
     
     auto DiscAAA = as_ArrayND<5>(DiscAAList[st_i]);
     set_slice_age_fleet(DiscAAA, sim, st.DiscardsAtAge[st_i]);    // [stock] sim, age, year, fleet, area
     
     Rcpp::List Lsz = LandSizeList[st_i];
     Rcpp::List Dsz = DiscSizeList[st_i];
     
     for (int fl = 0; fl < nFleet; ++fl) {
       auto Lsz_f = as_ArrayND<4>(Lsz[fl]);
       auto Dsz_f = as_ArrayND<4>(Dsz[fl]);
       set_slice_length(Lsz_f, sim, st.LandingsAtSize[st_i][fl]);   // ArrayND<3>: length, year, area
       set_slice_length(Dsz_f, sim, st.DiscardsAtSize[st_i][fl]);
     }
   }
}

#endif
