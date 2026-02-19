#ifndef CALC_APPLYMAXF_H
#define CALC_APPLYMAXF_H

#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "hist_view.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"
#include "calc_spawn_production.h"
#include "calc_recruitment.h"
#include "calc_number_next.h"
#include "calc_biomass.h"
#include "calc_catch.h"
#include "calc_overall_f.h"

// Scales effort for a fleet where the maxF constraint is applied to at least one stock

inline void ApplyMaxF(const int y,
                      const std::vector<int>& Sims,
                      const int nSim,
                      std::vector<Array5D>& FInteractArea,              // [stock] sim, age, year, fleet, area
                      std::vector<Array5D>& FDeadArea,                  // [stock] sim, age, year, fleet, area
                      std::vector<Array5D>& FRetainArea,                // [stock] sim, age, year, fleet, area
                      const std::vector<ConstArrayView5D>& SelAge,      // [stock] sim, age, year, fleet, area
                      const std::vector<ConstArrayView5D>& RetAge,      // [stock] sim, age, year, fleet, area
                      const std::vector<ConstArrayView5D>& DiscMort,    // [stock] sim, age, year, fleet, area
                      const Array4D& Distribution,                      // sim, year, fleet, area
                      const ConstArrayView4D& q,                        // sim, stock, year, fleet          
                      Array3D& Effort,                                  // sim, year, fleet
                      const ConstArrayView2D& RelSize,                  // sim, area
                      
                      std::vector<Array5D>& InteractAtAge,                    // [stock] sim, age, year, fleet, area
                      std::vector<Array5D>& LandingsAtAge,                    // [stock] sim, age, year, fleet, area
                      std::vector<Array5D>& DiscardsAtAge,                    // [stock] sim, age, year, fleet, area
                      Array4D& Interactions,                                  // sim, stock, year, fleet
                      Array4D& Landings,                                      // sim, stock, year, fleet
                      Array4D& Discards,                                      // sim, stock, year, fleet

                      const std::vector<ConstArrayView3D> NaturalMortality,   // [stock] sim, age, year
                      const std::vector<Array4D>& Number,                     // [stock] sim, age, year, area
                      const std::vector<ConstArrayView4D>& FleetWeight,       // [stock] sim, age, year, fleet
                      
                      Array4D& FInteract,                                     // sim, stock, year, fleet
                      Array4D& FDead,                                         // sim, stock, year, fleet
                      Array4D& FRetain,                                       // sim, stock, year, fleet
                      
                      const double maxF, 
                      const int nStock,
                      const int nFleet,
                      const int nArea,
                      const int DoCalcCatch=1,        // calculate catch?
                      const int debug = 0
                      
) {
  
  // track which simulations need recomputation
  std::vector<bool> recomputeSim(nSim, false);
  
  
  for (size_t  sim_idx = 0; sim_idx < Sims.size(); ++sim_idx) {
    int sim = Sims[sim_idx];
    
    double worst_ratio = 1.0;
    
    // Find most restrictive stock
    for (int st = 0; st < nStock; ++st) {
      
      double Fglob = 0;
      
      for (int fl = 0; fl < nFleet; ++fl) {
        Fglob += FInteract(sim, st, y, fl);
      }
    
      if (maxF > 0.0 && Fglob > 0.0) {
        double ratio = Fglob / maxF;
        if (ratio > worst_ratio)
          worst_ratio = ratio;
      }
    } 
    
    // If constraint violated, mark for recompute
    if (worst_ratio > 1.0) {
      double scale = 1.0 / worst_ratio;
      
      if (debug)
        Rcpp::Rcout << "Sim " << sim+1 << ": Scaling effort by " << scale << "\n";
      
      // Scale fleet effort
      for (int fl = 0; fl < nFleet; ++fl)
        Effort(sim, y, fl) *= scale;
      
      recomputeSim[sim_idx] = true;
    } 
  }
  
  // Recompute only for simulations that were scaled
  for (size_t  sim_idx = 0; sim_idx < Sims.size(); ++sim_idx) {
    
    if (!recomputeSim[sim_idx]) continue;
    
    int sim = Sims[sim_idx];
    
    // Recompute area Fs
    CalcArea_F(y,
               {sim},       
               nSim,          
               FInteractArea,
               FDeadArea,
               FRetainArea,
               SelAge,
               RetAge,
               DiscMort,
               Distribution,
               q,
               Effort,
               RelSize,
               nStock,
               nFleet,
               nArea);
    
    // Recompute catch if needed
    if (DoCalcCatch) {
      CalcCatch(y,
                {sim},  
                nSim,
                InteractAtAge,
                LandingsAtAge,
                DiscardsAtAge,
                Interactions,
                Landings,
                Discards,
                FInteractArea,
                FDeadArea,
                FRetainArea,
                NaturalMortality,
                Number,
                FleetWeight,
                nStock,
                nFleet,
                nArea);
      
    }
    
    // Recompute overall F for this sim
    CalcOverallF(y,
                 {sim},
                 nSim,
                 FInteract,
                 FDead,
                 FRetain,
                 InteractAtAge,
                 LandingsAtAge,
                 DiscardsAtAge,
                 Number,
                 nStock,
                 nFleet,
                 nArea);
    
  }
}
  
#endif