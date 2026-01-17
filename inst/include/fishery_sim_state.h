#ifndef FISHERY_SIM_STATE_H
#define FISHERY_SIM_STATE_H

#include "array_types.h"
#include "array_views.h"
#include "array_nd.h"

// structure for holding sim-specific views

struct FisherySimState {
  
  FisherySimState(int nStock, int nFleet) {
    Number.reserve(nStock);
    FDead.reserve(nStock);
    FRetain.reserve(nStock);
    FDeadArea.reserve(nStock);
    FRetainArea.reserve(nStock);
     
    LandingsAtAge.reserve(nStock);
    DiscardsAtAge.reserve(nStock);
     
    LandingsAtSize.resize(nStock);
    DiscardsAtSize.resize(nStock);
    for (int st = 0; st < nStock; ++st) {
      LandingsAtSize[st].reserve(nFleet);
      DiscardsAtSize[st].reserve(nFleet);
    } 
    
    Length.reserve(nStock);
    Weight.reserve(nStock);
    NaturalMortality.reserve(nStock);
    Maturity.reserve(nStock);
    Semelparous.reserve(nStock);
    Fecundity.reserve(nStock);
     
    WeightFleet.reserve(nStock);
    SelAge.reserve(nStock);
    RetAge.reserve(nStock);
    DiscMort.reserve(nStock);
     
    SelSize.resize(nStock);
    RetSize.resize(nStock);
    for (int st = 0; st < nStock; ++st) {
      SelSize[st].reserve(nFleet);
      RetSize[st].reserve(nFleet);
    } 
  }
  
  // -----------------
  // Mutable state
  // -----------------
  
  Array2D Biomass;        // stock, year
  Array2D SBiomass;       // stock, year
  Array2D SProduction;   // stock, year

  std::vector<Array3D> Number;            // [stock] age, year, area
  std::vector<Array3D> FDead;             // [stock] age, year, fleet
  std::vector<Array3D> FRetain;           // [stock] age, year, fleet
  std::vector<Array4D> FDeadArea;         // [stock] age, year, fleet, area
  std::vector<Array4D> FRetainArea;       // [stock] age, year, fleet, area

  std::vector<Array4D> LandingsAtAge;     // [stock] age, year, fleet, area
  std::vector<Array4D> DiscardsAtAge;     // [stock] age, year, fleet, area
 
  std::vector<std::vector<Array3D>> LandingsAtSize;   // [stock][fleet] class, year, area
  std::vector<std::vector<Array3D>> DiscardsAtSize;  // [stock][fleet] class, year, area
  
  Array2D Effort;        // year, fleet
  Array3D Distribution; // year, fleet, area
  
  // -----------------
  // Const (biology & fleet)
  // -----------------
  
  std::vector<ConstArrayView2D> Length;             // [stock] age, year
  std::vector<ConstArrayView2D> Weight;             // [stock] age, year
  std::vector<ConstArrayView2D> NaturalMortality;   // [stock] age, year
  std::vector<ConstArrayView2D> Maturity;           // [stock] age, year
  std::vector<ConstArrayView2D> Semelparous;        // [stock] age, year
  std::vector<ConstArrayView2D> Fecundity;          // [stock] age, year
  
  std::vector<ConstArrayView3D> WeightFleet;        // [stock] age, year, fleet
  std::vector<ConstArrayView4D> SelAge;             // [stock] age, year, fleet, area
  std::vector<ConstArrayView4D> RetAge;             // [stock] age, year, fleet, area
  std::vector<ConstArrayView4D> DiscMort;           // [stock] age, year, fleet, area
  
  std::vector<std::vector<ConstArrayView3D>> SelSize;   // [stock][fleet] class, year, area
  std::vector<std::vector<ConstArrayView3D>> RetSize;  // [stock][fleet] class, year, area
  
  ConstArrayView1D   SPFrom;          // stock
  ConstArrayView1D   RelSize;         // area
  ConstArrayView1D   SpawnTimeFrac;   // stock
  
  ConstArrayView2D  Targeting;       // year, fleet
  ConstArrayView3D  q;               // stock, year, fleet
  ConstArrayView4D  Closure;         // stock, year, fleet, area
}; 

#endif
