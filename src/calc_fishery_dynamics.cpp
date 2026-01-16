#include <Rcpp.h>
#include "array_types.h"
#include "array_views.h"
#include "helpers.h"
#include "calc_spatial_effort_dist.h"
#include "calc_area_f.h"
#include "calc_spawn_production.h"

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::S4 CalcFisheryDynamics_(Rcpp::S4 HistIn,
                              Rcpp::NumericVector Years, // Years to loop over
                              Rcpp::NumericVector AllYears,
                              int nSim,
                              const int nStock,
                              const int nFleet,
                              const int nArea,
                              const int debug=0,
                              const int CalcCatch=1       // calculate catch and overall F?
) {
  
  Rcpp::S4 Hist = Rcpp::clone(HistIn); 

  // ---------------------------------------------------------
  // Mutable Hist lists & arrays
  // ---------------------------------------------------------

  Rcpp::List NumberList = Hist.slot("Number");                    // sim, stock, year, area
  Array3D Biomass = Slot2Array3D(Hist, "Biomass");                // sim, stock, year
  Array3D SBiomass = Slot2Array3D(Hist, "SBiomass");              // sim, stock, year
  Array3D SProduction = Slot2Array3D(Hist, "SProduction");        // sim, stock, year
  Rcpp::List LandingsAtAgeList = Hist.slot("LandingsAtAge");      // sim, age, year, fleet, area
  Rcpp::List DiscardsAtAgeList = Hist.slot("DiscardsAtAge");      // sim, age, year, fleet, area
  Rcpp::List LandingsAtSizeList = Hist.slot("LandingsAtSize");    // sim, class, year, fleet, area
  Rcpp::List DiscardsAtSizeList = Hist.slot("DiscardsAtSize");    // sim, class, year, fleet, area
  Rcpp::List FDeadList = Hist.slot("FDead");                      // sim, age, year, fleet
  Rcpp::List FRetainList = Hist.slot("FRetain");                  // sim, age, year, fleet
  Array3D Effort = Slot2Array3D(Hist, "Effort");                  // sim, year, fleet
  Array4D Distribution = Slot2Array4D(Hist, "Distribution");      // sim, year, fleet, area
  Rcpp::List FDeadAreaList = Hist.slot("FDeadArea");              // sim, age, year, fleet, area
  Rcpp::List FRetainAreaList = Hist.slot("FRetainArea");          // sim, age, year, fleet, area
   
  // non-mutable temporary objects
  const Rcpp::List Misc = Hist.slot("Misc");

  // ---- wrap mutable arrays ----
  std::vector<Array4D> Number;

  std::vector<Array5D> LandingsAtAge;
  std::vector<Array5D> DiscardsAtAge;
  std::vector<std::vector<Array4D>> LandingsAtSize;
  std::vector<std::vector<Array4D>> DiscardsAtSize;
  std::vector<Array4D> FDead;
  std::vector<Array4D> FRetain;
  std::vector<Array5D> FDeadArea;
  std::vector<Array5D> FRetainArea;

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
    Number.emplace_back(as_ArrayND<4>(NumberList[st]));
    LandingsAtAge.emplace_back(as_ArrayND<5>(LandingsAtAgeList[st]));
    DiscardsAtAge.emplace_back(as_ArrayND<5>(DiscardsAtAgeList[st]));
    FDead.emplace_back(as_ArrayND<4>(FDeadList[st]));
    FRetain.emplace_back(as_ArrayND<4>(FRetainList[st]));
    FDeadArea.emplace_back(as_ArrayND<5>(FDeadAreaList[st]));
    FRetainArea.emplace_back(as_ArrayND<5>(FRetainAreaList[st]));
    
    const Rcpp::List LandSize_st = LandingsAtSizeList[st];
    const Rcpp::List DiscSize_st = DiscardsAtSizeList[st];
    
    if (LandSize_st.size() != nFleet || DiscSize_st.size() != nFleet) {
      Rcpp::stop("LandingsAtSize / DiscardsAtSize: wrong fleet dimension for stock %d", st);
    }
    
    std::vector<Array4D> LandSize_fl;
    std::vector<Array4D> DiscSize_fl;
    LandSize_fl.reserve(nFleet);
    DiscSize_fl.reserve(nFleet);
    for (int fl = 0; fl < nFleet; ++fl) {
      LandSize_fl.emplace_back(as_ArrayND<4>(LandSize_st[fl]));
      DiscSize_fl.emplace_back(as_ArrayND<4>(DiscSize_st[fl]));
    }
    LandingsAtSize.emplace_back(std::move(LandSize_fl));
    DiscardsAtSize.emplace_back(std::move(DiscSize_fl));
  }
  
  // ---------------------------------------------------------
  // Extract non-mutable objects to from Hist@Misc
  // ---------------------------------------------------------
  
  ConstArrayView1D SPFrom = GetMisc_ConstArrayView<1>(Hist, "SPFrom");
  ConstArrayView2D RelSize = GetMisc_ConstArrayView<2>(Hist, "RelSize");
  ConstArrayView2D SpawnTimeFrac = GetMisc_ConstArrayView<2>(Hist, "SpawnTimeFrac");

  // Stock
  // -at-age arrays in nStock list
  const Rcpp::List LengthList = Misc["LengthList"];
  const Rcpp::List WeightList = Misc["WeightList"];
  const Rcpp::List NaturalMortalityList = Misc["NaturalMortalityList"];
  const Rcpp::List MaturityList = Misc["MaturityList"];
  const Rcpp::List SemelparousList = Misc["SemelparousList"];
  const Rcpp::List FecundityList = Misc["FecundityList"];

  std::vector<ConstArrayView3D> Length;
  std::vector<ConstArrayView3D> Weight;
  std::vector<ConstArrayView3D> NaturalMortality;
  std::vector<ConstArrayView3D> Maturity;
  std::vector<ConstArrayView3D> Semelparous;
  std::vector<ConstArrayView3D> Fecundity;

  Length.reserve(nStock);
  Weight.reserve(nStock);
  NaturalMortality.reserve(nStock);
  Maturity.reserve(nStock);
  Semelparous.reserve(nStock);
  Fecundity.reserve(nStock);

  for (int st = 0; st < nStock; ++st) {
    Length.emplace_back(view_StockList3D(LengthList, st));
    Weight.emplace_back(view_StockList3D(WeightList, st));
    NaturalMortality.emplace_back(view_StockList3D(NaturalMortalityList, st));
    Maturity.emplace_back(view_StockList3D(MaturityList, st));
    Semelparous.emplace_back(view_StockList3D(SemelparousList, st));
    Fecundity.emplace_back(view_StockList3D(FecundityList, st));
  }
  
  // Fleet
  // -at-age arrays in nStock list
  const Rcpp::List WeightFleetList = Misc["WeightFleetList"];
  const Rcpp::List SelAgeList         = Misc["SelAgeList"];
  const Rcpp::List SelSizeList         = Misc["SelSizeList"];
  const Rcpp::List RetAgeList         = Misc["RetAgeList"];
  const Rcpp::List RetSizeList         = Misc["RetSizeList"];
  const Rcpp::List DiscMortList       = Misc["DiscMortList"];

  std::vector<ConstArrayView4D> WeightFleet;
  std::vector<ConstArrayView5D> SelAge;
  std::vector<ConstArrayView5D> SelSize;
  std::vector<ConstArrayView5D> RetAge;
  std::vector<ConstArrayView5D> RetSize;
  std::vector<ConstArrayView5D> DiscMort;

  WeightFleet.reserve(nStock);
  SelAge.reserve(nStock);
  SelSize.reserve(nStock);
  RetAge.reserve(nStock);
  RetSize.reserve(nStock);
  DiscMort.reserve(nStock);

  for (int st = 0; st < nStock; ++st) {
    WeightFleet.emplace_back(view_StockList4D(WeightFleetList, st));
    SelAge.emplace_back(view_StockList5D(SelAgeList, st));
    SelSize.emplace_back(view_StockList5D(SelSizeList, st));
    RetAge.emplace_back(view_StockList5D(RetAgeList, st));
    RetSize.emplace_back(view_StockList5D(RetSizeList, st));
    DiscMort.emplace_back(view_StockList5D(DiscMortList, st));
  }

  ConstArrayView4D q = GetMisc_ConstArrayView<4>(Hist, "Catchability");
  ConstArrayView5D Closure = GetMisc_ConstArrayView<5>(Hist, "Closure");
  ConstArrayView3D Targeting = GetMisc_ConstArrayView<3>(Hist, "Targeting");
 
  // Time Steps
  std::vector<int> ts_index = CalcTSIndex(Years, AllYears); // time-step index 
  int nTS = ts_index.size();
  
  // Loop over time steps in Years
  for (int ts = 0; ts < nTS; ++ts) { 
    
    int y = ts_index[ts]; // index for this time step
    
    // ---------------------------------------------------------
    // MICE Calculations - TODO 
    // 
    //  Note: currently all biological arrays are non-mutable
    //        and no life-history parameters are passed into the 
    //        function.
    //        This will have to be revised once MICE calcs are added
    //
    // ---------------------------------------------------------
    
  
    // ---------------------------------------------------------
    // Calculate Spatial Distribution of Fishing Effort
    // src: inst/include/calc_spatial_effort_dist.h
    // ---------------------------------------------------------
    
    CalcSpatialDistribution(y,
                            Distribution,
                            Number,
                            WeightFleet,
                            SelAge,
                            RetAge,
                            q,
                            Closure,
                            Targeting,
                            Effort,
                            RelSize,
                            nStock,
                            nFleet,
                            nArea);

    // ---------------------------------------------------------
    // Calculate Area-Specific Fishing Mortality  
    // src: inst/include/calc_area_f.h
    // ---------------------------------------------------------
    
    CalcArea_F(y,
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
    
    // ---------------------------------------------------------
    // Calculate Global Spawning Biomass and Spawning Production
    // src: inst/include/calc_spawn_production.h
    // ---------------------------------------------------------
    
    CalcSpawnProduction(y,
                        SBiomass,
                        SProduction,
                        Number,
                        Fecundity,
                        Maturity,
                        Weight,
                        NaturalMortality,
                        SpawnTimeFrac,
                        SPFrom,
                        FDeadArea,
                        nStock,
                        nFleet,
                        nArea);
    
    // ---------------------------------------------------------
    // Calculate Recruitment
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Catch (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate overall F (if applicable)
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    // ---------------------------------------------------------
    // Calculate Number at beginning of next time step
    // src: inst/include/...
    // ---------------------------------------------------------
    
    
    
    
  }
  return(Hist);
}
