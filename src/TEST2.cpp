#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;


arma::mat CalcVBiomass_(arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
                        arma::mat FleetWeightAtAgeThisTS, // nAge, nFleet
                        arma::mat SelectivityAtAgeThisTS, // nAge, nFleet
                        arma::mat ClosureAreaThisTS, // nFleet, nArea
                        arma::mat VBiomassAreaThisTS, // nFleet, nArea
                        int nArea) {
  
  for (int area=0; area<nArea; area++) {
    VBiomassAreaThisTS.col(area) = arma::sum(NumberAtAgeAreaThisTS.col(area) % 
                                   SelectivityAtAgeThisTS % 
                                   FleetWeightAtAgeThisTS,0) %
                                   ClosureAreaThisTS.col(area);
      
  }
  return(VBiomassAreaThisTS);
}

List CalcFArea_(List FDeadAtAgeAreaStock,
                List FRetainAtAgeAreaStock, // nAge, nFleet, nArea
                arma::vec RelativeSize, // nArea
                arma::mat Catchability, // nFleet
                arma::mat DiscardMortAtAgeThisTS, // nAge, nFleet
                arma::cube EffortArea,
                arma::mat SelectivityAtAgeThisTS,
                arma::mat RetentionAtAgeThisTS,
                int TSindex,
                int nFleet,
                int nArea,
                int nAge) {
  
  arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
  arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
  
  for (int fl=0; fl<nFleet; fl++) {
    for (int area=0; area<nArea; area++) {
      double catchabilityArea = arma::as_scalar(Catchability.row(TSindex).col(fl)/RelativeSize(area));
      double effortarea = arma::as_scalar(EffortArea.subcube(TSindex, fl, area, TSindex, fl, area));
      arma::vec FInteract = effortarea * catchabilityArea * SelectivityAtAgeThisTS.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAgeThisTS.col(fl);
      arma::vec Discard = FInteract - FRetain;
      arma::vec DeadDiscard = Discard % DiscardMortAtAgeThisTS.col(fl);
      FRetainAtAgeAreaThisTS.subcube(0, fl, area, nAge-1, fl, area) = FRetain;
      FDeadAtAgeAreaThisTS.subcube(0, fl, area, nAge-1,fl, area) = FRetain + DeadDiscard;
    }
  }
  
  FDeadAtAgeAreaStock[TSindex] = FDeadAtAgeAreaThisTS;
  FRetainAtAgeAreaStock[TSindex] = FRetainAtAgeAreaThisTS;
  
  List L = List::create(Named("FDeadAtAgeAreaStock")=FDeadAtAgeAreaStock,
                        Named("FRetainAtAgeAreaStock") = FRetainAtAgeAreaStock);
  return(L);
}



arma::cube CalcNumberNext_(arma::cube NumberAtAgeArea, // nAge, nTS, nArea
                           arma::mat Semelparous,  // nAge, nTS
                           arma::cube FDeadAtAgeAreaThisTS,  // nAge, nFleet, nArea
                           arma::vec NaturalMortalityAtAgeThisTS,
                           bool plusgroup,
                           int nAge,
                           int nArea,
                           int TSindex) {
  
  arma::mat Nnow = NumberAtAgeArea.col(TSindex);
  arma::mat Nnext(nAge, nArea, arma::fill::zeros);
  arma::mat Zmortality(nAge, nArea, arma::fill::zeros); // nAge, nArea;
  arma::mat FDead = arma::sum(FDeadAtAgeAreaThisTS, 1);
  
  for (int area=0; area<nArea; area++) {
    Zmortality.col(area) = FDead.col(area) + NaturalMortalityAtAgeThisTS;
  }
  
  for (int age=0; age<(nAge-1); age++) {
    Nnext.row(age+1) = Nnow.row(age) % exp(-Zmortality.row(age)) * (1-Semelparous(age, TSindex));
  }
  
  if (plusgroup) {
    Nnext.row(nAge-1) = Nnext.row(nAge-1) + (Nnow.row(nAge-1) % exp(-Zmortality.row(nAge-1)) * (1-Semelparous(nAge-1, TSindex)));
  }
  
  NumberAtAgeArea.col(TSindex+1) = Nnext;
  
  return(NumberAtAgeArea);
}


arma::cube MoveStock_(arma::cube NumberAtAgeArea,
                      arma::cube Movement, // nAge, FromArea, ToArea
                      int nAge,
                      int nArea,
                      int TSindex) {
  
  arma::mat NumberAtAgeAreaThisTS =  NumberAtAgeArea.col(TSindex); // nAge, nArea
  arma::mat NumberAtAgeAreaMoved(nAge, nArea, arma::fill::zeros);  // nAge, nArea
  
  for (int age=0; age<nAge; age++) {
    arma::mat moveage = Movement.row(age);
    for (int toArea=0; toArea<nArea; toArea++) {
      arma::vec Narea(nArea, arma::fill::zeros);
      for (int fromArea=0; fromArea<nArea; fromArea++) {
        Narea(toArea) += NumberAtAgeAreaThisTS(age, fromArea) * moveage(fromArea, toArea);
      }
      NumberAtAgeAreaMoved(age, toArea) = Narea(toArea);
    }
  }
  
  NumberAtAgeArea.col(TSindex) = NumberAtAgeAreaMoved;
  return(NumberAtAgeArea);
  
};




// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// for a SINGLE simulation
// [[export]]
List CalcPopDynamics2_(Rcpp::List OMListSim,
                             Rcpp::NumericVector TimeSteps) {


  List OMListSimOut = clone(OMListSim);
  List AgesList = OMListSimOut["Ages"];
  List Length = OMListSimOut["Length"];
  
  
  List Weight = OMListSimOut["Weight"];
  List WeightAtAgeList = Weight["MeanAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List NaturalMortalityAtAgeList = NaturalMortality["MeanAtAge"];
  
  List Maturity = OMListSimOut["Maturity"];
  List MaturityAtAgeList = Maturity["MeanAtAge"];
  List SemelparousList = Maturity["Semelparous"];
          
  List Fecundity = OMListSimOut["Fecundity"];
  List FecundityAtAgeList = Fecundity["MeanAtAge"];
  
  List SRR = OMListSimOut["SRR"];
  arma::vec SpawnTimeFrac = SRR["SpawnTimeFrac"];
  List SPFromList = SRR["SPFrom"];
  List R0List = SRR["R0"];
  
  List SP0List = OMListSimOut["SP0"];
  List RecDevsList = SRR["RecDevs"];
  List SRRParsList = SRR["SRRPars"];
  List SRRModelList = SRR["SRRModel"];
  
  List Spatial = OMListSimOut["Spatial"];
  List UnfishedDistList = Spatial["UnfishedDist"];
  List RelativeSizeList = Spatial["RelativeSize"];
  List MovementList = Spatial["Movement"];
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  List BiomassList = OMListSimOut["Biomass"];
  List SProductionList = OMListSimOut["SProduction"];
  List SBiomassList = OMListSimOut["SBiomass"];
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];

  List FishingMortality = OMListSimOut["FishingMortality"];
  
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  List DiscardMortalityAtAgeList = DiscardMortality["MeanAtAge"];
  
  List Effort = OMListSimOut["Effort"];
  List EffortList = Effort["Effort"];
  List CatchabilityList = Effort["Catchability"];
  
  List Selectivity = OMListSimOut["Selectivity"];
  List SelectivityAtAgeList = Selectivity["MeanAtAge"];
  
  List Retention = OMListSimOut["Retention"];
  List RetentionAtAgeList = Retention["MeanAtAge"];
  
  List Distribution = OMListSimOut["Distribution"];
  List ClosureAreaList = Distribution["Closure"];
  
  List EffortAreaList = OMListSimOut["EffortArea"];
  List VBiomassAreaList = OMListSimOut["VBiomassArea"];
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List FDeadAtAgeList = OMListSimOut["FDeadAtAge"];
  List FRetainAtAgeList = OMListSimOut["FRetainAtAge"];
  
  List RemovalAtAgeAreaList = OMListSimOut["RemovalAtAgeArea"];
  List RetainAtAgeAreaList = OMListSimOut["RetainAtAgeArea"];
  List RemovalNumberAtAgeList = OMListSimOut["RemovalNumberAtAge"];
  List RetainNumberAtAgeList = OMListSimOut["RetainNumberAtAge"];
  List RemovalBiomassAtAgeList =  OMListSimOut["RemovalBiomassAtAge"];
  List RetainBiomassAtAgeList = OMListSimOut["RetainBiomassAtAge"];
         
  List FleetWeightAtAgeList = OMListSimOut["FleetWeightAtAge"];

  int nTS = TimeSteps.size();
  int nStock = BiomassList.size();

  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);

    // Do MICE
    // update length, weight, fleet weight, natural mortality, maturity, etc 

    
    for (int st=0; st<nStock; st++) {
    
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
      arma::cube ClosureArea = ClosureAreaList[st]; // nTS, nFleet, nArea
      arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetentionAtAge = RetentionAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
      arma::mat Effort = EffortList[st]; // nTS, nFleet
      
  

      int nAge = NumberAtAgeArea.n_rows;
      int nArea = NumberAtAgeArea.n_slices;
      int nFleet = VBiomassArea.n_cols;
      
      // Biomass
      arma::mat WeightAtAge = WeightAtAgeList[st]; // nAge, nTS
      arma::vec Biomass = BiomassList[st];
      Biomass[TSindex] = arma::accu(WeightAtAge.col(TSindex) % arma::sum(NumberAtAgeArea.col_as_mat(TSindex),1));
      
      // VBiomass by Area
      // (assumed vulnerable (selectivity) & available (spatial closure))
      VBiomassArea.row(TSindex) = CalcVBiomass_(NumberAtAgeArea.col(TSindex), // nAge, nArea
                                                FleetWeightAtAge.col(TSindex), // nAge, nFleet
                                                SelectivityAtAge.col(TSindex), // nAge, nFleet
                                                ClosureArea.row(TSindex), // nFleet, nArea
                                                VBiomassArea.row(TSindex), // nFleet, nArea
                                                nArea); 
      // Distribute Effort over Areas
      // (currently proportional to VB)
      EffortArea.row(TSindex) = DistEffort_(EffortArea.row(TSindex),
                     VBiomassArea.row(TSindex))
        
        
      arma::mat DistEffort_(arma::mat EffortAreaThisTS,
                            arma::mat VBiomassAreaThisTS,
                            arma::vec EffortThisTS,
                            int nArea,
                            int nFleet
                             ) {
        
        
        arma::rowvec relvbiomassarea(nArea, arma::fill::zeros);
        double totalVB = arma::accu(VBiomassAreaThisTS.row(fl));
        if (totalVB > 0) {
          relvbiomassarea = VBiomassAreaThisTS.row(fl)/totalVB;
        }
        
        
        for (int fl=0; fl<nFleet; fl++) {
          EffortArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = arma::as_scalar(Effort.row(TSindex).col(fl)) * relvbiomassarea;
        }
        
        
        return(EffortAreaThisTS)
        
      }
   
      // for (int fl=0; fl<nFleet; fl++) {
      //   for (int area=0; area<nArea; area++) {
      //     VBiomassAreaThisTS.row(fl).col(area) = arma::accu(NumberAtAgeAreaThisTS.col(area) % 
      //       FleetWeightAtAgeThisTS.col(fl) % 
      //       SelectivityAtAgeThisTS.col(fl)) *
      //       ClosureAreaThisTS.row(fl).col(area);
      //   }
      //   

      //   
      // }
      // VBiomassArea.row(TSindex) = VBiomassAreaThisTS;
      // 
      
      
      arma::mat FleetWeightAtAgeThisTS = FleetWeightAtAge.col(TSindex); // nAge, nFleet
 
      arma::mat SelectivityAtAgeThisTS = SelectivityAtAge.col(TSindex); // nAge, nFleet
      
      arma::mat RetentionAtAgeThisTS = RetentionAtAge.col(TSindex); // nAge, nFleet
     
      arma::mat DiscardMortAtAgeThisTS = DiscardMortalityAtAge.col(TSindex);
     



      
      arma::mat VBiomassAreaThisTS = VBiomassArea.row(TSindex); // nFleet, nArea
      
      arma::mat NumberAtAgeAreaThisTS = NumberAtAgeArea.col(TSindex); // nAge, nArea
      
 
      arma::mat ClosureAreaThisTS = ClosureArea.row(TSindex); // nFleet, nArea // 1 for Open, 0 for Closed

      
      

     


     
      
  


     // // F within each Area
     // List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
     // List FRetainAtAgeAreaStock = FRetainAtAgeAreaStock[st];
     // 
     // List FArea = CalcFArea_(FDeadAtAgeAreaStock, 
     //                         FRetainAtAgeAreaStock,
     //                         RelativeSizeList[st],
     //                         CatchabilityList[st],
     //                         DiscardMortalityAtAge.col(TSindex),
     //                         EffortAreaList[st],
     //                         SelectivityAtAgeThisTS,
     //                         RetentionAtAgeThisTS,
     //                         TSindex,
     //                         nFleet,
     //                         nArea,
     //                         nAge);
     // 
     // 
     // FDeadAtAgeAreaStock = FArea["FDeadAtAgeAreaStock"];
     // FRetainAtAgeAreaStock = FArea["FDeadAtAgeAreaStock"];
  
     // arma::vec RelativeSize = RelativeSizeList[st]; // nArea
     // arma::mat Catchability = CatchabilityList[st]; // nTS, nFleet
     // arma::mat DiscardMortAtAgeThisTS = DiscardMortalityAtAge.col(TSindex);
     
    
     

     // Removals and Retained Number and Biomass by Area
     // 
     // Rcpp::List RemovalAtAgeAreaStock = RemovalAtAgeAreaList[st];
     // Rcpp::List RetainAtAgeAreaStock = RetainAtAgeAreaList[st];
     // arma::cube RemovalAtAgeAreaThisTS = RemovalAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
     // arma::cube RetainAtAgeAreaThisTS = RetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
     // 
     // arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
     // arma::cube RetainNumberAtAge = RetainNumberAtAgeList[st];  // nAge, nTS, nFleet
     // arma::cube RemovalBiomassAtAge = RemovalBiomassAtAgeList[st];  // nAge, nTS, nFleet
     // arma::cube RetainBiomassAtAge = RetainBiomassAtAgeList[st];  // nAge, nTS, nFleet
     // 
     // arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
     // 
     // arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
     // arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
     // 
     // arma::mat Zmortality(nAge, nArea, arma::fill::zeros); // nAge, nArea;
     // for (int area=0; area<nArea; area++) {
     //   Zmortality.col(area) = arma::sum(FDeadAtAgeAreaThisTS.slice(area),1) + NaturalMortalityAtAge.col(TSindex);
     //   arma::mat ndead = NumberAtAgeAreaThisTS.col(area) % (1-exp(-Zmortality.col(area))); // nAge, nFleet
     //   RemovalAtAgeAreaThisTS.slice(area) = FDeadAtAgeAreaThisTS.slice(area)/Zmortality.col(area) % ndead ; // nAge, nFleet;
     //   RetainAtAgeAreaThisTS.slice(area) = FRetainAtAgeAreaThisTS.slice(area)/Zmortality.col(area) % ndead;
     // }
     

     // sum catches over areas
     // arma::mat RemovalAgeFleet = arma::sum(RemovalAtAgeAreaThisTS, 2);
     // arma::mat RetainAgeFleet = arma::sum(RetainAtAgeAreaThisTS, 2);
     // RemovalNumberAtAge.col(TSindex) = RemovalAgeFleet;
     // RetainNumberAtAge.col(TSindex) = RetainAgeFleet;
     // RemovalBiomassAtAge.col(TSindex) =  RemovalAgeFleet % FleetWeightAtAgeThisTS;
     // RetainBiomassAtAge.col(TSindex) =  RetainAgeFleet % FleetWeightAtAgeThisTS;
     // 
     // RemovalAtAgeAreaStock[TSindex] = RemovalAtAgeAreaThisTS;
     // RetainAtAgeAreaStock[TSindex] = RetainAtAgeAreaThisTS;
     // 
     
     // Calculate F over all areas
     // arma::cube FDeadAtAge = FDeadAtAgeList[st]; // nAge, nTS, nFleet
     // arma::cube FRetainAtAge = FRetainAtAgeList[st]; // nAge, nTS, nFleet
     
     // if (nArea<2) {
     //   // no spatial structure
     //   arma::mat temp = sum(FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)),2);
     //   FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = temp;
     //   
     //   temp = sum(FRetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)),2);
     //   FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = temp;
     //   
     //   FDeadAtAgeList[st] = FDeadAtAge;
     //   FRetainAtAgeList[st] = FRetainAtAge;
     //   
     // } else {
     //   // spatial structure - need to calculate overall F 
     //   
     //   arma::mat NatAgeArea = NumberAtAgeArea(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nArea-1)); // nAge, nArea
     //   arma::vec NumberAtAge = sum(NatAgeArea,1); // summed over areas
     //   double TotalNumber = accu(NumberAtAge); 
     //   
     //   arma::mat TotalRemovalsFleet = RemovalNumberAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1));
     //   arma::vec TotalRemovals = sum(TotalRemovalsFleet,0);
     //   
     //   LogicalVector ZeroCatch(nFleet);
     //   ZeroCatch = TotalRemovals < 1E-4;
     //   
     //   // only continue if catches >0
     //   if ((all(ZeroCatch).is_false())) {
     //     // check if F same in all areas 
     //     LogicalVector IdenticalF(nFleet);
     //     for (int fl=0; fl<nFleet; fl++) {
     //       arma::mat fdeadfleet = FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl,fl), arma::span(0,nArea-1));
     //       NumericMatrix FDeadFleet = as<NumericMatrix>(Rcpp::wrap(fdeadfleet));
     //       int same = 0;
     //       for (int area=1; area<nArea; area++) {
     //         NumericVector sameAge = FDeadFleet(_,0) / FDeadFleet(_,area);
     //         same += sum(sameAge);
     //       }
     //       if (same == nAge) {
     //         IdenticalF(fl) = TRUE;
     //       }
     //     }
     //     
     //     if ((all(IdenticalF).is_true())) {
     //       // identical F across areas for all fleets
     //       FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = 
     //         FDeadAtAgeAreaThisTS(arma::span(0, nAge-1),arma::span(0, nFleet-1),   arma::span(0,0)); 
     //       FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = 
     //         FRetainAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)); 
     //       
     //       FDeadAtAgeList[st] = FDeadAtAge;
     //       FRetainAtAgeList[st] = FRetainAtAge;
     //      
     //     } else {
     //       // need to solve for overall F 
     //       int MaxIt = 200;
     //       double tolF = 1E-4;
     //       
     //       arma::vec apicalF(nFleet);
     //       for (int fl=0; fl<nFleet; fl++) {
     //         apicalF(fl) = TotalRemovals[fl]/TotalNumber;
     //       }
     //       
     //       for (int i=0; i<MaxIt; i++) {
     //         // Calculate F-at-age fleet
     //         for (int fl=0; fl<nFleet; fl++) {
     //           arma::vec FInteract = apicalF(fl) * SelectivityAtAgeThisTS.col(fl);
     //           arma::vec FRetain = FInteract % RetentionAtAgeThisTS.col(fl);
     //           arma::vec FDiscard = FInteract - FRetain;
     //           arma::vec FDeadDiscard = FDiscard % DiscardMortAtAgeThisTS.col(fl);
     //           arma::vec FDead = FDeadDiscard + FRetain;
     //           FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl)) = FRetain;
     //           FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl)) = FDead;
     //         }
     //         
     //         arma::mat FDeadAtAgeFleet = FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1));
     //         arma::vec ZatAge = sum(FDeadAtAgeFleet,1) + NaturalMortalityAtAge.col(TSindex);
     //         arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % NumberAtAge;
     //         
     //         // derivative of catch wrt ft
     //         arma::vec dct(nFleet);
     //         arma::vec predRemovals(nFleet);
     //         for (int fl=0; fl<nFleet; fl++) {
     //           arma::vec FDead = FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl));
     //           predRemovals(fl) = arma::accu(FDead/ZatAge % (1-exp(-ZatAge)) % NumberAtAge);
     //           arma::vec temp = PopDeadAtAge / ZatAge - ((FDead % PopDeadAtAge / pow(ZatAge,2))) + FDead/ZatAge % exp(-ZatAge)%NumberAtAge;
     //           dct(fl) = accu(temp);
     //         }
     //         
     //         apicalF = apicalF - (predRemovals - TotalRemovals)/(0.8*dct);
     //         NumericVector diff = as<NumericVector>(Rcpp::wrap((predRemovals - TotalRemovals)));
     //         NumericVector totalremovals = as<NumericVector>(Rcpp::wrap((TotalRemovals)));
     //         
     //         LogicalVector converge = (Rcpp::abs(diff)/totalremovals) < tolF;
     //         if (all(converge).is_true())
     //           break;
     //       }
     //       FDeadAtAgeList[st] = FDeadAtAge;
     //       FRetainAtAgeList[st] = FRetainAtAge;
     //       
     //     }
     //   }
     // } // end of calculate overall F 
     
     
     // Calc Spawning Production and Spawning Biomass
     // arma::vec SProduction = SProductionList[st]; // nTS
     // arma::vec SBiomass = SBiomassList[st]; // nTS
     // 
     // arma::mat FecundityAtAge = FecundityAtAgeList[st]; // nage, nTS
     // arma::mat MaturityAtAge = MaturityAtAgeList[st]; // nage, nTS
     // 
     // // Sum number over areas and F over fleets
     // arma::vec NatAge(nAge);
     // arma::vec FDeadatAge(nAge);
     // for (int age=1; age<nAge; age++) { // assumes first age class is age 0 and doesn't spawn
     //   NatAge(age) = accu(NumberAtAgeArea(arma::span(age, age), arma::span(TSindex, TSindex), arma::span(0, nArea-1)));
     //   FDeadatAge(age) = accu(FDeadAtAge(arma::span(age, age), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)));
     // }
     // 
     // // Calculate SP production
     // arma::vec SpawnMortality(nAge);
     // arma::vec NSpawn = NatAge;
     // if (SpawnTimeFrac[st] > 0) {
     //   SpawnMortality = (NaturalMortalityAtAge.col(TSindex) + FDeadatAge) * SpawnTimeFrac[st];
     //   NSpawn = NSpawn % exp(-SpawnMortality);
     // }
     // double Sproduction = arma::accu(NSpawn % FecundityAtAge.col(TSindex));
     // double Sbiomass = arma::accu(NSpawn % WeightAtAge.col(TSindex) % MaturityAtAge.col(TSindex));
     // SProduction(TSindex) = Sproduction;
     // SBiomass(TSindex) = Sbiomass;
     // SProductionList[st] = SProduction;
     // SBiomassList[st] = SBiomass;
     // 
     // RemovalAtAgeAreaList[st] = RemovalAtAgeAreaStock;
     // RetainAtAgeAreaList[st] = RetainAtAgeAreaStock;
     // 
     // RemovalNumberAtAgeList[st] = RemovalNumberAtAge;
     // RetainNumberAtAgeList[st] = RetainNumberAtAge;
     // RemovalBiomassAtAgeList[st] = RemovalBiomassAtAge;
     // RetainBiomassAtAgeList[st] = RetainBiomassAtAge;
     // 
     // BiomassList[st] = Biomass;
     // VBiomassAreaList[st] = VBiomassArea;
     // EffortAreaList[st] = EffortArea;
     // 
     // 
     // 
     // FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
     // FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
     
     
    
    } // end of stock loop
    
    // apply SPfrom
    // if (nStock>1) {
    //   for (int st=0; st<nStock; st++) {
    //     int SPFrom = SPFromList[st];
    //     SProductionList[st] = SProductionList[SPFrom];
    //     SBiomassList[st] = SBiomassList[SPFrom];
    //   }
    // } // end of stock loop for Spawning Production 
    // 
    // 
   // for (int st=0; st<nStock; st++) {
   //   arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
   // 
   //   arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
   //   arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
   // 
   //   Rcpp::List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
   //   Rcpp::List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
   // 
   // 
   // 
   //   List SRRPars = SRRParsList[st];
   //   
   //   int nAge = NumberAtAgeArea.n_rows;
   //   int nArea = NumberAtAgeArea.n_slices;
   //   int nFleet = VBiomassArea.n_cols;
   //   
   //   // Calc Recruitment
   //   NumericVector SProduction = SProductionList[st];
   //   NumericVector R0 = R0List[st];
   //   NumericVector SP0 = SP0List[st];
   //   NumericVector RecDevs = RecDevsList[st];
   //   Function SRRModel = SRRModelList[st];
   // 
   //   // uses SP0 from first time step
   //   List Arglist = List::create(Named("S") = SProduction[TSindex],
   //                               Named("S0") = SP0[0],
   //                                                Named("R0") = R0[TSindex]);
   // 
   //   CharacterVector ParNames = SRRPars.names();
   //   CharacterVector ArglistNames(3+SRRPars.size());
   //   ArglistNames[0] = "S";
   //   ArglistNames[1] = "S0";
   //   ArglistNames[2] = "R0";
   // 
   //   for (int i=0; i<SRRPars.size(); i++) {
   //     NumericVector argVec = SRRPars[i];
   //     double arg = argVec[i];
   //     Arglist.push_back(arg);
   //     ArglistNames[3+i] = ParNames[i];
   //   }
   //   Arglist.attr("names") = ArglistNames;
   // 
   //   Rcpp::Environment base("package:base");
   //   Rcpp::Function doCall = base["do.call"];
   // 
   //   RObject RecruitsEQ = doCall(SRRModel, Arglist);
   //   // TODO - check for valid value
   //   double Recruits = as<double>(RecruitsEQ) * RecDevs[TSindex];
   // 
   //   
   //   
   //   
   //   // Add Recruits
   //   // TODO option to add to beginning of next time-step i.e age 1
   //   // Need to check if first age class is 0 or 1;
   // 
   //   int AgeRec = 0; // will always be 0
   //   int TSRec = TSindex; // alternative: TSindex + 1 for age-1 recruitment
   //   arma::cube UnfishedDist = UnfishedDistList[st]; // nage, nTS, nArea
   // 
   //   arma::vec recruitArea(nArea);
   //   for (int area=0; area<nArea; area++) {
   //     recruitArea(area) = Recruits * arma::as_scalar(UnfishedDist(arma::span(AgeRec, AgeRec), arma::span(TSRec, TSRec), arma::span(area, area)));
   //   }
   //  
   //   NumberAtAgeArea.subcube(AgeRec, TSRec, 0, AgeRec, TSRec, nArea-1) = recruitArea;
   // 
   // 
   // 
   //   if (timestep<(nTS-1)) {
   //     // Update Number beginning of next Time Step
   //     Rcpp::RObject ages = AgesList[st];
   //     bool plusgroup = ages.slot("PlusGroup");
   //     
   //     NumberAtAgeArea = CalcNumberNext_(NumberAtAgeArea, 
   //                                       SemelparousList[st],
   //                                       FDeadAtAgeAreaStock[TSindex],
   //                                       NaturalMortalityAtAge.col(TSindex),
   //                                       plusgroup,
   //                                       nAge,
   //                                       nArea,
   //                                       TSindex); 
   //     
   // 
   //     // Move Population at beginning of next Time Step
   //     List MovementStock = MovementList[st];
   //     NumberAtAgeAreaList[st] = MoveStock_(NumberAtAgeArea, MovementStock[TSindex+1], nAge, nArea, TSindex+1);
   //   } else {
   //     NumberAtAgeAreaList[st] = NumberAtAgeArea;  
   //   }
   // 
   // 
   //   
   //   
   //   
   //   
   // } // end of stock loop
   // 
 
     
    // OMListSimOut["NumberAtAgeArea"] = NumberAtAgeAreaList;
    // OMListSimOut["Biomass"] = BiomassList;
    // OMListSimOut["Biomass"] = BiomassList;
    // OMListSimOut["VBiomassArea"] = VBiomassAreaList;
    // OMListSimOut["EffortArea"] = EffortAreaList;
    // OMListSimOut["FDeadAtAgeArea"] = FDeadAtAgeAreaList;
    // OMListSimOut["FRetainAtAgeArea"] = FRetainAtAgeAreaList;
    // 
    // OMListSimOut["FDeadAtAgeArea"] = FDeadAtAgeAreaList;
    // OMListSimOut["FRetainAtAgeArea"] = FRetainAtAgeAreaList;
    // 
    // OMListSimOut["RemovalAtAgeArea"] = RemovalAtAgeAreaList;
    // OMListSimOut["RetainAtAgeArea"] = RetainAtAgeAreaList;
    // OMListSimOut["RemovalNumberAtAge"] = RemovalNumberAtAgeList;
    // OMListSimOut["RetainNumberAtAge"] = RetainNumberAtAgeList;
    // 
    // OMListSimOut["RemovalBiomassAtAge"] = RemovalBiomassAtAgeList;
    // OMListSimOut["RetainBiomassAtAge"] = RetainBiomassAtAgeList;
    // 
    // OMListSimOut["FDeadAtAge"] = FDeadAtAgeList;
    // OMListSimOut["FRetainAtAge"] =FRetainAtAgeList;
    // 
    // OMListSimOut["SBiomass"] = SBiomassList;
    // OMListSimOut["SProductionList"] = SBiomassList;
    
  } // end of time steps loop
  
 

  return(OMListSimOut);
}