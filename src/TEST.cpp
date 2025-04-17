#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List CalcBiomass_(List BiomassList,
                  List NumberAtAgeAreaList,
                  List WeightList,
                  int TSindex) {
  
  int nStock = BiomassList.size();

  for (int st=0; st<nStock; st++) {
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector Biomass = BiomassList[st];
    Biomass[TSindex] = 0;
    
    int nAge = NumberAtAgeArea.n_rows;
    int nArea = NumberAtAgeArea.n_slices;

    NumericMatrix WeightAtAge = WeightList[st];
    
    arma::mat narea = NumberAtAgeArea.subcube(0, TSindex, 0, nAge-1, TSindex, nArea-1);
    NumericMatrix NArea  = as<NumericMatrix>(Rcpp::wrap(narea));
    for (int area=0; area<nArea; area++) {
      Biomass(TSindex) += sum(WeightAtAge(_, TSindex) * NArea(_, area));
    }
    BiomassList[st] = Biomass;
  }
  return(BiomassList);
}



// [[Rcpp::export]]
List CalcVBiomass_(List VBiomassAreaList,
                   List NumberAtAgeAreaList,
                   List FleetWeightAtAgeList,
                   List SelectivityAtAgeList,
                   List ClosureAreaList,
                   int TSindex) {

  int nStock = VBiomassAreaList.size();
  
  for (int st=0; st<nStock; st++) {
  
    arma::cube VBiomassArea = VBiomassAreaList[st];

    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st];
    arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st];
    arma::cube SelectivityAtAge = SelectivityAtAgeList[st];
    arma::cube ClosureArea = ClosureAreaList[st];
    
    int nAge = NumberAtAgeArea.n_rows;
    int nArea = VBiomassArea.n_slices;
    int nFleet = VBiomassArea.n_cols;

    arma::mat NArea  = NumberAtAgeArea.subcube(0, TSindex, 0, nAge-1, TSindex, nArea-1); // nAge, nArea
    arma::mat FWeight  = FleetWeightAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1); // nAge, nFleet
    arma::mat Selectivity  = SelectivityAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1); // nAge, nFleet
    arma::mat Closure  = ClosureArea.subcube(TSindex, 0, 0, TSindex, nFleet-1, nArea-1); // nFleet, nArea
    
    for (int area=0; area<nArea; area++) {
      for (int fl=0; fl<nFleet; fl++) {
        VBiomassArea.subcube(TSindex, fl, area, TSindex, fl, area)=
          arma::accu(NumberAtAgeArea.subcube(0, TSindex, area, nAge-1, TSindex, area) % 
          FleetWeightAtAge.subcube(0, TSindex, fl, nAge-1, TSindex, fl) % 
          SelectivityAtAge.subcube(0, TSindex, fl, nAge-1, TSindex, fl)) *
          arma::as_scalar(ClosureArea.subcube(TSindex, fl, area, TSindex, fl, area));
          // arma::accu(NArea.col(area) %  FWeight.col(fl) % Selectivity.col(fl)) % Closure.row(fl);
      }
    }
    
    VBiomassAreaList[st] = VBiomassArea;
  }

  return(VBiomassAreaList);
}


// [[Rcpp::export]]
List CalcDensity_(List DensityAreaList,
                  List VBiomassAreaList,
                  List RelativeSizeList,
                  int TSindex) {
  
  int nStock = DensityAreaList.size();
  
  for (int st=0; st<nStock; st++) {
    arma::cube DensityArea = DensityAreaList[st]; // nTS, nFleet, nArea
    arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
    arma::vec RelativeSize = RelativeSizeList[st]; // narea
   
    int nArea = VBiomassArea.n_slices;
    int nFleet = VBiomassArea.n_cols;

    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FleetVBArea = VBiomassArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1);
      arma::vec FleetDensityArea = FleetVBArea  / RelativeSize;
      DensityArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = FleetDensityArea/sum(FleetDensityArea);;
    }
    DensityAreaList[st] = DensityArea;
  }
  return(DensityAreaList);
}



// [[Rcpp::export]]
List DistEffort_(List EffortAreaList,
                 List VBiomassAreaList,
                 List EffortList,
                 int TSindex) {

  int nStock = VBiomassAreaList.size();
  

  for (int st=0; st<nStock; st++) {
    
    arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
    arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
    arma::mat Effort = EffortList[st]; // nTS, nFleet
    
    int nArea = VBiomassArea.n_slices;
    int nFleet = VBiomassArea.n_cols;
    
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec vbiomassarea = VBiomassArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1);
      double totalVB = arma::accu(vbiomassarea);
      arma::vec relvbiomassarea(nArea, arma::fill::zeros);
      if (totalVB > 0) {
        relvbiomassarea = vbiomassarea/totalVB;
      }
      
      EffortArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = arma::as_scalar(Effort.row(TSindex).col(fl)) * relvbiomassarea; 
    }
    
    EffortAreaList[st] = EffortArea;
  }
  return(EffortAreaList);
}

// [[Rcpp::export]]
List CalcFArea_(List FDeadAtAgeAreaList,
                List FRetainAtAgeAreaList,
                List EffortAreaList,
                List RelativeSizeList,
                List CatchabilityList,
                List SelectivityAtAgeList,
                List RetentionAtAgeList,
                List DiscardMortalityAtAgeList,
                int TSindex) {

  int nStock = FDeadAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {
    Rcpp::List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
    Rcpp::List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
    
    arma::cube FDeadAtAgeArea = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
    arma::cube FRetainAtAgeArea = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea

    arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea

    arma::vec RelativeSize = RelativeSizeList[st]; // narea
    
    arma::mat Catchability = CatchabilityList[st]; // nTS, nFleet

    arma::cube SelectivityAtAge = SelectivityAtAgeList[st];
    arma::cube RetentionAtAge = RetentionAtAgeList[st];
    arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st];

    int nArea = FDeadAtAgeArea.n_slices;
    int nFleet = FDeadAtAgeArea.n_cols;
    int nAge = FDeadAtAgeArea.n_rows;

    arma::mat selectivity = SelectivityAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1);
    arma::mat retention = RetentionAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1);
    arma::mat discardmort = DiscardMortalityAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1);
    arma::rowvec q = Catchability.row(TSindex);

    // Calculate fishing mortality by fleet and area
    for (int area=0; area<nArea; area++) {
      for (int fl=0; fl<nFleet; fl++) {
        double catchabilityArea = q(fl)/RelativeSize(area);
        double effortarea = arma::as_scalar(EffortArea.subcube(TSindex, fl, area, TSindex, fl, area));
        arma::vec FInteract = effortarea * catchabilityArea * selectivity.col(fl);  
        arma::vec FRetain = FInteract % retention.col(fl);  
        arma::vec Discard = FInteract - FRetain;
        arma::vec DeadDiscard = Discard % discardmort.col(fl);
        FRetainAtAgeArea.subcube(0, fl, area, nAge-1, fl, area) = FRetain;
        FDeadAtAgeArea.subcube(0, fl, area, nAge-1,fl, area) = FRetain + DeadDiscard;
      }
    }
    
    FDeadAtAgeAreaStock[TSindex] = FDeadAtAgeArea;
    FRetainAtAgeAreaStock[TSindex] = FRetainAtAgeArea;
    
    FDeadAtAgeAreaList[st] = FDeadAtAgeAreaStock;
    FRetainAtAgeAreaList[st] = FRetainAtAgeAreaStock;
  }

  List L = List::create(Named("FDeadAtAgeArea")=FDeadAtAgeAreaList,
                        Named("FRetainAtAgeArea") = FRetainAtAgeAreaList);
  return(L);
}

// [[Rcpp::export]]
List CalcCatch_(List RemovalAtAgeAreaList,
                List RetainAtAgeAreaList,
                List RemovalNumberAtAgeList,
                List RetainNumberAtAgeList,
                List RemovalBiomassAtAgeList,
                List RetainBiomassAtAgeList,
                List NaturalMortalityAtAgeList,
                List FleetWeightAtAgeList,
                List NumberAtAgeAreaList,
                List FDeadAtAgeAreaList,
                List FRetainAtAgeAreaList,
                int TSindex) {

  int nStock = NaturalMortalityAtAgeList.size();

  for (int st=0; st<nStock; st++) {

    Rcpp::List RemovalAtAgeAreaStock = RemovalAtAgeAreaList[st];
    Rcpp::List RetainAtAgeAreaStock = RetainAtAgeAreaList[st];
    Rcpp::List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
    Rcpp::List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
    

    arma::cube RemovalAtAgeArea = RemovalAtAgeAreaStock[TSindex]; // age, fleet, area
    arma::cube RetainAtAgeArea = RetainAtAgeAreaStock[TSindex]; // age, fleet, area

    arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // age, time step, fleet
    arma::cube RetainNumberAtAge = RetainNumberAtAgeList[st];  // age, time step, fleet
    arma::cube RemovalBiomassAtAge = RemovalBiomassAtAgeList[st];  // age, time step, fleet
    arma::cube RetainBiomassAtAge = RetainBiomassAtAgeList[st];  // age, time step, fleet

    arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
    arma::cube FleetWeightAtAge = FleetWeightAtAgeList[st]; // age, time step, fleet
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // age, time step, area
    arma::cube FDeadAtAgeArea = FDeadAtAgeAreaStock[TSindex]; // age, fleet, area
    arma::cube FRetainAtAgeArea = FRetainAtAgeAreaStock[TSindex]; // age, fleet, area

   int nAge = NumberAtAgeArea.n_rows;
   int nArea =NumberAtAgeArea.n_slices;
   int nFleet = FDeadAtAgeArea.n_cols;

   for (int area=0; area<nArea; area++) {
     arma::vec zdead = arma::sum(FDeadAtAgeArea.subcube(0, 0, area, nAge-1, nFleet-1, area), 1);
     zdead = zdead + NaturalMortalityAtAge.col(TSindex);

     // Rcpp::NumericVector ZDead(nAge);
     // for (int fl=0; fl<nFleet; fl++) {
     //   Rcpp::NumericVector FDeadFleetArea = Rcpp::wrap(FDeadAtAgeArea.subcube(0, fl, area, nAge-1, fl, area));
     //   ZDead = ZDead +=FDeadFleetArea;
     // }
     //
     // arma::vec zdead = Rcpp::as<arma::vec>(Rcpp::wrap(ZDead)) + NaturalMortalityAtAge.col(TSindex);


     for (int fl=0; fl<nFleet; fl++) {
       arma::vec FDeadFleetArea = FDeadAtAgeArea.subcube(0, fl, area, nAge-1, fl, area);
       arma::vec FRetainFleetArea = FRetainAtAgeArea.subcube(0, fl, area, nAge-1, fl, area);
       arma::vec NumberArea = NumberAtAgeArea.subcube(0, TSindex, area, nAge-1, TSindex, area);

       arma::vec removal = FDeadFleetArea/zdead % NumberArea % (1-exp(-zdead));
       arma::vec retain = FDeadFleetArea/zdead % NumberArea % (1-exp(-zdead));

       RemovalAtAgeArea.subcube(0, fl, area, nAge-1, fl, area) = removal;
       RetainAtAgeArea.subcube(0, fl, area, nAge-1, fl, area) = retain;
     }
   }

   // sum over areas
   for (int age=0; age<nAge; age++) {
     for (int fl=0; fl<nFleet; fl++) {
       arma::vec removalArea = RemovalAtAgeArea.subcube(age, fl, 0,age, fl, nArea-1);
       arma::vec retainArea = RemovalAtAgeArea.subcube(age, fl, 0,age, fl, nArea-1);
       double weight = arma::as_scalar(FleetWeightAtAge.subcube(age, TSindex, fl, age, TSindex, fl));

       RemovalNumberAtAge.subcube(age, TSindex, fl, age , TSindex, fl) = sum(removalArea);
       RetainNumberAtAge.subcube(age, TSindex, fl, age, TSindex, fl) = sum(retainArea);
       RemovalBiomassAtAge.subcube(age, TSindex, fl, age , TSindex, fl) = sum(removalArea * weight);
       RetainBiomassAtAge.subcube(age, TSindex, fl, age , TSindex, fl) = sum(retainArea * weight);
     }
   }

   RemovalAtAgeAreaStock[TSindex] = RemovalAtAgeArea;
   RetainAtAgeAreaStock[TSindex] = RetainAtAgeArea;

   RemovalAtAgeAreaList[st] = RemovalAtAgeAreaStock;
   RetainAtAgeAreaList[st] = RetainAtAgeAreaStock;

   RemovalNumberAtAgeList[st] = RemovalNumberAtAge;
   RetainNumberAtAgeList[st] = RetainNumberAtAge;
   RemovalBiomassAtAgeList[st] = RemovalBiomassAtAge;
   RetainBiomassAtAgeList[st] = RetainBiomassAtAge;

  }

  List L = List::create(Named("RemovalAtAgeArea") = RemovalAtAgeAreaList,
                        Named("RetainAtAgeArea") = RetainAtAgeAreaList,
                        Named("RemovalNumberAtAge") = RemovalNumberAtAgeList,
                        Named("RetainNumberAtAge") = RetainNumberAtAgeList,
                        Named("RemovalBiomassAtAge") = RemovalBiomassAtAgeList,
                        Named("RetainBiomassAtAge") = RetainBiomassAtAgeList
  );

  return(L);
}


// [[Rcpp::export]]
List CalcFfromCatch_(List FDeadAtAgeList,
                     List FRetainAtAgeList,
                     List NumberAtAgeAreaList,
                     List RemovalNumberAtAgeList,
                     List NaturalMortalityAtAgeList,
                     List SelectivityAtAgeList,
                     List RetentionAtAgeList,
                     List DiscardMortalityAtAgeList,
                     List FDeadAtAgeAreaList,
                     List FRetainAtAgeAreaList,
                     int TSindex) {

  int MaxIt = 200;
  double tolF = 1E-4;

  // if (control.isNotNull()){
  //   List Control(control);
  //   MaxIt      = Control["MaxIt"];
  //   tolF      = Control["tolF"];
  // }

  int nStock = NaturalMortalityAtAgeList.size();

  for (int st=0; st<nStock; st++) {

    arma::cube FDeadAtAge = FDeadAtAgeList[st]; // age, time steep, fleet
    
    arma::cube FRetainAtAge = FRetainAtAgeList[st]; // age, time steep, fleet
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // age, time step, area
    arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // age, time steep, fleet
    arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
    
    arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // age, time steep, fleet
    arma::cube RetentionAtAge = RetentionAtAgeList[st]; // age, time steep, fleet
    arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // age, time steep, fleet
  
    Rcpp::List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
    Rcpp::List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
    
    arma::cube FDeadAtAgeArea = FDeadAtAgeAreaStock[TSindex]; // age, fleet, area
    arma::cube FRetainAtAgeArea = FRetainAtAgeAreaStock[TSindex]; // age, fleet, area
    
    int nAge = NumberAtAgeArea.n_rows;
    int nArea =NumberAtAgeArea.n_slices;
    int nFleet = FDeadAtAgeArea.n_cols;
    
    
    arma::mat selectivity = SelectivityAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1); // age, fleet
    arma::mat retention = RetentionAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1); // age, fleet
    arma::mat discardmort = DiscardMortalityAtAge.subcube(0, TSindex, 0, nAge-1, TSindex, nFleet-1); // age, fleet
    
    if (nArea<2) {
 
      arma::mat temp = sum(FDeadAtAgeArea(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)),2);
      FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = temp;
      
      temp = sum(FRetainAtAgeArea(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)),2);
      FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = temp;
      
      FDeadAtAgeList[st] = FDeadAtAge;
      FRetainAtAgeList[st] = FRetainAtAge;
      continue;
    }
    
    // solve for F
    arma::mat NatAgeArea = NumberAtAgeArea(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nArea-1));
    arma::vec NumberAtAge = sum(NatAgeArea,1);
    double TotalNumber = accu(NumberAtAge);
    
    arma::mat TotalRemovalsFleet = RemovalNumberAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1));
    arma::vec TotalRemovals = sum(TotalRemovalsFleet,0);
    
    LogicalVector ZeroCatch(nFleet);
    ZeroCatch = TotalRemovals < 1E-4;
    
    if ((all(ZeroCatch).is_true())) {
      // no catches from any fleets
      continue;
    }
    
    // check if F same in all areas 
    LogicalVector IdenticalF(nFleet);
    for (int fl=0; fl<nFleet; fl++) {
       arma::mat fdeadfleet = FDeadAtAgeArea(arma::span(0, nAge-1), arma::span(fl,fl), arma::span(0,nArea-1));
       NumericMatrix FDeadFleet = as<NumericMatrix>(Rcpp::wrap(fdeadfleet));
       int same = 0;
       for (int area=1; area<nArea; area++) {
         NumericVector sameAge = FDeadFleet(_,0) / FDeadFleet(_,area);
         same += sum(sameAge);
       }
       if (same == nAge) {
         IdenticalF(fl) = TRUE;
       }
     
    }
    
    if ((all(IdenticalF).is_true())) {
      // identical F across areas for all fleets
      FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = 
        FDeadAtAgeArea(arma::span(0, nAge-1),arma::span(0, nFleet-1),   arma::span(0,0)); 
      FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = 
        FRetainAtAgeArea(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)); 
      
      // NumericVector FDeadAtAgeNamed = as<NumericVector>(Rcpp::wrap(FDeadAtAge));
      // FDeadAtAgeNamed.attr("dim") = dims;
      // FDeadAtAgeNamed.attr("dimnames") = dimnames;
      // 
      // NumericVector FRetainAtAgeNamed = as<NumericVector>(Rcpp::wrap(FRetainAtAge));
      // FRetainAtAgeNamed.attr("dim") = dims;
      // FRetainAtAgeNamed.attr("dimnames") = dimnames;
      
      FDeadAtAgeList[st] = FDeadAtAge;
      FRetainAtAgeList[st] = FRetainAtAge;

      continue;
    }
    
  
    // solve for overall F 
    
    // initial guess at apicalF
    arma::vec apicalF(nFleet);
    for (int fl=0; fl<nFleet; fl++) {
      apicalF(fl) = TotalRemovals[fl]/TotalNumber;
    }
    
    for (int i=0; i<MaxIt; i++) {
      // Calculate F-at-age fleet
      for (int fl=0; fl<nFleet; fl++) {
        arma::vec FInteract = apicalF(fl) * selectivity.col(fl);
        arma::vec FRetain = FInteract % retention.col(fl);
        arma::vec FDiscard = FInteract - FRetain;
        arma::vec FDeadDiscard = FDiscard % discardmort.col(fl);
        arma::vec FDead = FDeadDiscard + FRetain;
        FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl)) = FRetain;
        FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl)) = FDead;
      }

      arma::mat FDeadAtAgeFleet = FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1));
      arma::vec ZatAge = sum(FDeadAtAgeFleet,1) + NaturalMortalityAtAge.col(TSindex);
      arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % NumberAtAge;
      
      // derivative of catch wrt ft
      arma::vec dct(nFleet);
      arma::vec predRemovals(nFleet);
      for (int fl=0; fl<nFleet; fl++) {
        arma::vec FDead = FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl));
        predRemovals(fl) = arma::accu(FDead/ZatAge % (1-exp(-ZatAge)) % NumberAtAge);
        arma::vec temp = PopDeadAtAge / ZatAge - ((FDead % PopDeadAtAge / pow(ZatAge,2))) + FDead/ZatAge % exp(-ZatAge)%NumberAtAge;
        dct(fl) = accu(temp);
      }
      
      apicalF = apicalF - (predRemovals - TotalRemovals)/(0.8*dct);
      NumericVector diff = as<NumericVector>(Rcpp::wrap((predRemovals - TotalRemovals)));
      NumericVector totalremovals = as<NumericVector>(Rcpp::wrap((TotalRemovals)));
      
      LogicalVector converge = (Rcpp::abs(diff)/totalremovals) < tolF;
      if (all(converge).is_true())
        break;
    }

    // NumericVector FDeadAtAgeNamed = as<NumericVector>(Rcpp::wrap(FDeadAtAge));
    // FDeadAtAgeNamed.attr("dim") = dims;
    // FDeadAtAgeNamed.attr("dimnames") = dimnames;
    // 
    // NumericVector FRetainAtAgeNamed = as<NumericVector>(Rcpp::wrap(FRetainAtAge));
    // FRetainAtAgeNamed.attr("dim") = dims;
    // FRetainAtAgeNamed.attr("dimnames") = dimnames;
    
    FDeadAtAgeList[st] = FDeadAtAge;
    FRetainAtAgeList[st] = FRetainAtAge;
    
  }

  List L = List::create(Named("FDeadAtAge")=FDeadAtAgeList,
                        Named("FRetainAtAge") = FRetainAtAgeList
  );

  return(L);
}


// [[Rcpp::export]]
List CalcSpawnProduction_(List SProductionList,
                          List SBiomassList,
                          List NumberAtAgeAreaList,
                          List NaturalMortalityAtAgeList,
                          List FecundityAtAgeList,
                          List WeightAtAgeList,
                          List MaturityAtAgeList,
                          arma::vec SpawnTimeFrac,
                          List SPFromList,
                          List FDeadAtAgeList,
                          int TSindex) {

  int nStock = NaturalMortalityAtAgeList.size();
  
  for (int st=0; st<nStock; st++) {
    arma::vec SProduction = SProductionList[st]; // length time steps
    arma::vec SBiomass = SBiomassList[st]; // length time steps

    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nage, nTS, nArea
    arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
    arma::mat FecundityAtAge = FecundityAtAgeList[st]; // age, time step
    arma::mat WeightAtAge = WeightAtAgeList[st]; // age, time step
    arma::mat MaturityAtAge = MaturityAtAgeList[st]; // age, time step

    arma::cube FDeadAtAge = FDeadAtAgeList[st]; // age, time steep, fleet
    
    int nAge = NumberAtAgeArea.n_rows;
    int nArea = NumberAtAgeArea.n_slices;
    int nFleet = FDeadAtAge.n_slices;

    // Sum number over areas and F over fleets
    arma::vec NatAge(nAge);
    arma::vec FDeadatAge(nAge);
    for (int age=1; age<nAge; age++) { // assumes first age class is age 0 and doesn't spawn
      NatAge(age) = accu(NumberAtAgeArea(arma::span(age, age), arma::span(TSindex, TSindex), arma::span(0, nArea-1)));
      FDeadatAge(age) = accu(FDeadAtAge(arma::span(age, age), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)));
    }

    // Calculate SP production
    arma::vec SpawnMortality(nAge);
    arma::vec NSpawn = NatAge;
    if (SpawnTimeFrac[st] > 0) {
      SpawnMortality = (NaturalMortalityAtAge.col(TSindex) + FDeadatAge) * SpawnTimeFrac[st];
      NSpawn = NSpawn % exp(-SpawnMortality);
    }
    double Sproduction = arma::accu(NSpawn % FecundityAtAge.col(TSindex));
    double Sbiomass = arma::accu(NSpawn % WeightAtAge.col(TSindex) % MaturityAtAge.col(TSindex));
    SProduction(TSindex) = Sproduction;
    SBiomass(TSindex) = Sbiomass;
    SProductionList[st] = SProduction;
    SBiomassList[st] = SBiomass;
  }
  
  // apply SPfrom
  if (nStock>1) {
    for (int st=0; st<nStock; st++) {
      int SPFrom = SPFromList[st];
      SProductionList[st] = SProductionList[SPFrom];
      SBiomassList[st] = SBiomassList[SPFrom];
    }
  }

  List L = List::create(Named("SProduction")=SProductionList,
                        Named("SBiomass") = SBiomassList
  );
  
  return(L);

}



// [[Rcpp::export]]
NumericVector CalcRecruitment_(List SProductionList,
                               List SP0List,
                               List R0List,
                               List RecDevsList,
                               List SRRModelList,
                               List SRRParsList,
                               int TSindex) {

  int nStock = SProductionList.size();
  NumericVector Recruits(nStock);

  for (int st=0; st<nStock; st++) {
    NumericVector SProduction = SProductionList[st];
    NumericVector R0 = R0List[st];
    NumericVector SP0 = SP0List[st];
    NumericVector RecDevs = RecDevsList[st];
    Function SRRModel = SRRModelList[st];
    List SRRPars = SRRParsList[st];

    List Arglist = List::create(Named("S") = SProduction[TSindex],
                                Named("S0") = SP0[0], // uses SP0 from first time step
                                Named("R0") = R0[TSindex]);

    CharacterVector ParNames = SRRPars.names();
    CharacterVector ArglistNames(3+SRRPars.size());
    ArglistNames[0] = "S";
    ArglistNames[1] = "S0";
    ArglistNames[2] = "R0";

    for (int i=0; i<SRRPars.size(); i++) {
      NumericVector argVec = SRRPars[i];
      double arg = argVec[i];
      Arglist.push_back(arg);
      ArglistNames[3+i] = ParNames[i];
    }
    Arglist.attr("names") = ArglistNames;

    Rcpp::Environment base("package:base");
    Rcpp::Function doCall = base["do.call"];

    RObject RecruitsEQ = doCall(SRRModel, Arglist);
    Recruits[st] = as<double>(RecruitsEQ) * RecDevs[TSindex];
  }

  return(Recruits);
}


// [[Rcpp::export]]
List AddRecruits_(List NumberAtAgeAreaList,
                  arma::vec Recruits,
                  List UnfishedDistList,
                  int TSindex) {

  int nStock = NumberAtAgeAreaList.size();

  // TODO option to recruit into next time step: TSindex + 1
  // Need to check if first age class is 0 or 1;
  int AgeRec = 0; // will always be 0
  int TSRec = TSindex; // could be TSindex + 1
  
  for (int st=0; st<nStock; st++) {
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nage, nTS, nArea
    arma::cube UnfishedDist = UnfishedDistList[st]; // nage, nTS, nArea
    double recruits  = Recruits[st];
    int nArea = NumberAtAgeArea.n_slices;
    
    arma::vec recruitArea(nArea);
    for (int area=0; area<nArea; area++) {
      recruitArea(area) =recruits * arma::as_scalar(UnfishedDist(arma::span(AgeRec, AgeRec), arma::span(TSRec, TSRec), arma::span(area, area)));
    }
    NumberAtAgeArea.subcube(AgeRec, TSRec, 0, AgeRec, TSRec, nArea-1) = recruitArea;
    NumberAtAgeAreaList[st] = NumberAtAgeArea;
  }
  return(NumberAtAgeAreaList);
}




// [[Rcpp::export]]
List CalcNumberNext_(List NumberAtAgeAreaList,
                     List NaturalMortalityAtAgeList,
                     List FDeadAtAgeAreaList,
                     List SemelparousList,
                     List AgesList,
                     int TSindex) {

  int nStock = NumberAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nage, nTS, nArea
    arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // age, time step
    
    Rcpp::List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
    arma::cube FDeadAtAgeArea = FDeadAtAgeAreaStock[TSindex]; // age, fleet, area
    
    
    arma::mat Semelparous = SemelparousList[st]; // age, time step
    
    int nAge = NumberAtAgeArea.n_rows;
    int nArea = NumberAtAgeArea.n_slices;
    int nFleet = FDeadAtAgeArea.n_cols;

    arma::mat Zmortality(nAge, nArea);
    arma::mat Nnow(nAge, nArea);
    arma::mat Nnext(nAge, nArea);
    for (int area=0; area<nArea; area++) {
      arma::vec FDead = sum(FDeadAtAgeArea(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(area, area)),1);
      Zmortality.col(area) = FDead + NaturalMortalityAtAge.col(TSindex);
      arma::vec nnow = NumberAtAgeArea(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(area, area));
      Nnow.col(area) = nnow;
    }
    for (int area=0; area<nArea; area++) {  
      Nnext(0, area) = 5e-16;
      for (int age=0; age<(nAge-1); age++) {
        Nnext(age+1, area) = Nnow(age, area) * exp(-Zmortality(age, area)) * (1-Semelparous(age, TSindex));
      }
    }
    
    Rcpp::RObject ages = AgesList[st];
    bool plusgroup = ages.slot("PlusGroup");
    if (plusgroup) {
      for (int area=0; area<nArea; area++) {
        Nnext(nAge-1, area) = Nnext(nAge-1, area) + Nnow(nAge-1, area) * exp(-Zmortality(nAge-1, area)) * (1-Semelparous(nAge-1, TSindex));
      }
    }
    
    NumberAtAgeArea(arma::span(0, nAge-1), arma::span(TSindex+1, TSindex+1), arma::span(0, nArea-1)) =Nnext;
    
    NumberAtAgeAreaList[st] = NumberAtAgeArea;
  }

  return(NumberAtAgeAreaList);
}




// [[Rcpp::export]]
List MoveStock_(List NumberAtAgeAreaList,
                List MovementList,
                int TSindex) {

  int nStock = NumberAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {

    arma::cube NumberAtAgeAreaTS = NumberAtAgeAreaList[st]; // nage, nTS, nArea

    List MovementStock = MovementList[st];
    arma::cube Movement = MovementStock[TSindex]; // nage, FromArea, ToArea

    int nAge = NumberAtAgeAreaTS.n_rows;
    int nArea = NumberAtAgeAreaTS.n_slices;

    NumericVector NumberAtAgeAreaVec = as<NumericVector>(Rcpp::wrap(NumberAtAgeAreaTS(arma::span(0,nAge-1),
                                                                                    arma::span(TSindex, TSindex),
                                                                                    arma::span(0, nArea-1))));
    NumberAtAgeAreaVec.attr("dim") = Dimension(nAge, nArea);
    NumericMatrix NumberAtAgeArea = as<NumericMatrix>(NumberAtAgeAreaVec);
    
 
    NumericMatrix NumberAtAgeAreaMoved = NumberAtAgeArea;
    
    for (int age=0; age<nAge; age++) {
      arma::mat moveage = Movement.subcube(age, 0, 0, age, nArea-1, nArea-1);
      NumericMatrix movAge = as<NumericMatrix>(Rcpp::wrap(moveage));
      
      for (int toArea=0; toArea<nArea; toArea++) {
        NumericVector Narea(nArea);
        for (int fromArea=0; fromArea<nArea; fromArea++) {
          Narea(toArea) += NumberAtAgeArea(age, fromArea) * movAge(fromArea, toArea);
        }
        NumberAtAgeAreaMoved(age, toArea) = Narea(toArea);
      }
    }
    
    NumberAtAgeAreaTS(arma::span(0, nAge-1), 
                      arma::span(TSindex, TSindex),
                      arma::span(0, nArea-1)) = as<arma::mat>(NumberAtAgeAreaMoved);
    
   NumberAtAgeAreaList[st] = NumberAtAgeAreaTS;
  }

  return(NumberAtAgeAreaList);
}


// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// for a SINGLE simulation
// [[Rcpp::export]]
List CalcPopDynamics_(List OMListSim,
                      NumericVector TimeSteps) {


  List OMListSimOut = clone(OMListSim);
  List AgesList = OMListSimOut["Ages"];
  
  List Length = OMListSimOut["Length"];
  List Weight = OMListSimOut["Weight"];
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List Maturity = OMListSimOut["Maturity"];
  List Fecundity = OMListSimOut["Fecundity"];
  List SRR = OMListSimOut["SRR"];
  List SRRPars = SRR["SRRPars"];
  List SRRModel = SRR["SRRModel"];
  List Spatial = OMListSimOut["Spatial"];

  List FishingMortality = OMListSimOut["FishingMortality"];
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  List EffortList = OMListSimOut["Effort"];
  List Selectivity = OMListSimOut["Selectivity"];
  List Retention = OMListSimOut["Retention"];
  List Distribution = OMListSimOut["Distribution"];

  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];
  
  int nTS = TimeSteps.size();
  
  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);
    
    
    // Do MICE 
 
    // Biomass at beginning of this time step
    OMListSimOut["Biomass"] = CalcBiomass_(
      OMListSimOut["Biomass"],
      OMListSimOut["NumberAtAgeArea"],
      Weight["MeanAtAge"],
      TSindex
    );
    
    // VB by Area
    OMListSimOut["VBiomassArea"] = CalcVBiomass_(
      OMListSimOut["VBiomassArea"],
      OMListSimOut["NumberAtAgeArea"],
      OMListSimOut["FleetWeightAtAge"],
      Selectivity["MeanAtAge"],
      Distribution["Closure"],
      TSindex
    );
    
    // Relative VB Density by Area & Fleet
    // OMListSimOut["DensityArea"] = CalcDensity_(
    //   OMListSimOut["DensityArea"],
    //   OMListSimOut["VBiomassArea"],
    //   Spatial["RelativeSize"],
    //   TSindex
    // );

    // Distribute Effort over Areas (currently proportional to VB)
    OMListSimOut["EffortArea"] = DistEffort_(
      OMListSimOut["EffortArea"],
      OMListSimOut["VBiomassArea"],
      EffortList["Effort"],
      TSindex
    );

    // F within each Area
    List FArea = CalcFArea_(
      OMListSimOut["FDeadAtAgeArea"],
      OMListSimOut["FRetainAtAgeArea"],
      OMListSimOut["EffortArea"],
      Spatial["RelativeSize"],
      EffortList["Catchability"],
      Selectivity["MeanAtAge"],
      Retention["MeanAtAge"],
      DiscardMortality["MeanAtAge"],
      TSindex
    );

    OMListSimOut["FDeadAtAgeArea"] = FArea["FDeadAtAgeArea"];
    OMListSimOut["FRetainAtAgeArea"] = FArea["FRetainAtAgeArea"];

    // Removals and Retained Number and Biomass by Area
    List CatchList = CalcCatch_(
      OMListSimOut["RemovalAtAgeArea"],
      OMListSimOut["RetainAtAgeArea"],
      OMListSimOut["RemovalNumberAtAge"],
      OMListSimOut["RetainNumberAtAge"],
      OMListSimOut["RemovalBiomassAtAge"],
      OMListSimOut["RetainBiomassAtAge"],
      NaturalMortality["MeanAtAge"],
      OMListSimOut["FleetWeightAtAge"],
      OMListSimOut["NumberAtAgeArea"],
      OMListSimOut["FDeadAtAgeArea"],
      OMListSimOut["FRetainAtAgeArea"],
      TSindex
    );

    OMListSimOut["RemovalAtAgeArea"] = CatchList["RemovalAtAgeArea"];
    OMListSimOut["RetainAtAgeArea"] = CatchList["RetainAtAgeArea"];
    OMListSimOut["RemovalNumberAtAge"] = CatchList["RemovalNumberAtAge"];
    OMListSimOut["RetainNumberAtAge"] = CatchList["RetainNumberAtAge"];
    OMListSimOut["RemovalBiomassAtAge"] = CatchList["RemovalBiomassAtAge"];
    OMListSimOut["RetainBiomassAtAge"] = CatchList["RetainBiomassAtAge"];


    // Calculate F over all areas
    List Foverall = CalcFfromCatch_(
      OMListSimOut["FDeadAtAge"],
      OMListSimOut["FRetainAtAge"],
      OMListSimOut["NumberAtAgeArea"],
      OMListSimOut["RemovalNumberAtAge"],
      NaturalMortality["MeanAtAge"],
      Selectivity["MeanAtAge"],
      Retention["MeanAtAge"],
      DiscardMortality["MeanAtAge"],
      OMListSimOut["FDeadAtAgeArea"],
      OMListSimOut["FRetainAtAgeArea"],
      TSindex
    );

    OMListSimOut["FDeadAtAge"] = Foverall["FDeadAtAge"];
    OMListSimOut["FRetainAtAge"] = Foverall["FRetainAtAge"];

    // Calc Spawning Production and Spawning Biomass
    List SProductSBiomass = CalcSpawnProduction_(
      OMListSimOut["SProduction"],
      OMListSimOut["SBiomass"],
      OMListSimOut["NumberAtAgeArea"],
      NaturalMortality["MeanAtAge"],
      Fecundity["MeanAtAge"],
      Weight["MeanAtAge"],
      Maturity["MeanAtAge"],
      SRR["SpawnTimeFrac"],
      SRR["SPFrom"],
      OMListSimOut["FDeadAtAge"],
      TSindex
    );

    OMListSimOut["SProduction"] = SProductSBiomass["SProduction"];
    OMListSimOut["SBiomass"] = SProductSBiomass["SBiomass"];

	  // Calc Recruitment
	  NumericVector Recruits = CalcRecruitment_(
	    OMListSimOut["SProduction"],
      OMListSimOut["SP0"],
      SRR["R0"],
      SRR["RecDevs"],
      SRR["SRRModel"],
      SRR["SRRPars"],
      TSindex
	   );

     // Add Recruits
     // TODO option to add to beginning of next time-step i.e age 1
     OMListSimOut["NumberAtAgeArea"] = AddRecruits_(
       OMListSimOut["NumberAtAgeArea"],
       Recruits,
       Spatial["UnfishedDist"],
       TSindex
     );


    if (timestep<(nTS-1)) {

      // Update Number beginning of next Time Step
      OMListSimOut["NumberAtAgeArea"] = CalcNumberNext_(
        OMListSimOut["NumberAtAgeArea"],
        NaturalMortality["MeanAtAge"],
        OMListSimOut["FDeadAtAgeArea"],
        Maturity["Semelparous"],
		    AgesList,
		    TSindex
		  );

      // Move Population at beginning of next Time Step
      OMListSimOut["NumberAtAgeArea"] = MoveStock_(
        OMListSimOut["NumberAtAgeArea"],
        Spatial["Movement"],
        TSindex+1
      );
    }
  }

  return(OMListSimOut);
}

