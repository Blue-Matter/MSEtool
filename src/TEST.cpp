#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List CalcBiomass_(List BiomassAreaList,
                  List NumberAtAgeAreaList,
                  List WeightList,
                  int TSindex) {
  
  int nStock = BiomassAreaList.size();
  
  for (int st=0; st<nStock; st++) {
    arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericMatrix BiomassArea = clone(BiomassAreaList)[st];
    NumericMatrix WeightAtAge = WeightList[st];
    int nAge = NumberAtAgeArea.n_rows;
    int nArea = BiomassArea.ncol();
    NumericVector W = WeightAtAge.column(TSindex);
    arma::mat narea = NumberAtAgeArea.subcube(0, TSindex, 0, nAge-1, TSindex, nArea-1);
    NumericMatrix NArea  = as<NumericMatrix>(Rcpp::wrap(narea));
    for (int area=0; area<nArea; area++) {
      BiomassArea(TSindex, area) = sum(W * NArea(_, area));
    }
    BiomassAreaList[st] = BiomassArea;
  }
  return(BiomassAreaList);
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
    
    NumericVector out = clone(VBiomassAreaList)[st];
    List dimnames = out.attr("dimnames");
    NumericVector dims = out.attr("dim");
  
    arma::cube VBiomassArea = clone(VBiomassAreaList)[st];

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
        VBiomassArea.subcube(TSindex, fl, area, TSindex, fl, area)=  accu(NArea.col(area) %  FWeight.col(fl) % Selectivity.col(fl) *  Closure.row(fl).col(area));
      }
    }
    
    out = as<NumericVector>(Rcpp::wrap(VBiomassArea));
    out.attr("dim") = dims;
    out.attr("dimnames") = dimnames;
    
    VBiomassAreaList[st] = out;
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
    
    NumericVector out = clone(DensityAreaList)[st];
    List dimnames = out.attr("dimnames");
    NumericVector dims = out.attr("dim");
    
    arma::cube DensityArea = clone(DensityAreaList)[st]; // nTS, nFleet, nArea
    arma::cube VBiomassArea = VBiomassAreaList[st]; // nTS, nFleet, nArea
    arma::vec RelativeSize = RelativeSizeList[st]; // narea
   
    int nArea = VBiomassArea.n_slices;
    int nFleet = VBiomassArea.n_cols;

    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FleetVBArea = VBiomassArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1);
      arma::vec FleetDensityArea = FleetVBArea  / RelativeSize;
      DensityArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = FleetDensityArea/sum(FleetDensityArea);;
    }
    
    out = as<NumericVector>(Rcpp::wrap(DensityArea));
    out.attr("dim") = dims;
    out.attr("dimnames") = dimnames;
    
    DensityAreaList[st] = out;
  }
  return(DensityAreaList);
}



// [[Rcpp::export]]
List DistEffort_(List EffortAreaList,
                 List DensityAreaList,
                 List EffortList,
                 int TSindex) {

  int nStock = DensityAreaList.size();

  for (int st=0; st<nStock; st++) {
    
    NumericVector out = clone(EffortAreaList)[st];
    List dimnames = out.attr("dimnames");
    NumericVector dims = out.attr("dim");
    
    arma::cube EffortArea = clone(EffortAreaList)[st]; // nTS, nFleet, nArea
    arma::cube DensityArea = DensityAreaList[st]; // nTS, nFleet, nArea
    arma::mat Effort = EffortList[st]; // nTS, nFleet
    
    int nArea = DensityArea.n_slices;
    int nFleet = DensityArea.n_cols;
    
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec densityArea = DensityArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1);
      EffortArea.subcube(TSindex, fl, 0, TSindex, fl, nArea-1) = arma::as_scalar(Effort.row(TSindex).col(fl)) * densityArea;
    }
    
    out = as<NumericVector>(Rcpp::wrap(EffortArea));
    out.attr("dim") = dims;
    out.attr("dimnames") = dimnames;
    
    EffortAreaList[st] = out;
  }
  return(EffortAreaList);
}

// [[Rcpp::export]]
List CalcFArea_(List FDeadAtAgeAreaList,
                List FRetainAtAgeAreaList,
                List EffortAreaList,
                List DensityAreaList,
                List CatchabilityList,
                List SelectivityAtAgeList,
                List RetentionAtAgeList,
                List DiscardMortalityAtAgeList,
                int TSindex) {

  int nStock = FDeadAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {
    Rcpp::List FDeadAtAgeAreaStock =  FDeadAtAgeAreaList[st];
    Rcpp::List FRetainAtAgeAreaStock =  FRetainAtAgeAreaList[st];

    arma::cube FDeadAtAgeArea =FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
    arma::cube FRetainAtAgeArea = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea

    arma::cube EffortArea = EffortAreaList[st]; // nTS, nFleet, nArea
    arma::cube DensityArea = DensityAreaList[st]; // nTS, nFleet, nArea
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
    arma::mat effortArea = EffortArea.subcube(TSindex, 0, 0, TSindex, nFleet-1, nArea-1);
    arma::vec q = Catchability.row(TSindex);
    
    // Calculate fishing mortality by fleet and area
    for (int area=0; area<nArea; area++) {
      double catchabilityArea = 0;
      for (int fl=0; fl<nFleet; fl++) {
        double fleetdensity = arma::as_scalar(DensityArea.subcube(TSindex, fl, area, TSindex, fl, area));
        if (fleetdensity>0) {
          catchabilityArea = arma::as_scalar(q.col(fl))/fleetdensity;
        }
        arma::vec FInteract = arma::as_scalar(effortArea.col(area) * catchabilityArea) * selectivity.col(fl);  
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
     Rcpp::NumericVector ZDead(nAge);
     for (int fl=0; fl<nFleet; fl++) {
       Rcpp::NumericVector FDeadFleetArea = Rcpp::wrap(FDeadAtAgeArea.subcube(0, fl, area, nAge-1, fl, area));
       ZDead = ZDead +=FDeadFleetArea;
     }
     arma::vec zdead = Rcpp::as<arma::vec>(Rcpp::wrap(ZDead)) + NaturalMortalityAtAge.col(TSindex);
     
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

  int MaxIt = 300;
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
      FDeadAtAge(arma::span(0, nAge-1),
                 arma::span(TSindex, TSindex), 
                 arma::span(0, nFleet-1)) = FRetainAtAgeArea(
                   arma::span(0, nAge-1), 
                   arma::span(0, nFleet-1), 
                   arma::span(0,0)); 
      FRetainAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(0, nFleet-1)) = 
        FRetainAtAgeArea(arma::span(0, nAge-1), arma::span(0, nFleet-1), arma::span(0,0)); 

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
    
    Rcout << "TotalRemovals" << TotalRemovals << std::endl;

    LogicalVector ZeroCatch(nFleet);
    ZeroCatch = TotalRemovals < 1E-4;
    
    if ((all(ZeroCatch).is_true())) {
      // no catches from any fleets
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
      arma::vec ZatAge = sum(FDeadAtAgeFleet,1);
      arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % NumberAtAge;
      
      arma::vec dct(nFleet);
      arma::vec predRemovals(nFleet);
      for (int fl=0; fl<nFleet; fl++) {
        arma::vec FDead = FDeadAtAge(arma::span(0, nAge-1), arma::span(TSindex, TSindex), arma::span(fl, fl));
        arma::vec temp = PopDeadAtAge / ZatAge - pow((FDead % PopDeadAtAge / ZatAge),2) + FDead % exp(-ZatAge%NumberAtAge);
        dct(fl) = accu(temp);
        
        predRemovals(fl) = accu(FDead/ZatAge%NumberAtAge);
      }
      
      apicalF = apicalF - (predRemovals - TotalRemovals)/(0.8*dct);
      
      LogicalVector converge = Rcpp::abs(Rcpp::wrap((predRemovals - TotalRemovals)/TotalRemovals)) <tolF;
      if (all(converge).is_true())
        break;
      
    }

    FDeadAtAgeList[st] = FDeadAtAge;
    FRetainAtAgeList[st] = FRetainAtAge;
  }

  List L = List::create(Named("FDeadAtAge")=FDeadAtAgeList,
                        Named("FRetainAtAge") = FRetainAtAgeList
  );

  return(L);

}
// 
// 
//  
// 
// // [[Rcpp::export]]
// List CalcSpawnProduction_(List SProductionList,
//                           List NumberAtAgeAreaList,
//                           List NaturalMortalityAtAgeList,
//                           List FecundityAtAgeList,
//                           NumericVector SpawnTimeFrac,
//                           List SPFromList,
//                           List FDeadAtAgeList, 
//                           int TSindex) {
//   
//   int nStock = NaturalMortalityAtAgeList.size();
// 
//   for (int st=0; st<nStock; st++) {
//     NumericVector SProduction = SProductionList[st];
//     NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
//     NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
//     NumericVector FecundityAtAge = FecundityAtAgeList[st];
//     NumericVector FDeadAtAge = FDeadAtAgeList[st];
//     
//     IntegerVector ATF = CalcDims_(FDeadAtAge); // age, time step, fleet
//     IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, area
//     
//     int nAge = ATF[0];
//     int nTS = ATF[1];
//     int nFleet = ATF[2];
//     int nArea = ATR[2];
//     
//     IntegerVector AT{nAge, nTS};
//     
//     // Sum number over areas and F over fleets
//     NumericVector NumberAtAge(nAge);
//     NumericVector FDeadTotalAtAge(nAge);
//     for (int age=0; age<nAge; age++) {
//       for (int area=0; area<nArea; area++) {
//         IntegerVector ATRindex{age, TSindex, area};
//         NumberAtAge[age] += NumberAtAgeArea[GetIndex_(ATR, ATRindex)];
//       }
// 
//       for(int fl=0; fl<nFleet; fl++) {
//         IntegerVector ATFindex{age, TSindex, fl};
//         FDeadTotalAtAge[age] += FDeadAtAge[GetIndex_(ATF, ATFindex)];
//       }
//     }
//     
//     // Calculate SP production
//     for (int age=0; age<nAge; age++) {
//       double NSpawn = NumberAtAge[age];
//       IntegerVector ATindex{age, TSindex};
//       if (SpawnTimeFrac[st] > 0) {
//         double totalmortality =  NaturalMortalityAtAge[GetIndex_(AT, ATindex)] + FDeadTotalAtAge[age];
//         NSpawn = NSpawn * exp(-totalmortality*SpawnTimeFrac[st]);
// 
//       }
//       SProduction[TSindex] += NSpawn * FecundityAtAge[GetIndex_(AT, ATindex)];
//     }
//   
//     SProductionList[st] = SProduction;
//   }
//   
//   // apply SPfrom
//   if (nStock>1) {
//     for (int st=0; st<nStock; st++) {
//       int SPFrom = SPFromList[st];
//       SProductionList[st] = SProductionList[SPFrom];
//     }
//   }
// 
//   return(SProductionList);
// }
// 
//  
// // [[Rcpp::export]]
// NumericVector CalcRecruitment_(List SProductionList,
//                       List SP0List,
//                       List R0List,
//                       List RecDevsList,
//                       List SRRModelList,
//                       List SRRParsList,
//                       int TSindex) {
// 
//   int nStock = SProductionList.size();
//   NumericVector Recruits(nStock);
//   
//   for (int st=0; st<nStock; st++) {
//     NumericVector SProduction = SProductionList[st];
//     NumericVector R0 = R0List[st];
//     NumericVector SP0 = SP0List[st];
//     NumericVector RecDevs = RecDevsList[st];
//     Function SRRModel = SRRModelList[st];
//     List SRRPars = SRRParsList[st];
//     
//     List Arglist = List::create(Named("S") = SProduction[TSindex],
//                                 Named("S0") = SP0[0], // uses SP0 from first time step
//                                 Named("R0") = R0[TSindex]);
//     
//     CharacterVector ParNames = SRRPars.names();
//     CharacterVector ArglistNames(3+SRRPars.size());
//     ArglistNames[0] = "S";
//     ArglistNames[1] = "S0";
//     ArglistNames[2] = "R0";
//     
//     for (int i=0; i<SRRPars.size(); i++) {
//       NumericVector argVec = SRRPars[i];
//       double arg = argVec[i];
//       Arglist.push_back(arg);
//       ArglistNames[3+i] = ParNames[i];
//     }
//     Arglist.attr("names") = ArglistNames;
//     
//     Rcpp::Environment base("package:base");
//     Rcpp::Function doCall = base["do.call"];
//     
//     RObject RecruitsEQ = doCall(SRRModel, Arglist);
//     Recruits[st] = as<double>(RecruitsEQ) * RecDevs[TSindex];
//     
//   }
//   return(Recruits);
// }
// 
// 
// // [[Rcpp::export]]
// List AddRecruits_(List NumberAtAgeAreaList,
//                   NumericVector Recruits,
//                   List UnfishedDistList,
//                   int TSindex) {
// 
//   int nStock = NumberAtAgeAreaList.size();
// 
//   for (int st=0; st<nStock; st++) {
//     NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
//     NumericVector UnfishedDist = UnfishedDistList[st];
// 
//     // TODO option to recruit into next time step: TSindex + 1
//     // Need to check if first age class is 0 or 1;
//     
//     int ageRecruit = 0;
//     int TSRecruit = TSindex;
//     
//     IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, region
//     int nArea = ATR[2];
//     
//     for (int area=0; area<nArea; area++) {
//       IntegerVector ATRindex{ageRecruit, TSRecruit, area};
//       NumberAtAgeArea[GetIndex_(ATR, ATRindex)] = Recruits[st]*UnfishedDist[GetIndex_(ATR, ATRindex)];
//     }
//     NumberAtAgeAreaList[st] = NumberAtAgeArea;
//   }
//  
//   return(NumberAtAgeAreaList);
// 
// }
// 
// 
// // [[Rcpp::export]]
// List CalcNumberNext_(List NumberAtAgeAreaList,
//                      List NaturalMortalityAtAgeList,
//                      List FDeadAtAgeAreaList,
//                      List SemelparousList,
//                      List AgesList,
//                      int TSindex) {
// 
//   int nStock = NumberAtAgeAreaList.size();
// 
//   for (int st=0; st<nStock; st++) {
//     NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
//     NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
//     NumericVector FDeadAtAgeArea = FDeadAtAgeAreaList[st];
//     NumericVector Semelparous = SemelparousList[st];
// 
//     IntegerVector ATFR = CalcDims_(FDeadAtAgeArea); // age, time step, fleet, region
//     int nAge = ATFR[0];
//     int nTS = ATFR[1];
//     int nFleet = ATFR[2];
//     int nArea = ATFR[3];
// 
//     IntegerVector AT{nAge, nTS};
//     IntegerVector ATR{nAge, nTS, nArea};
// 
//     
//     NumericVector Zmortality(nAge);
//     for (int age=0; age<nAge; age++) {
//       IntegerVector ATindex{age, TSindex};
// 
//       for (int area=0; area<nArea; area++) {
//         IntegerVector ATRnow{age, TSindex, area};
//         IntegerVector ATRnext{age+1, TSindex+1, area};
//         double Fmortality = 0;
//         for (int fl=0; fl<nFleet; fl++) {
//           IntegerVector ATFRindex{age, TSindex, fl, area};
//           Fmortality += FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)];
//         }
//         Zmortality[age] = Fmortality + NaturalMortalityAtAge[GetIndex_(AT, ATindex)];
//       }
//     }
//     
//     for (int age=0; age<(nAge-1); age++) {
//       IntegerVector ATindex{age, TSindex};
//       for (int area=0; area<nArea; area++) {
//         IntegerVector ATRnow{age, TSindex, area};
//         IntegerVector ATRnext{age+1, TSindex+1, area};
//         NumberAtAgeArea[GetIndex_(ATR, ATRnext)] = NumberAtAgeArea[GetIndex_(ATR, ATRnow)] *
//           exp(-Zmortality[age]) * 
//           (1-Semelparous[GetIndex_(AT, ATindex)]);
//         
//       }
//     }
//     
//     Rcpp::RObject ages = AgesList[st];
//     bool plusgroup = ages.slot("PlusGroup");
//     if (plusgroup) {
//       IntegerVector ageClasses = ages.slot("Classes");
//       int LastAge = ageClasses.size()-1;
//       IntegerVector ATindex{LastAge, TSindex};
//       for (int area=0; area<nArea; area++) {
//         IntegerVector ATRLast{LastAge, TSindex, area};
//         IntegerVector ATRNextLast{LastAge, TSindex+1, area};
//         NumberAtAgeArea[GetIndex_(ATR, ATRNextLast)] += NumberAtAgeArea[GetIndex_(ATR, ATRLast)] *
//           exp(-Zmortality[LastAge]) * 
//           (1-Semelparous[GetIndex_(AT, ATindex)]);
//       }
//     }
// 
//     NumberAtAgeAreaList[st] = NumberAtAgeArea;
//   }
// 
//   return(NumberAtAgeAreaList);
// }
// 
// 
// 
// 
// // [[Rcpp::export]]
// List MoveStock_(List NumberAtAgeAreaList,
//                 List MovementList,
//                 int TSindex,
//                 int Sim=1) {
// 
//   int nStock = NumberAtAgeAreaList.size();
// 
//   for (int st=0; st<nStock; st++) {
// 
//     NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
//     NumericVector NumberAtAgeAreaMoved = clone(NumberAtAgeArea); 
//     NumericVector Movement = MovementList[st];
// 
//     IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, region
//     int nAge = ATR[0];
//     int nTS = ATR[1];
//     int nArea = ATR[2];
// 
//     IntegerVector ATRR{nAge, nTS, nArea, nArea};
// 
//     for (int age=0; age<nAge; age++) {
//       for (int toArea=0; toArea<nArea; toArea++) {
//         NumericVector Narea(nArea);
//         IntegerVector ATRTo{age, TSindex, toArea};
//         for (int fromArea=0; fromArea<nArea; fromArea++) {
//           IntegerVector ATRFrom{age, TSindex, fromArea};
//           IntegerVector ATFromToindex{age, TSindex, fromArea, toArea};
//           
//           Narea[toArea] += NumberAtAgeArea[GetIndex_(ATR, ATRFrom)] *
//             Movement[GetIndex_(ATRR, ATFromToindex)];
//         }
//         NumberAtAgeAreaMoved[GetIndex_(ATR, ATRTo)] = Narea[toArea];
//       }
//     }
//    NumberAtAgeAreaList[st] = NumberAtAgeAreaMoved;
//   }
// 
//   return(NumberAtAgeAreaList);
// }
// 
// 
// // Given N and Effort at beginning of Time Step, calculates catch etc this time
// // step and N at beginning of next time step
// // returns updated arrays
// // for a SINGLE simulation
// // [[Rcpp::export]]
// List CalcPopDynamics_(List PopulationList,
//                       List FleetList,
//                       NumericVector TimeSteps) {
// 
//   // deep copy (for development at least)
//   List PopulationListOut = clone(PopulationList);
//   List FleetListOut = clone(FleetList);
// 
//   List AgesList = PopulationListOut["Ages"];
//   List Length = PopulationListOut["Length"];
//   List Weight = PopulationListOut["Weight"];
//   List NaturalMortality = PopulationListOut["NaturalMortality"];
//   List Maturity = PopulationListOut["Maturity"];
//   List Fecundity = PopulationListOut["Fecundity"];
//   List SRR = PopulationListOut["SRR"];
//   List SRRPars = SRR["SRRPars"];
//   List SRRModel = SRR["SRRModel"];
//   List Spatial = PopulationListOut["Spatial"];
// 
//   List FishingMortality = FleetListOut["FishingMortality"];
//   List DiscardMortality = FleetListOut["DiscardMortality"];
//   List EffortList = FleetListOut["Effort"];
//   List Selectivity = FleetListOut["Selectivity"];
//   List Retention = FleetListOut["Retention"];
//   List Distribution = FleetListOut["Distribution"];
// 
//   int nTS = TimeSteps.size();
// 
//   for (int TSindex=0; TSindex<nTS; TSindex++) {
// 
//     // Biomass by Area beginning of this time step
//     PopulationListOut["BiomassArea"] = CalcBiomass_(
//       PopulationListOut["BiomassArea"],
//       PopulationListOut["NumberAtAgeArea"],
//       Weight["MeanAtAge"],
//       TSindex
//     );
// // 
// //     // VB by Area
//     FleetListOut["VBiomassArea"] = CalcVBiomass_(
//       FleetListOut["VBiomassArea"],
//       PopulationListOut["NumberAtAgeArea"],
//       FleetListOut["FleetWeightAtAge"],
//       Selectivity["MeanAtAge"],
//       Distribution["Closure"],
//       TSindex
//     );
// 
//     // Relative VB Density by Area & Fleet
//     FleetListOut["DensityArea"] = CalcDensity_(
//       FleetListOut["DensityArea"],
//       FleetListOut["VBiomassArea"],
//       Spatial["RelativeSize"],
//       TSindex
//     );
// 
//     // Distribute Effort over Areas (currently proportional to VB Density)
//     FleetListOut["EffortArea"] = DistEffort_(
//       FleetListOut["EffortArea"],
//       FleetListOut["DensityArea"],
//       EffortList["Effort"],
//       TSindex
//     );
// 
//     // F within each Area
//     List FArea = CalcFArea_(
//       FleetListOut["FDeadAtAgeArea"],
//       FleetListOut["FRetainAtAgeArea"],
//       FleetListOut["EffortArea"],
//       FleetListOut["DensityArea"],
//       EffortList["Catchability"],
//       Selectivity["MeanAtAge"],
//       Retention["MeanAtAge"],
//       DiscardMortality["MeanAtAge"],
//       TSindex
//     );
// 
//     FleetListOut["FDeadAtAgeArea"] = FArea["FDeadAtAgeArea"];
//     FleetListOut["FRetainAtAgeArea"] = FArea["FRetainAtAgeArea"];
// 
//     // Removals and Retained Number and Biomass by Area
//     List CatchList = CalcCatch_(
//       FleetListOut["RemovalAtAgeArea"],
//       FleetListOut["RetainAtAgeArea"],
//       FleetListOut["RemovalNumberAtAge"],
//       FleetListOut["RetainNumberAtAge"],
//       FleetListOut["RemovalBiomassAtAge"],
//       FleetListOut["RetainBiomassAtAge"],
//       NaturalMortality["MeanAtAge"],
//       FleetListOut["FleetWeightAtAge"],
//       PopulationListOut["NumberAtAgeArea"],
//       FleetListOut["FDeadAtAgeArea"],
//       FleetListOut["FRetainAtAgeArea"],
//       TSindex
//     );
// 
//     FleetListOut["RemovalAtAgeArea"] = CatchList["RemovalAtAgeArea"];
//     FleetListOut["RetainAtAgeArea"] = CatchList["RetainAtAgeArea"];
//     FleetListOut["RemovalNumberAtAge"] = CatchList["RemovalNumberAtAge"];
//     FleetListOut["RetainNumberAtAge"] = CatchList["RetainNumberAtAge"];
//     FleetListOut["RemovalBiomassAtAge"] = CatchList["RemovalBiomassAtAge"];
//     FleetListOut["RetainBiomassAtAge"] = CatchList["RetainBiomassAtAge"];
// 
// 
//     // Calculate F over all areas
//     List Foverall = CalcFfromCatch_(
//       FleetListOut["FDeadAtAge"],
//       FleetListOut["FRetainAtAge"],
//       PopulationListOut["NumberAtAgeArea"],
//       FleetListOut["RemovalNumberAtAge"],
//       NaturalMortality["MeanAtAge"],
//       Selectivity["MeanAtAge"],
//       Retention["MeanAtAge"],
//       DiscardMortality["MeanAtAge"],
//       FleetListOut["FDeadAtAgeArea"],
//       FleetListOut["FRetainAtAgeArea"],
//       TSindex
//     );
// 
//     FleetListOut["FDeadAtAge"] = Foverall["FDeadAtAge"];
//     FleetListOut["FRetainAtAge"] = Foverall["FRetainAtAge"];
// 
//     // Calc Spawning Production
//     PopulationListOut["SProduction"] = CalcSpawnProduction_(
//       PopulationListOut["SProduction"],
//       PopulationListOut["NumberAtAgeArea"],
//       NaturalMortality["MeanAtAge"],
//       Fecundity["MeanAtAge"],
//       SRR["SpawnTimeFrac"],
//       SRR["SPFrom"],
//       FleetListOut["FDeadAtAge"],
//       TSindex
//     );
// 
// 
// 	  // Calc Recruitment
// 	  NumericVector Recruits = CalcRecruitment_(
// 	    PopulationListOut["SProduction"],
//       PopulationListOut["SP0"],
//       SRR["R0"],
//       SRR["RecDevs"],
//       SRR["SRRModel"],
//       SRR["SRRPars"],
//       TSindex
// 	   );
// 
//      // Add Recruits
//      // TODO option to add to beginning of next time-step i.e age 1
//     PopulationListOut["NumberAtAgeArea"] = AddRecruits_(
//       PopulationListOut["NumberAtAgeArea"],
//       Recruits,
//       Spatial["UnfishedDist"],
//       TSindex
//      );
// 
// 
//     if (TSindex<(nTS-1)) {
//    
//       // Update Number beginning of next Time Step
//       PopulationListOut["NumberAtAgeArea"] = CalcNumberNext_(
//         PopulationListOut["NumberAtAgeArea"],
//         NaturalMortality["MeanAtAge"],
//         FleetListOut["FDeadAtAgeArea"],
//         Maturity["Semelparous"],
// 		    AgesList,
// 		    TSindex
// 		  );
// 
//       // Move Population at beginning of next Time Step
//       PopulationListOut["NumberAtAgeArea"] = MoveStock_(
//         PopulationListOut["NumberAtAgeArea"],
//         Spatial["Movement"],
//         TSindex+1
//       );
// 
//     }
//   }
// 
//   List L = List::create(Named("PopulationList")=PopulationListOut,
//                         Named("FleetList")=FleetListOut
//   );
// 
// 
//   return(L);
// }
// 
// 









