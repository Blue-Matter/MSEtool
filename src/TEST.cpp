#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
IntegerVector CalcDims_(NumericVector vec) {
  List dimnames = vec.attr("dimnames");
  int Ndim = dimnames.length();
  IntegerVector dim(Ndim);
  for (int i=0; i<Ndim; i++) {
    dim(i) = as<CharacterVector>(dimnames[i]).size();
  }
  dim.names() = dimnames.names();
  return(dim);
}







// // [[Rcpp::export]]
// int FindIndex_(NumericVector NamedArray,
//                RObject DimValue,
//                String DimName) {
//   
//   int index = 0;
//   List dimnames = NamedArray.attr("dimnames");
//   
//   if (DimName=="Fleet" | DimName=="Age" | DimName=="Area") {
//     int dimValue = as<int>(DimValue);
//     // if (DimName =="Area")
//     //   dimValue += 1;
//     return(dimValue);
//   } 
//   
//   double dimValue = as<double>(DimValue);
//   
//   if (DimName =="Area")
//     dimValue += 1;
//   
//   
//   Rcpp::Environment base("package:base");
//   Rcpp::Function asNumeric = base["as.numeric"];
//   
//   NumericVector ObjectDimension = asNumeric(dimnames[DimName]);
//   
//   int DimSize = ObjectDimension.size();
//   double FirstValue = ObjectDimension[0];
//   if (dimValue < FirstValue) {
//     return(index);
//   }
//   
//   for (int i=0; i<DimSize; i++) {
//     if (ObjectDimension[i] <= dimValue) 
//       index = i;
//   }
//   return(index);
// }
// 
// // [[Rcpp::export]]
// int GetIndex_(NumericVector NamedArray,
//               List index) {
//   
//   IntegerVector DimLens = CalcDims_(NamedArray);
//   
//   int nDim = DimLens.size();
//   int nIndex = index.size();
//   
//   if (nDim != nIndex) {
//     Rcout << "nDim: " << nDim << std::endl;
//     Rcout << "nIndex: " << nIndex << std::endl;
//     Rcpp::stop("length index != dimensions of Array");
//   }
//   
//   CharacterVector DimNames = DimLens.attr("names");
//   CharacterVector indexNames = index.attr("names");
//   LogicalVector Match(nDim);
//   
//   for (int i=0; i<nDim; i++) {
//     Match[i] = DimNames[i] == indexNames[i];
//     if (!Match[i]) {
//       Rcout << "DimName: " << DimNames[i] << std::endl;
//       Rcout << "indexName: " << indexNames[i] << std::endl;
//       Rcpp::stop("Index names do not match");
//     }
//     
//   }
//   
//   IntegerVector Index(nIndex);
//   for (int i=0; i<nDim; i++) {
//     Index[i] = FindIndex_(NamedArray, index[i], indexNames[i]);
//   }
//   
//   int out = GetDimIndex_(DimLens, Index);
//   return(out);
// }

// // [[Rcpp::export]]
// double Subset_(NumericVector NamedArray,
//                List index) {
//   
//   return(NamedArray[GetIndex_(NamedArray, index)]);
//   
// }



// [[Rcpp::export]]
int GetIndex_(IntegerVector dimSizes, IntegerVector dimIndices) {
  
  int n = dimSizes.length();
  for (int i=0; i<n; i++) {
    if (dimIndices[i]>=dimSizes[i]) {
        Rcout << "dimSizes: " << dimSizes << std::endl;
        Rcout << "dimIndices: " << dimIndices << std::endl;
        Rcpp::stop("`dimIndices` is greater than `dimSizes`");
    }
      
  }
  int x1 = dimIndices(0);
  
  int x2 = 0;
  for (int i=1; i<n; i++) {
    int prod = 1;
    for (int j=0; j<i; j++) {
      prod = prod * dimSizes(j);
    }
    x2 += dimIndices(i) * prod;
  }
  return(x1 + x2);
}



// [[Rcpp::export]]
List CalcBiomass_(List BiomassAreaList,
                   List NumberAtAgeAreaList,
                   List WeightList,
                   int TSindex) {
  
  int nStock = BiomassAreaList.size();
  
  for (int st=0; st<nStock; st++) {
    NumericVector BiomassArea = BiomassAreaList[st];
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector WeightAtAge = WeightList[st];
    
    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, area
    IntegerVector AT = ATR[IntegerVector{0,1}];
    IntegerVector TR = ATR[IntegerVector{1,2}];
    
    int nAge = ATR[0];
    int nArea = ATR[2];
    
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector ATRindex = IntegerVector::create(age, TSindex, area);
        IntegerVector ATindex = IntegerVector::create(age, TSindex);
        IntegerVector TRindex = IntegerVector::create(TSindex, area);
        
        BiomassArea[GetIndex_(TR, TRindex)] += NumberAtAgeArea[GetIndex_(ATR, ATRindex)] *
          WeightAtAge[GetIndex_(AT, ATindex)];
        
      }
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
    
    NumericVector VBiomassArea = VBiomassAreaList[st];
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector FleetWeightAtAge = FleetWeightAtAgeList[st];
    NumericVector SelectivityAtAge = SelectivityAtAgeList[st];
    NumericVector ClosureArea = ClosureAreaList[st];
    
    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age time step, area
    IntegerVector TFR = CalcDims_(VBiomassArea); // time step, fleet, area
    
    int nAge = ATR[0];
    int nTS = ATR[1];
    int nArea = ATR[2];
    int nFleet = TFR[1];
    IntegerVector ATF = IntegerVector{nAge,nTS, nFleet};
    
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector ATRindex = IntegerVector::create(age,TSindex, area);
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector TFRindex = IntegerVector::create(TSindex, fl, area);
          IntegerVector ATFindex = IntegerVector::create(age, TSindex, fl);
          VBiomassArea[GetIndex_(TFR, TFRindex)] += 
            NumberAtAgeArea[GetIndex_(ATR, ATRindex)] * 
            FleetWeightAtAge[GetIndex_(ATF, ATFindex)] *
            SelectivityAtAge[GetIndex_(ATF, ATFindex)] * 
            ClosureArea[GetIndex_(TFR, TFRindex)];
        }
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
    NumericVector DensityArea = DensityAreaList[st];
    NumericVector VBiomassArea = VBiomassAreaList[st];
    NumericVector RelativeSize = RelativeSizeList[st];
    
    IntegerVector TFR = CalcDims_(DensityArea); // time step, fleet, area
    
    int nFleet = TFR[1];
    int nArea = TFR[2];
    
    NumericVector DensityTotal(nFleet);
    for (int area=0; area<nArea; area++) {
      for (int fl=0; fl<nFleet; fl++) {
        IntegerVector TFRindex = IntegerVector::create(TSindex, fl,  area);
        DensityArea[GetIndex_(TFR, TFRindex)] = VBiomassArea[GetIndex_(TFR, TFRindex)] / 
          RelativeSize[area];
      }
    }
  
    for (int fl=0; fl<nFleet; fl++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector TFRindex = IntegerVector::create(TSindex, fl,  area);
        DensityTotal(fl) += DensityArea[GetIndex_(TFR, TFRindex)];
      }
      
      for (int area=0; area<nArea; area++) {
        IntegerVector TFRindex = IntegerVector::create(TSindex, fl,  area);
        if (DensityTotal(fl)>0) {
          DensityArea[GetIndex_(TFR, TFRindex)] =  DensityArea[GetIndex_(TFR, TFRindex)]/ 
            DensityTotal(fl);
        } else {
          DensityArea[GetIndex_(TFR, TFRindex)] = 0;
        }
      }
    }
    DensityAreaList[st] = DensityArea;
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
    NumericVector EffortArea = EffortAreaList[st];
    NumericVector DensityArea = DensityAreaList[st];
    NumericVector Effort = EffortList[st];
    
    IntegerVector TFR = CalcDims_(DensityArea); // time step, fleet, area
    int nTS = TFR[0];
    int nFleet = TFR[1];
    int nArea = TFR[2];
    IntegerVector TF{nTS, nFleet};
    
    for (int fl=0; fl<nFleet; fl++) {
      IntegerVector TFindex = IntegerVector::create(TSindex, fl);
      for (int area=0; area<nArea; area++) {
        IntegerVector TFRindex = IntegerVector::create(TSindex, fl, area);
        EffortArea[GetIndex_(TFR, TFRindex)] = Effort[GetIndex_(TF, TFindex)] * 
          DensityArea[GetIndex_(TFR, TFRindex)];
      }
    }
    EffortAreaList[st] = EffortArea;
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
    NumericVector FDeadAtAgeArea = FDeadAtAgeAreaList[st];
    NumericVector FRetainAtAgeArea = FRetainAtAgeAreaList[st];
    NumericVector EffortArea = EffortAreaList[st];
    NumericVector DensityArea = DensityAreaList[st];
    NumericVector Catchability = CatchabilityList[st];
    NumericVector SelectivityAtAge = SelectivityAtAgeList[st];
    NumericVector RetentionAtAge = RetentionAtAgeList[st];
    NumericVector DiscardMortalityAtAge = DiscardMortalityAtAgeList[st];
    
    IntegerVector ATFR = CalcDims_(FDeadAtAgeArea); // age time step, fleet, area
    
    int nAge = ATFR[0];
    int nTS = ATFR[1];
    int nFleet = ATFR[2];
    int nArea = ATFR[3];
    
    IntegerVector ATF{nAge, nTS, nFleet};
    IntegerVector TF{nTS, nFleet};
    IntegerVector TFR{nTS, nFleet, nArea};
    
    // Calculate fishing mortality by fleet and area
    for (int fl=0; fl<nFleet; fl++) {
      IntegerVector TFindex = IntegerVector::create(TSindex, fl);
      double catchability = Catchability[GetIndex_(TF, TFindex)];
      
      for (int area=0; area<nArea; area++) {
        IntegerVector TFRindex = IntegerVector::create(TSindex, fl, area); 
        double effort = EffortArea[GetIndex_(TFR, TFRindex)];
        double catchabilityArea = 0;
        double densityArea = DensityArea[GetIndex_(TFR, TFRindex)];
        if (densityArea>0) {
          catchabilityArea = catchability/densityArea;
        }
        for(int age=0; age<nAge; age++) {
          IntegerVector ATFindex = IntegerVector::create(age, TSindex, fl);
          IntegerVector ATFRindex = IntegerVector::create(age, TSindex, fl, area);
          
          double FInteract = effort * catchabilityArea * SelectivityAtAge[GetIndex_(ATF, ATFindex)];
          
          FRetainAtAgeArea[GetIndex_(ATFR, ATFRindex)] = FInteract * RetentionAtAge[GetIndex_(ATF, ATFindex)];
          double FDiscardTotal = FInteract - FRetainAtAgeArea[GetIndex_(ATFR, ATFRindex)];
          double FDiscardDead = FDiscardTotal * DiscardMortalityAtAge[GetIndex_(ATF, ATFindex)];
          FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)] = FDiscardDead + FRetainAtAgeArea[GetIndex_(ATFR, ATFRindex)];
        }
      }
    }
    
    FDeadAtAgeAreaList[st] = FDeadAtAgeArea; 
    FRetainAtAgeAreaList[st] = FRetainAtAgeArea;
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
    NumericVector RemovalAtAgeArea = RemovalAtAgeAreaList[st];
    NumericVector RetainAtAgeArea = RetainAtAgeAreaList[st];
    NumericVector RemovalNumberAtAge = RemovalNumberAtAgeList[st];
    NumericVector RetainNumberAtAge = RetainNumberAtAgeList[st];
    NumericVector RemovalBiomassAtAge = RemovalBiomassAtAgeList[st];
    NumericVector RetainBiomassAtAge = RetainBiomassAtAgeList[st];
    
    NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
    NumericVector FleetWeightAtAge = FleetWeightAtAgeList[st];
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector FDeadAtAgeArea = FDeadAtAgeAreaList[st];
    NumericVector FRetainAtAgeArea = FRetainAtAgeAreaList[st];

   
   IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age time step, area
   IntegerVector ATFR = CalcDims_(FDeadAtAgeArea); // age, time step, fleet, area
   
   int nAge = ATR[0];
   int nTS = ATR[1];
   int nArea = ATR[2];
   int nFleet = ATFR[2];
   
   IntegerVector AT{nAge, nTS};
   IntegerVector ATF{nAge, nTS, nFleet};
   

   for (int area=0; area<nArea; area++) {
     for(int age=0; age<nAge; age++) {
       // Z within each Area
       double ZDead = 0;
       for (int fl=0; fl<nFleet; fl++) {
         IntegerVector ATFRindex = IntegerVector::create(age, TSindex, fl, area);
         ZDead += FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)];
       }
       IntegerVector ATindex = IntegerVector::create(age, TSindex);
       ZDead += NaturalMortalityAtAge[GetIndex_(AT, ATindex)];
       
       IntegerVector ATRindex = IntegerVector::create(age, TSindex, area);
       
       for (int fl=0; fl<nFleet; fl++) {
         IntegerVector ATFRindex = IntegerVector::create(age, TSindex, fl, area);
         // number
         RemovalAtAgeArea[GetIndex_(ATFR, ATFRindex)] = 
           FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)] / 
           ZDead *
           NumberAtAgeArea[GetIndex_(ATR, ATRindex)] * (1-exp(-ZDead));
         
         RetainAtAgeArea[GetIndex_(ATFR, ATFRindex)] = 
           FRetainAtAgeArea[GetIndex_(ATFR, ATFRindex)] / 
           ZDead *
           NumberAtAgeArea[GetIndex_(ATR, ATRindex)] * (1-exp(-ZDead));
       }
     }
   }
   
   // sum over areas 
   for (int area=0; area<nArea; area++) {
     for(int age=0; age<nAge; age++) {
       for (int fl=0; fl<nFleet; fl++) {
         IntegerVector ATFindex = IntegerVector::create(age, TSindex, fl);
         IntegerVector ATFRindex = IntegerVector::create(age, TSindex, fl, area);
         
         RemovalNumberAtAge[GetIndex_(ATF, ATFindex)] += RemovalAtAgeArea[GetIndex_(ATFR, ATFRindex)];
         RetainNumberAtAge[GetIndex_(ATF, ATFindex)] += RetainAtAgeArea[GetIndex_(ATFR, ATFRindex)];
         
         // biomass
         RemovalBiomassAtAge[GetIndex_(ATF, ATFindex)] += RemovalAtAgeArea[GetIndex_(ATFR, ATFRindex)] * 
           FleetWeightAtAge[GetIndex_(ATF, ATFindex)];
         
         RetainBiomassAtAge[GetIndex_(ATF, ATFindex)] += RetainAtAgeArea[GetIndex_(ATFR, ATFRindex)] * 
           FleetWeightAtAge[GetIndex_(ATF, ATFindex)];
       }
     }
   }
   
   
   RemovalAtAgeAreaList[st] = RemovalAtAgeArea;
   RetainAtAgeAreaList[st] = RetainAtAgeArea;
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
    
    NumericVector FDeadAtAge = FDeadAtAgeList[st];
    NumericVector FRetainAtAge = FRetainAtAgeList[st];
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector RemovalNumberAtAge = RemovalNumberAtAgeList[st];
    NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
    NumericVector SelectivityAtAge = SelectivityAtAgeList[st];
    NumericVector RetentionAtAge = RetentionAtAgeList[st];
    NumericVector DiscardMortalityAtAge = DiscardMortalityAtAgeList[st];
    NumericVector FDeadAtAgeArea = FDeadAtAgeAreaList[st];
    NumericVector FRetainAtAgeArea = FRetainAtAgeAreaList[st];
    
    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age time step, area
    IntegerVector ATFR = CalcDims_(SelectivityAtAge); // time step, fleet, area
    
    int nAge = ATR[0];
    int nTS = ATR[1];
    int nArea = ATR[2];
    int nFleet = ATFR[2];
    
    IntegerVector ATF{nAge, nTS, nFleet};
    
    if (nArea<2) {
      for (int st=0; st<nStock; st++) {
        for (int fl=0; fl<nFleet; fl++) {
          for (int age=0; age<nAge; age++) {
            IntegerVector ATFindex = IntegerVector::create(age, TSindex, fl);
            IntegerVector ATFRindex = IntegerVector::create(age, TSindex, fl, 0);
            FDeadAtAge[GetIndex_(ATF, ATFindex)] += FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)];
            FRetainAtAge[GetIndex_(ATF, ATFindex)] += FRetainAtAgeArea[GetIndex_(ATFR, ATFRindex)];
          }
        }
      }
      
    } else {
      // solve for F 
      
      NumericVector NumberAtAge(nAge); // sum over areas
       // total number in population
       NumericVector TotalRemovals(nFleet); // total removals from population
      
      for (int age=0; age<nAge; age++) {
        for (int area=0; area<nArea; area++) {
          IntegerVector ATRindex{age, TSindex, area};
          NumberAtAge(age) += NumberAtAgeArea[GetIndex_(ATR, ATRindex)];
        }
        
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector ATFindex{age, TSindex, fl};
          TotalRemovals[fl] += RemovalNumberAtAge[GetIndex_(ATF, ATFindex)];
        }
      }
      double TotalNumber = sum(NumberAtAge); 
      
      LogicalVector ZeroCatch(nFleet);
      ZeroCatch = TotalRemovals < 1E-4;
      
      if ((all(ZeroCatch).is_true())) {
        // no catches from any fleets
        for (int fl=0; fl<nFleet; fl++) {
          for (int age=0; age<nAge; age++) {
            IntegerVector ATFindex{age, TSindex, fl};
            FDeadAtAge[GetIndex_(ATF, ATFindex)] =  1E-15;
            FRetainAtAge[GetIndex_(ATF, ATFindex)] = 1E-15;
          }
        }
      } else {
        
        NumericVector apicalF(nFleet);
        // initial guess at apicalF
        for (int fl=0; fl<nFleet; fl++) {
          apicalF(fl) = TotalRemovals[fl]/TotalNumber;
        }
        
        for (int i=0; i<MaxIt; i++) {
          NumericVector ZatAge(nAge);
          NumericVector PopDeadAtAge(nAge);
          
          for (int age=0; age<nAge; age++) {
            for (int fl=0; fl<nFleet; fl++) {
              IntegerVector ATFindex{age, TSindex, fl};
              
              double FInteract;
              double FDiscard;
              double FDeadDiscard;
              FInteract = apicalF(fl) * SelectivityAtAge[GetIndex_(ATF, ATFindex)];
              FRetainAtAge[GetIndex_(ATF, ATFindex)] = FInteract * RetentionAtAge[GetIndex_(ATF, ATFindex)];
              FDiscard =  FInteract - FRetainAtAge[GetIndex_(ATF, ATFindex)];
              FDeadDiscard = FDiscard * DiscardMortalityAtAge[GetIndex_(ATF, ATFindex)];
              FDeadAtAge[GetIndex_(ATF, ATFindex)] = FDeadDiscard + FRetainAtAge[GetIndex_(ATF, ATFindex)]; 
              ZatAge[age] += FDeadAtAge[GetIndex_(ATF, ATFindex)];
            }
          }
          
          NumericVector dct(nFleet);
          NumericVector PredictRemoval(nFleet);
          NumericVector RemovalTotal(nFleet);
          
          for (int fl=0; fl<nFleet; fl++) {
            for (int age=0; age<nAge; age++) {
              IntegerVector ATFindex{age, TSindex, fl};
              PopDeadAtAge[age] = (1-exp(-ZatAge[age])) * NumberAtAge[age];
              
              dct[fl] += PopDeadAtAge[age] / 
                ZatAge[age] - 
                pow((FDeadAtAge[GetIndex_(ATF, ATFindex)] * PopDeadAtAge[age])/ZatAge[age],2) +
                FDeadAtAge[GetIndex_(ATF, ATFindex)]/ZatAge[age] * exp(-ZatAge[age]) * NumberAtAge[age];
              
              PredictRemoval[fl] += FDeadAtAge[GetIndex_(ATF, ATFindex)]/ZatAge[age] * PopDeadAtAge[age];
              RemovalTotal[fl] += RemovalNumberAtAge[GetIndex_(ATF, ATFindex)];
            }
            apicalF[fl] = apicalF[fl] - (PredictRemoval[fl] - RemovalTotal[fl])/(0.8*dct[fl]);
          }
          
          LogicalVector converge = Rcpp::abs((PredictRemoval - RemovalTotal)/RemovalTotal) <tolF;
          if (all(converge).is_true())
            break;
        }
      }
    }
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
                          List NumberAtAgeAreaList,
                          List NaturalMortalityAtAgeList,
                          List FecundityAtAgeList,
                          NumericVector SpawnTimeFrac,
                          List SPFromList,
                          List FDeadAtAgeList, 
                          int TSindex) {
  
  int nStock = NaturalMortalityAtAgeList.size();

  for (int st=0; st<nStock; st++) {
    NumericVector SProduction = SProductionList[st];
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
    NumericVector FecundityAtAge = FecundityAtAgeList[st];
    NumericVector FDeadAtAge = FDeadAtAgeList[st];
    
    IntegerVector ATF = CalcDims_(FDeadAtAge); // age, time step, fleet
    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, area
    
    int nAge = ATF[0];
    int nTS = ATF[1];
    int nFleet = ATF[2];
    int nArea = ATR[2];
    
    IntegerVector AT{nAge, nTS};
    
    // Sum number over areas and F over fleets
    NumericVector NumberAtAge(nAge);
    NumericVector FDeadTotalAtAge(nAge);
    for (int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector ATRindex{age, TSindex, area};
        NumberAtAge[age] += NumberAtAgeArea[GetIndex_(ATR, ATRindex)];
      }

      for(int fl=0; fl<nFleet; fl++) {
        IntegerVector ATFindex{age, TSindex, fl};
        FDeadTotalAtAge[age] += FDeadAtAge[GetIndex_(ATF, ATFindex)];
      }
    }
    
    // Calculate SP production
    for (int age=0; age<nAge; age++) {
      double NSpawn = NumberAtAge[age];
      IntegerVector ATindex{age, TSindex};
      if (SpawnTimeFrac[st] > 0) {
        double totalmortality =  NaturalMortalityAtAge[GetIndex_(AT, ATindex)] + FDeadTotalAtAge[age];
        NSpawn = NSpawn * exp(-totalmortality*SpawnTimeFrac[st]);

      }
      SProduction[TSindex] += NSpawn * FecundityAtAge[GetIndex_(AT, ATindex)];
    }
  
    SProductionList[st] = SProduction;
  }
  
  // apply SPfrom
  if (nStock>1) {
    for (int st=0; st<nStock; st++) {
      int SPFrom = SPFromList[st];
      SProductionList[st] = SProductionList[SPFrom];
    }
  }

  return(SProductionList);
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
                  NumericVector Recruits,
                  List UnfishedDistList,
                  int TSindex) {

  int nStock = NumberAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector UnfishedDist = UnfishedDistList[st];

    // TODO option to recruit into next time step: TSindex + 1
    // Need to check if first age class is 0 or 1;
    
    int ageRecruit = 0;
    int TSRecruit = TSindex;
    
    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, region
    int nArea = ATR[2];
    
    for (int area=0; area<nArea; area++) {
      IntegerVector ATRindex{ageRecruit, TSRecruit, area};
      NumberAtAgeArea[GetIndex_(ATR, ATRindex)] = Recruits[st]*UnfishedDist[GetIndex_(ATR, ATRindex)];
    }
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
    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector NaturalMortalityAtAge = NaturalMortalityAtAgeList[st];
    NumericVector FDeadAtAgeArea = FDeadAtAgeAreaList[st];
    NumericVector Semelparous = SemelparousList[st];

    IntegerVector ATFR = CalcDims_(FDeadAtAgeArea); // age, time step, fleet, region
    int nAge = ATFR[0];
    int nTS = ATFR[1];
    int nFleet = ATFR[2];
    int nArea = ATFR[3];

    IntegerVector AT{nAge, nTS};
    IntegerVector ATR{nAge, nTS, nArea};

    
    NumericVector Zmortality(nAge);
    for (int age=0; age<nAge; age++) {
      IntegerVector ATindex{age, TSindex};

      for (int area=0; area<nArea; area++) {
        IntegerVector ATRnow{age, TSindex, area};
        IntegerVector ATRnext{age+1, TSindex+1, area};
        double Fmortality = 0;
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector ATFRindex{age, TSindex, fl, area};
          Fmortality += FDeadAtAgeArea[GetIndex_(ATFR, ATFRindex)];
        }
        Zmortality[age] = Fmortality + NaturalMortalityAtAge[GetIndex_(AT, ATindex)];
      }
    }
    
    for (int age=0; age<(nAge-1); age++) {
      IntegerVector ATindex{age, TSindex};
      for (int area=0; area<nArea; area++) {
        IntegerVector ATRnow{age, TSindex, area};
        IntegerVector ATRnext{age+1, TSindex+1, area};
        NumberAtAgeArea[GetIndex_(ATR, ATRnext)] = NumberAtAgeArea[GetIndex_(ATR, ATRnow)] *
          exp(-Zmortality[age]) * 
          (1-Semelparous[GetIndex_(AT, ATindex)]);
        
      }
    }
    
    Rcpp::RObject ages = AgesList[st];
    bool plusgroup = ages.slot("PlusGroup");
    if (plusgroup) {
      IntegerVector ageClasses = ages.slot("Classes");
      int LastAge = ageClasses.size()-1;
      IntegerVector ATindex{LastAge, TSindex};
      for (int area=0; area<nArea; area++) {
        IntegerVector ATRLast{LastAge, TSindex, area};
        IntegerVector ATRNextLast{LastAge, TSindex+1, area};
        NumberAtAgeArea[GetIndex_(ATR, ATRNextLast)] += NumberAtAgeArea[GetIndex_(ATR, ATRLast)] *
          exp(-Zmortality[LastAge]) * 
          (1-Semelparous[GetIndex_(AT, ATindex)]);
      }
    }

    NumberAtAgeAreaList[st] = NumberAtAgeArea;
  }

  return(NumberAtAgeAreaList);
}




// [[Rcpp::export]]
List MoveStock_(List NumberAtAgeAreaList,
                List MovementList,
                int TSindex,
                int Sim=1) {

  int nStock = NumberAtAgeAreaList.size();

  for (int st=0; st<nStock; st++) {

    NumericVector NumberAtAgeArea = NumberAtAgeAreaList[st];
    NumericVector NumberAtAgeAreaMoved = clone(NumberAtAgeArea); 
    NumericVector Movement = MovementList[st];

    IntegerVector ATR = CalcDims_(NumberAtAgeArea); // age, time step, region
    int nAge = ATR[0];
    int nTS = ATR[1];
    int nArea = ATR[2];

    IntegerVector ATRR{nAge, nTS, nArea, nArea};

    for (int age=0; age<nAge; age++) {
      for (int toArea=0; toArea<nArea; toArea++) {
        NumericVector Narea(nArea);
        IntegerVector ATRTo{age, TSindex, toArea};
        for (int fromArea=0; fromArea<nArea; fromArea++) {
          IntegerVector ATRFrom{age, TSindex, fromArea};
          IntegerVector ATFromToindex{age, TSindex, fromArea, toArea};
          
          Narea[toArea] += NumberAtAgeArea[GetIndex_(ATR, ATRFrom)] *
            Movement[GetIndex_(ATRR, ATFromToindex)];
        }
        NumberAtAgeAreaMoved[GetIndex_(ATR, ATRTo)] = Narea[toArea];
      }
    }
   NumberAtAgeAreaList[st] = NumberAtAgeAreaMoved;
  }

  return(NumberAtAgeAreaList);
}


// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// for a SINGLE simulation
// [[Rcpp::export]]
List CalcPopDynamics_(List PopulationList,
                      List FleetList,
                      NumericVector TimeSteps) {

  // deep copy (for development at least)
  List PopulationListOut = clone(PopulationList);
  List FleetListOut = clone(FleetList);

  List AgesList = PopulationListOut["Ages"];
  List Length = PopulationListOut["Length"];
  List Weight = PopulationListOut["Weight"];
  List NaturalMortality = PopulationListOut["NaturalMortality"];
  List Maturity = PopulationListOut["Maturity"];
  List Fecundity = PopulationListOut["Fecundity"];
  List SRR = PopulationListOut["SRR"];
  List SRRPars = SRR["SRRPars"];
  List SRRModel = SRR["SRRModel"];
  List Spatial = PopulationListOut["Spatial"];

  List FishingMortality = FleetListOut["FishingMortality"];
  List DiscardMortality = FleetListOut["DiscardMortality"];
  List EffortList = FleetListOut["Effort"];
  List Selectivity = FleetListOut["Selectivity"];
  List Retention = FleetListOut["Retention"];
  List Distribution = FleetListOut["Distribution"];

  int nTS = TimeSteps.size();

  for (int TSindex=0; TSindex<nTS; TSindex++) {

    // Biomass by Area beginning of this time step
    PopulationListOut["BiomassArea"] = CalcBiomass_(
      PopulationListOut["BiomassArea"],
      PopulationListOut["NumberAtAgeArea"],
      Weight["MeanAtAge"],
      TSindex
    );

    // VB by Area
    FleetListOut["VBiomassArea"] = CalcVBiomass_(
      FleetListOut["VBiomassArea"],
      PopulationListOut["NumberAtAgeArea"],
      FleetListOut["FleetWeightAtAge"],
      Selectivity["MeanAtAge"],
      Distribution["Closure"],
      TSindex
    );

    // Relative VB Density by Area & Fleet
    FleetListOut["DensityArea"] = CalcDensity_(
      FleetListOut["DensityArea"],
      FleetListOut["VBiomassArea"],
      Spatial["RelativeSize"],
      TSindex
    );

    // Distribute Effort over Areas (currently proportional to VB Density)
    FleetListOut["EffortArea"] = DistEffort_(
      FleetListOut["EffortArea"],
      FleetListOut["DensityArea"],
      EffortList["Effort"],
      TSindex
    );

    // F within each Area
    List FArea = CalcFArea_(
      FleetListOut["FDeadAtAgeArea"],
      FleetListOut["FRetainAtAgeArea"],
      FleetListOut["EffortArea"],
      FleetListOut["DensityArea"],
      EffortList["Catchability"],
      Selectivity["MeanAtAge"],
      Retention["MeanAtAge"],
      DiscardMortality["MeanAtAge"],
      TSindex
    );

    FleetListOut["FDeadAtAgeArea"] = FArea["FDeadAtAgeArea"];
    FleetListOut["FRetainAtAgeArea"] = FArea["FRetainAtAgeArea"];

    // Removals and Retained Number and Biomass by Area
    List CatchList = CalcCatch_(
      FleetListOut["RemovalAtAgeArea"],
      FleetListOut["RetainAtAgeArea"],
      FleetListOut["RemovalNumberAtAge"],
      FleetListOut["RetainNumberAtAge"],
      FleetListOut["RemovalBiomassAtAge"],
      FleetListOut["RetainBiomassAtAge"],
      NaturalMortality["MeanAtAge"],
      FleetListOut["FleetWeightAtAge"],
      PopulationListOut["NumberAtAgeArea"],
      FleetListOut["FDeadAtAgeArea"],
      FleetListOut["FRetainAtAgeArea"],
      TSindex
    );

    FleetListOut["RemovalAtAgeArea"] = CatchList["RemovalAtAgeArea"];
    FleetListOut["RetainAtAgeArea"] = CatchList["RetainAtAgeArea"];
    FleetListOut["RemovalNumberAtAge"] = CatchList["RemovalNumberAtAge"];
    FleetListOut["RetainNumberAtAge"] = CatchList["RetainNumberAtAge"];
    FleetListOut["RemovalBiomassAtAge"] = CatchList["RemovalBiomassAtAge"];
    FleetListOut["RetainBiomassAtAge"] = CatchList["RetainBiomassAtAge"];


    // Calculate F over all areas
    List Foverall = CalcFfromCatch_(
      FleetListOut["FDeadAtAge"],
      FleetListOut["FRetainAtAge"],
      PopulationListOut["NumberAtAgeArea"],
      FleetListOut["RemovalNumberAtAge"],
      NaturalMortality["MeanAtAge"],
      Selectivity["MeanAtAge"],
      Retention["MeanAtAge"],
      DiscardMortality["MeanAtAge"],
      FleetListOut["FDeadAtAgeArea"],
      FleetListOut["FRetainAtAgeArea"],
      TSindex
    );

    FleetListOut["FDeadAtAge"] = Foverall["FDeadAtAge"];
    FleetListOut["FRetainAtAge"] = Foverall["FRetainAtAge"];

    // Calc Spawning Production
    PopulationListOut["SProduction"] = CalcSpawnProduction_(
      PopulationListOut["SProduction"],
      PopulationListOut["NumberAtAgeArea"],
      NaturalMortality["MeanAtAge"],
      Fecundity["MeanAtAge"],
      SRR["SpawnTimeFrac"],
      SRR["SPFrom"],
      FleetListOut["FDeadAtAge"],
      TSindex
    );


	  // Calc Recruitment
	  NumericVector Recruits = CalcRecruitment_(
	    PopulationListOut["SProduction"],
      PopulationListOut["SP0"],
      SRR["R0"],
      SRR["RecDevs"],
      SRR["SRRModel"],
      SRR["SRRPars"],
      TSindex
	   );

     // Add Recruits
     // TODO option to add to beginning of next time-step i.e age 1
    PopulationListOut["NumberAtAgeArea"] = AddRecruits_(
      PopulationListOut["NumberAtAgeArea"],
      Recruits,
      Spatial["UnfishedDist"],
      TSindex
     );


    if (TSindex<(nTS-1)) {
   
      // Update Number beginning of next Time Step
      PopulationListOut["NumberAtAgeArea"] = CalcNumberNext_(
        PopulationListOut["NumberAtAgeArea"],
        NaturalMortality["MeanAtAge"],
        FleetListOut["FDeadAtAgeArea"],
        Maturity["Semelparous"],
		    AgesList,
		    TSindex
		  );

      // Move Population at beginning of next Time Step
      PopulationListOut["NumberAtAgeArea"] = MoveStock_(
        PopulationListOut["NumberAtAgeArea"],
        Spatial["Movement"],
        TSindex+1
      );

    }
  }

  List L = List::create(Named("PopulationList")=PopulationListOut,
                        Named("FleetList")=FleetListOut
  );


  return(L);
}











