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

int MatchTimeStep_(NumericVector vec,
                   CharacterVector TimeStep) {
  List dimnames = vec.attr("dimnames");
  IntegerVector TSMatch = match(TimeStep, as<CharacterVector>(dimnames["Time Step"]));
  int TSindex = TSMatch[0]-1;
  return(TSindex);
}

// [[Rcpp::export]]
int GetIndex_(IntegerVector dimSizes, IntegerVector dimIndices) {
  
  int n = dimSizes.length();
  for (int i=0; i<n; i++) {
    if (dimIndices[i]>=dimSizes[i]) 
      Rcpp::stop("`dimIndices` is greater than `dimSizes`");
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
NumericVector CalcBiomass_(NumericVector BiomassArea,
                           NumericVector NumberAtAgeArea,
                           NumericVector WeightAtAge,
                           int TSindex) {
  

  // Note nTime Steps may be longer for NumberAtAgeArea
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SAT = CalcDims_(WeightAtAge); 
  IntegerVector STR = CalcDims_(BiomassArea); 
  
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nArea = SATR[3];
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      IntegerVector STRindex = IntegerVector::create(st, TSindex, area);
      for(int age=0; age<nAge; age++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        BiomassArea[GetIndex_(STR, STRindex)] +=
          NumberAtAgeArea[GetIndex_(SATR, SATRindex)] *
          WeightAtAge[GetIndex_(SAT, SATindex)];
      }
    }
  }
  
  return(BiomassArea);
}

// [[Rcpp::export]]
NumericVector CalcVBiomass_(NumericVector VBiomassArea,
                            NumericVector NumberAtAgeArea,
                            NumericVector FleetWeightAtAge,
                            NumericVector SelectivityAtAge,
                            NumericVector ClosureArea,
                            int TSindex) {
  
  // Note nTime Steps may be longer for NumberAtAgeArea
  IntegerVector STFR = CalcDims_(VBiomassArea); // stock, time step, fleet, area
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SATF = CalcDims_(FleetWeightAtAge);// stock, age, time step, fleet
  IntegerVector STRF = CalcDims_(ClosureArea);// stock, time step, fleet, area
  
  int nStock = STFR[0];
  int nAge = SATR[1];
  int nFleet = STFR[2];
  int nArea = STFR[3];
  
  for (int st=0; st<nStock; st++) {
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          VBiomassArea[GetIndex_(STFR, STFRindex)] += 
            NumberAtAgeArea[GetIndex_(SATR, SATRindex)] *
            FleetWeightAtAge[GetIndex_(SATF, SATFindex)] * SelectivityAtAge[GetIndex_(SATF, SATFindex)] *
            ClosureArea[GetIndex_(STFR, STFRindex)];
        }
      }
    }
  }
  
  return(VBiomassArea);
}



// [[Rcpp::export]]
NumericVector CalcDensity_(NumericVector DensityArea, 
                           NumericVector VBiomassArea,
                           NumericVector RelativeSize, 
                           int TSindex) {
  
  IntegerVector STFR = CalcDims_(VBiomassArea); // stock, time step, fleet, area
  IntegerVector SR = STFR[IntegerVector{0,3}];
  IntegerVector SF = STFR[IntegerVector{0,2}];
  int nStock = STFR[0];
  int nFleet = STFR[2];
  int nArea = STFR[3];
  
  NumericVector DensityTotal = (nStock * nFleet);
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      IntegerVector SRindex = IntegerVector::create(st, area);
      for (int fl=0; fl<nFleet; fl++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        DensityArea[GetIndex_(STFR, STFRindex)] = VBiomassArea[GetIndex_(STFR, STFRindex)]/ RelativeSize[GetIndex_(SR, SRindex)];
      }
    }
  }
  
  for (int st=0; st<nStock; st++) {
    for (int fl=0; fl<nFleet; fl++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        IntegerVector SFindex = IntegerVector::create(st, fl);
        DensityTotal[GetIndex_(SF, SFindex)] += DensityArea[GetIndex_(STFR, STFRindex)];
      }
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        IntegerVector SFindex = IntegerVector::create(st, fl);
        if (DensityTotal[GetIndex_(SF, SFindex)]>0) {
          DensityArea[GetIndex_(STFR, STFRindex)] =  DensityArea[GetIndex_(STFR, STFRindex)]/DensityTotal[GetIndex_(SF, SFindex)];  
        } else {
          DensityArea[GetIndex_(STFR, STFRindex)] = 0;
        }
        
      }
    }
  }
        
  return(DensityArea);
}

// [[Rcpp::export]]
NumericVector DistEffort_(NumericVector EffortArea, 
                          NumericVector DensityArea, 
                          NumericVector Effort, 
                          int TSindex) {
  
  IntegerVector STFR = CalcDims_(EffortArea); // stock, time step, fleet, area
  IntegerVector STF = STFR[IntegerVector{0,1,2}]; // stock, time step, fleet
  
  int nStock = STFR[0];
  int nFleet = STFR[2];
  int nArea = STFR[3];
  
  for (int st=0; st<nStock; st++) {
    for (int fl=0; fl<nFleet; fl++) {
      IntegerVector STFindex = IntegerVector::create(st, TSindex, fl);
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        EffortArea[GetIndex_(STFR, STFRindex)] = Effort[GetIndex_(STF, STFindex)] *  DensityArea[GetIndex_(STFR, STFRindex)];
      }
    }
  }
  

  return(EffortArea);
}

List CalcFArea_(NumericVector EffortArea,
                NumericVector DensityArea,
                NumericVector Catchability,
                NumericVector SelectivityAtAge,
                NumericVector RetentionAtAge,
                NumericVector DiscardMortalityAtAge,
                NumericVector FDeadAtAgeArea,
                NumericVector FRetainAtAgeArea,
                int TSindex) {
  
  IntegerVector SATFR = CalcDims_(FDeadAtAgeArea); // stock, age, time step, fleet, area
  IntegerVector STFR = SATFR[IntegerVector{0,2,3,4}];
  IntegerVector SATF = SATFR[IntegerVector{0,1,2,3}];
  IntegerVector STF = SATFR[IntegerVector{0,2,3}]; 
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  // Calculate fishing mortality by fleet and area 
  for (int st=0; st<nStock; st++) {
    for (int fl=0; fl<nFleet; fl++) {
      IntegerVector STFindex = IntegerVector::create(st, TSindex, fl);
      double catchability = Catchability[GetIndex_(STF, STFindex)];
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        double effort = EffortArea[GetIndex_(STFR, STFRindex)];
        double catchabilityArea = 0;
        if (DensityArea[GetIndex_(STFR, STFRindex)]>0) {
          catchabilityArea = catchability/DensityArea[GetIndex_(STFR, STFRindex)];  
        }
        for(int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          double FInteract = effort * catchabilityArea * SelectivityAtAge[GetIndex_(SATF, SATFindex)];
          FRetainAtAgeArea[GetIndex_(SATFR, SATFRindex)] = FInteract * RetentionAtAge[GetIndex_(SATF, SATFindex)];
          double FDiscardTotal = FInteract - FRetainAtAgeArea[GetIndex_(SATFR, SATFRindex)];
          double FDiscardDead = FDiscardTotal * DiscardMortalityAtAge[GetIndex_(SATF, SATFindex)];
          FDeadAtAgeArea[GetIndex_(SATFR, SATFRindex)] = FDiscardDead + FRetainAtAgeArea[GetIndex_(SATFR, SATFRindex)];
        }
      }
    }
  }
  List L = List::create(Named("FDeadAtAgeArea")=FDeadAtAgeArea,
                        Named("FRetainAtAgeArea") = FRetainAtAgeArea); 
  return(L);
}

// [[Rcpp::export]]
List CalcCatch_(NumericVector NaturalMortalityAtAge,
                NumericVector SelectivityAtAge,
                NumericVector RetentionAtAge,
                NumericVector DiscardMortalityAtAge,
                NumericVector FleetWeightAtAge,
                NumericVector EffortArea,
                NumericVector Catchability,
                NumericVector DensityArea,
                NumericVector NumberAtAgeArea,
                NumericVector FDeadAtAgeArea,
                NumericVector FRetainAtAgeArea,
                NumericVector RemovalAtAgeArea,
                NumericVector RetainAtAgeArea,
                NumericVector RemovalNumberAtAge,
                NumericVector RetainNumberAtAge,
                NumericVector RemovalBiomassAtAge,
                NumericVector RetainBiomassAtAge,
                int TSindex) {
  
  // Note nTime Steps may be longer for NumberAtAgeArea
  IntegerVector SATFR = CalcDims_(FDeadAtAgeArea); // stock, age, time step, fleet, area
  IntegerVector SAT = CalcDims_(NaturalMortalityAtAge); // stock, age, time step
  IntegerVector SAR = SATFR[IntegerVector{0,1,4}];
  IntegerVector SATF = CalcDims_(SelectivityAtAge); // stock, age, time step, fleet
  IntegerVector STF = CalcDims_(Catchability); // stock, time step, fleet
  IntegerVector STFR = CalcDims_(DensityArea); // stock, time, fleet, area
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  // Fishing mortality within each area 
  List FArea = CalcFArea_(EffortArea,
                          DensityArea,
                          Catchability,
                          SelectivityAtAge,
                          RetentionAtAge,
                          DiscardMortalityAtAge,
                          FDeadAtAgeArea,
                          FRetainAtAgeArea,
                          TSindex);
  
  FDeadAtAgeArea = FArea["FDeadAtAgeArea"];
  FRetainAtAgeArea = FArea["FRetainAtAgeArea"];
  
  
  NumericVector ZDeadTotal = (nStock * nAge * nArea); // Total Z by Area
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      for(int age=0; age<nAge; age++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        IntegerVector SARindex = IntegerVector::create(st, age, area);
        
        // Z within each Area
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          ZDeadTotal[GetIndex_(SAR, SARindex)] += FDeadAtAgeArea[GetIndex_(SATFR, SATFRindex)] + NaturalMortalityAtAge[GetIndex_(SAT, SATindex)];
        }
        
        for (int fl=0; fl<nFleet; fl++) {
          
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          
          // number
          RemovalAtAgeArea[GetIndex_(SATFR, SATFRindex)] = FDeadAtAgeArea[GetIndex_(SATFR, SATFRindex)]/ZDeadTotal[GetIndex_(SAR, SARindex)] *
            NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * (1-exp(-ZDeadTotal[GetIndex_(SAR, SARindex)]));
          
          RetainAtAgeArea[GetIndex_(SATFR, SATFRindex)] = FRetainAtAgeArea[GetIndex_(SATFR, SATFRindex)]/ZDeadTotal[GetIndex_(SAR, SARindex)] *
            NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * (1-exp(-ZDeadTotal[GetIndex_(SAR, SARindex)]));
          
          // sum over areas
          RemovalNumberAtAge[GetIndex_(SATF, SATFindex)] += RemovalAtAgeArea[GetIndex_(SATFR, SATFRindex)];
          RetainNumberAtAge[GetIndex_(SATF, SATFindex)] += RetainAtAgeArea[GetIndex_(SATFR, SATFRindex)];
          
          // biomass
          RemovalAtAgeArea[GetIndex_(SATFR, SATFRindex)] = RemovalAtAgeArea[GetIndex_(SATFR, SATFRindex)] * FleetWeightAtAge[GetIndex_(SATF, SATFindex)];
          RetainAtAgeArea[GetIndex_(SATFR, SATFRindex)] = RetainAtAgeArea[GetIndex_(SATFR, SATFRindex)] * FleetWeightAtAge[GetIndex_(SATF, SATFindex)];
          
          // sum over areas
          RemovalBiomassAtAge[GetIndex_(SATF, SATFindex)] += RemovalAtAgeArea[GetIndex_(SATFR, SATFRindex)];
          RetainBiomassAtAge[GetIndex_(SATF, SATFindex)] += RetainAtAgeArea[GetIndex_(SATFR, SATFRindex)];
        
        }
      }
    }
  }
  
  List L = List::create(Named("FDeadAtAgeArea")=FDeadAtAgeArea,
                        Named("FRetainAtAgeArea") = FRetainAtAgeArea,
                        Named("RemovalAtAgeArea") = RemovalAtAgeArea,
                        Named("RetainAtAgeArea") = RetainAtAgeArea,
                        Named("RemovalNumberAtAge") = RemovalNumberAtAge,
                        Named("RetainNumberAtAge") = RetainNumberAtAge,
                        Named("RemovalBiomassAtAge") = RemovalBiomassAtAge,
                        Named("RetainBiomassAtAge") = RetainBiomassAtAge
  ); 
  
  return(L);
}




// [[Rcpp::export]]
List CalcFfromCatch_(NumericVector FDeadAtAge,
                     NumericVector FRetainAtAge,
                     NumericVector NumberAtAgeArea,
                     NumericVector RemovalNumberAtAge,
                     NumericVector NaturalMortalityAtAge,
                     NumericVector SelectivityAtAge,
                     NumericVector RetentionAtAge,
                     NumericVector DiscardMortalityAtAge,
                     NumericVector FDeadAtAgeArea,
                     NumericVector FRetainAtAgeArea,
                     int TSindex) {

  int MaxIt = 300;
  double tolF = 1E-4;
  
  // if (control.isNotNull()){
  //   List Control(control);
  //   MaxIt      = Control["MaxIt"];
  //   tolF      = Control["tolF"];
  // } 
  
  
  // Note nTime Steps may be longer for NumberAtAgeArea
  IntegerVector nSATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, fleet, area
  IntegerVector SATFR = CalcDims_(FDeadAtAgeArea); // stock, age, time step, fleet, area
  
  IntegerVector SATF = CalcDims_(SelectivityAtAge); // stock, age, time step, fleet
  IntegerVector SAT = SATFR[IntegerVector{0,1,2}];
  IntegerVector SAR = SATFR[IntegerVector{0,1,4}];
  IntegerVector SA = SATFR[IntegerVector{0,1}];
  IntegerVector AF = SATFR[IntegerVector{1,3}];
  
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  NumericVector FDeadTotalAtAge(nStock*nAge); // summed across fleets
    
  // 1 Area
  if (nArea<2) {
    for (int st=0; st<nStock; st++) {
      for (int fl=0; fl<nFleet; fl++) {
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SAindex = IntegerVector::create(st, age);
          for (int area=0; area<nArea; area++) {
            IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] += FDeadAtAgeArea[GetIndex_(SATFR, SATFRindex)];
            
            FRetainAtAge[GetIndex_(SATF, SATFindex)] += FRetainAtAgeArea[GetIndex_(SATFR, SATFRindex)];  
          }
          FDeadTotalAtAge[GetIndex_(SA, SAindex)] +=  FDeadAtAge[GetIndex_(SATF, SATFindex)];
        }
      }
    }
    List L = List::create(Named("FDeadAtAge")=FDeadAtAge,
                          Named("FRetainAtAge") = FRetainAtAge,
                          Named("FDeadTotalAtAge") = FDeadTotalAtAge
    ); 
    return(L);
  }
  
  // >1 Area
  NumericVector NumberAtAge(nStock*nAge); // sum over areas
  NumericVector Number(nStock); // total number in population
  NumericVector Catch(nStock); // total removals from population
  
  for (int st=0; st<nStock; st++) {
    for (int age=0; age<nAge; age++) {
      IntegerVector SAindex = IntegerVector::create(st, age);
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        NumberAtAge[GetIndex_(SA, SAindex)] += NumberAtAgeArea[GetIndex_(nSATR, SATRindex)]; 
      }
      
      Number[st] += NumberAtAge[GetIndex_(SA, SAindex)];
      for (int fl=0; fl<nFleet; fl++) {
        IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
        Catch[st] += RemovalNumberAtAge[GetIndex_(SATF, SATFindex)];
      }
      
    }
  }

  // Solve for apical F
  for (int st=0; st<nStock; st++) {
    NumericVector apicalF(nFleet);
    NumericVector FInteract(nFleet*nAge);
    NumericVector FRetain(nFleet*nAge);
    NumericVector FDiscard(nFleet*nAge);
    NumericVector FDeadDiscard(nFleet*nAge);
    NumericVector FDeadTotal(nFleet*nAge);
    NumericVector ZatAge(nAge);
    NumericVector PopDead(nAge);
    NumericVector PredictRemoval(nFleet*nAge);
    NumericVector PredictRemovalTotal(nFleet);
    NumericVector RemovalTotal(nFleet);
    NumericVector dct(nFleet);
    LogicalVector converge(nFleet);
    LogicalVector ZeroCatch(nFleet);
    
    for (int age=0; age<nAge; age++) {
      ZatAge[age] = NaturalMortalityAtAge[GetIndex_(SA, IntegerVector::create(st, age))];
    }
    
    // initial guess at apicalF
    for (int fl=0; fl<nFleet; fl++) {
      apicalF(fl) = Catch[st]/Number[st];
    }
    
    ZeroCatch = Catch[st] < 1E-4;
    if ((all(ZeroCatch).is_true())) {
      for (int fl=0; fl<nFleet; fl++) {
        
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          for (int area=0; area<nArea; area++) {
            
            FDeadAtAge[GetIndex_(SATF, SATFindex)] =  1E-15;
            FRetainAtAge[GetIndex_(SATF, SATFindex)] = 1E-15;
          }
        }
      }
      
    } else {
      // calc F by fleet for this stock
      for (int i=0; i<MaxIt; i++) {
        
        for (int fl=0; fl<nFleet; fl++) {
          for (int age=0; age<nAge; age++) {
            IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
            IntegerVector AFindex = IntegerVector::create(age, fl);
            FInteract[GetIndex_(AF, AFindex)] = apicalF(fl) * SelectivityAtAge[GetIndex_(SATF, SATFindex)];
            FRetain[GetIndex_(AF, AFindex)] = FInteract[GetIndex_(AF, AFindex)] * RetentionAtAge[GetIndex_(SATF, SATFindex)];
            FDiscard[GetIndex_(AF, AFindex)] =  FInteract[GetIndex_(AF, AFindex)] - FRetain[GetIndex_(AF, AFindex)];
            FDeadDiscard[GetIndex_(AF, AFindex)] = FDiscard[GetIndex_(AF, AFindex)] * DiscardMortalityAtAge[GetIndex_(SATF, SATFindex)];
            FDeadTotal[GetIndex_(AF, AFindex)] = FDeadDiscard[GetIndex_(AF, AFindex)] + FRetain[GetIndex_(AF, AFindex)];
            ZatAge[age] += FDeadTotal[GetIndex_(AF, AFindex)];
          }
        }
        
        for (int age=0; age<nAge; age++) {
          IntegerVector SAindex = IntegerVector::create(st, age);
          PopDead[age] = (1-exp(-ZatAge[age])) * NumberAtAge[GetIndex_(SA, SAindex)];
          for (int fl=0; fl<nFleet; fl++) {
            IntegerVector AFindex = IntegerVector::create(age, fl);
            PredictRemoval[GetIndex_(AF, AFindex)] = FDeadTotal[GetIndex_(AF, AFindex)]/ZatAge[age] *  PopDead[age];
            // derivative of predCatch wrt apicalF
            dct[fl] += PopDead[age]/ZatAge[age] - pow(( FDeadTotal[GetIndex_(AF, AFindex)] * PopDead[age])/ZatAge[age],2) +
              FDeadTotal[GetIndex_(AF, AFindex)]/ZatAge[age] * exp(-ZatAge[age]) * NumberAtAge[GetIndex_(AF, AFindex)];
          }
        }
        
        for (int fl=0; fl<nFleet; fl++) {
          for (int age=0; age<nAge; age++) {
            IntegerVector AFindex = IntegerVector::create(age, fl);
            IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
            PredictRemovalTotal[fl] += PredictRemoval[GetIndex_(AF, AFindex)];
            RemovalTotal[fl] += RemovalNumberAtAge[GetIndex_(SATF, SATFindex)];
          }
          
          apicalF[fl] = apicalF[fl] - (PredictRemovalTotal[fl] - RemovalTotal[fl])/(0.8*dct[fl]);
        }
        
        converge = Rcpp::abs((PredictRemovalTotal - RemovalTotal)/RemovalTotal) <tolF;
        
        // check for convergence
        if (all(converge).is_true())
          break;
      }
      
      // update arrays with 
      for (int fl=0; fl<nFleet; fl++) {
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SAindex = IntegerVector::create(st, age);
          for (int area=0; area<nArea; area++) {
            IntegerVector AFindex = IntegerVector::create(age, fl);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] =  FDeadTotal[GetIndex_(AF, AFindex)];
            FRetainAtAge[GetIndex_(SATF, SATFindex)] = FRetain[GetIndex_(AF, AFindex)];
          }
          FDeadTotalAtAge[GetIndex_(SA, SAindex)] += FDeadAtAge[GetIndex_(SATF, SATFindex)];
        }
      }
      
    }
  }
  

  List L = List::create(Named("FDeadAtAge")=FDeadAtAge,
                        Named("FRetainAtAge") = FRetainAtAge,
                        Named("FDeadTotalAtAge") = FDeadTotalAtAge
  ); 
  
  return(L);

}


// [[Rcpp::export]]
NumericVector CalcSpawnProduction_(NumericVector SProduction,
                                   NumericVector NaturalMortalityAtAge,
                                   NumericVector FDeadTotalAtAge, 
                                   NumericVector FecundityAtAge,
                                   NumericVector NumberAtAgeArea, 
                                   NumericVector SpawnTimeFrac,
                                   IntegerVector SPFrom,
                                   int TSindex) {

  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SAT = CalcDims_(FecundityAtAge); // stock, age, time step
  IntegerVector ST = CalcDims_(SProduction); // stock, time step
  IntegerVector SA = SATR[IntegerVector{0,1}];
  
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nArea = SATR[3];
  
  // sum Number over areas
  NumericVector NumberAtAge(nStock*nAge);
  for (int st=0; st<nStock; st++) {
    for (int age=0; age<nAge; age++) {
      IntegerVector SAindex = IntegerVector::create(st, age);
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        NumberAtAge[GetIndex_(SA, SAindex)] += NumberAtAgeArea[GetIndex_(SATR, SATRindex)];
      }
    }
  }
  
  // Calculate SP production
  for (int st=0; st<nStock; st++) {
    IntegerVector STindex = IntegerVector::create(st, TSindex);
    for (int age=0; age<nAge; age++) {
      IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
      IntegerVector SAindex = IntegerVector::create(st, age);
      if (SpawnTimeFrac[st] > 0) {
        double totalmortality =  NaturalMortalityAtAge[GetIndex_(SAT, SATindex)] + FDeadTotalAtAge[GetIndex_(SA, SAindex)];
        double NSpawn = NumberAtAge[GetIndex_(SA, SAindex)] * exp(-totalmortality*SpawnTimeFrac[st]);
        SProduction[GetIndex_(ST, STindex)] += NSpawn * FecundityAtAge[GetIndex_(SAT, SATindex)];
      } else {
        SProduction[GetIndex_(ST, STindex)] += NumberAtAge[GetIndex_(SA, SAindex)] * FecundityAtAge[GetIndex_(SAT, SATindex)];
      }
    }
    
  }
  
 

  // apply SPfrom 
  for (int st=0; st<nStock; st++) {
    IntegerVector SPto = IntegerVector::create(st, TSindex);
    IntegerVector SPfrom = IntegerVector::create(SPFrom[st]-1, TSindex);
    SProduction[GetIndex_(ST, SPto)] = SProduction[GetIndex_(ST, SPfrom)];
  }

  return(SProduction);
}


// [[Rcpp::export]]
NumericVector CalcRecruitment_(NumericVector SProduction,
                               NumericVector R0,
                               NumericVector SP0,
                               NumericVector RecDevs,
                               List SRRModel,
                               List SRRPars,
                               int TSindex) {
  
  IntegerVector ST = CalcDims_(SProduction); // stock, time step
  int nStock = ST[0];
  NumericVector Recruits(nStock);
  
  for (int st=0; st<nStock; st++) {
    IntegerVector STindex = IntegerVector::create(st, TSindex);
    List srrparsList = SRRPars[st];
    Function srrModel = SRRModel[st];
    
    List Arglist = List::create(Named("S") = SProduction[GetIndex_(ST, STindex)],
                                Named("S0") = SP0[st],
                                Named("R0") = R0[GetIndex_(ST, STindex)]);
    
    CharacterVector ParNames = srrparsList.names();
    CharacterVector ArglistNames(3+srrparsList.size());
    ArglistNames[0] = "S";
    ArglistNames[1] = "S0";
    ArglistNames[2] = "R0";
 
    for (int i=0; i<srrparsList.size(); i++) {
      NumericVector argVec = srrparsList[i];
      IntegerVector SimT = CalcDims_(argVec);
      double arg = argVec[i];
      Arglist.push_back(arg);
      ArglistNames[3+i] = ParNames[i];
    }
  
    Arglist.attr("names") = ArglistNames;
    
    Rcpp::Environment base("package:base"); 
    Rcpp::Function doCall = base["do.call"]; 
    
    RObject RecruitsEQ = doCall(srrModel, Arglist);
    Recruits[st] = as<double>(RecruitsEQ) * RecDevs[GetIndex_(ST, STindex)];
  }
  return(Recruits);
}

// [[Rcpp::export]]
NumericVector AddRecruits_(NumericVector NumberAtAgeArea,
                           NumericVector Recruits,
                           NumericVector UnfishedDist,
                           int TSindex) {
  
  // TODO option to recruit into next time step: TSindex + 1
  // Need to check if first age class is 0 or 1; 
  
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, region
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nTS = SATR[2];
  int nArea = SATR[3];
  
  IntegerVector SRAT {nStock, nArea, nAge, nTS};
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      IntegerVector SATRindex = IntegerVector::create(st, 0, TSindex, area);
      IntegerVector SRATindex = IntegerVector::create(st, area, 0, TSindex);
      NumberAtAgeArea[GetIndex_(SATR, SATRindex)] = Recruits[st]*UnfishedDist[GetIndex_(SRAT, SRATindex)]; 
    }
  }
  
  return(NumberAtAgeArea);
  
}

// // [[Rcpp::export]]
// NumericVector MoveStock_(NumericVector NumberAtAgeArea,
//                          NumericVector Movement,
//                          int TSindex) {
//   
//   IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, region
//   int nStock = SATR[0];
//   int nAge = SATR[1];
//   int nTS = SATR[2];
//   int nArea = SATR[3];
//   
//   IntegerVector SATRR = {nStock, nAge, nTS, nArea, nArea};
//   
//   for (int st=0; st<nStock; st++) {
//     for (int age=0; age<nAge; age++) {
//       for (int fromArea=0; fromArea<nArea; fromArea++) {
//         NumericVector temp(nArea);
//         for (int toArea=0; toArea<nArea; toArea++) {
//           IntegerVector SATFromRindex = IntegerVector::create(st, age, TSindex, fromArea);
//           IntegerVector SATYToRindex = IntegerVector::create(st, age, TSindex, toArea);
//           IntegerVector Moveindex = IntegerVector::create(st, fromArea, toArea, age, TSindex);
//           
//           temp[toArea] += NumberAtAgeArea[GetIndex_(SATR, SATFromRindex)] * Movement[GetIndex_(SATRR, Moveindex)];
//         }
//         for (int toArea=0; toArea<nArea; toArea++) {
//           IntegerVector SATYToRindex = IntegerVector::create(st, age, TSindex, toArea);
//           NumberAtAgeArea[GetIndex_(SATR, SATYToRindex)] += temp[toArea];
//         }
//       }
//     }
//   }
//   
//  return(NumberAtAgeArea); 
// }

// [[Rcpp::export]]
NumericVector CalcNumberNext_(NumericVector NumberAtAgeArea,
                              NumericVector NaturalMortalityAtAge,
                              NumericVector FDeadArea,
                              NumericVector Semelparous,
                              List Ages,
                              int TSindex) {
  
  IntegerVector SATFR = CalcDims_(FDeadArea); // stock, age, time step, fleet, region
  IntegerVector SATR = SATFR[IntegerVector{0,1,2,4}];
  IntegerVector SAR = SATFR[IntegerVector{0,1,4}];
  IntegerVector SAT = SATFR[IntegerVector{0,1,2}];
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  
  // Total mortality and survival by area 
  NumericVector FMortAgeArea(nStock *nAge*nArea);
  NumericVector ZMortAgeArea(nStock *nAge*nArea);
  NumericVector Surv(nStock*nAge*nArea);
  
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      for (int age=0; age<nAge; age++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SARindex = IntegerVector::create(st, age, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);

          FMortAgeArea[GetIndex_(SAR, SARindex)] += FDeadArea[GetIndex_(SATFR, SATFRindex)];
        }
        ZMortAgeArea[GetIndex_(SAR, SARindex)] = FMortAgeArea[GetIndex_(SAR, SARindex)] + NaturalMortalityAtAge[GetIndex_(SAT, SATindex)];
        Surv[GetIndex_(SAR, SARindex)] = exp(-ZMortAgeArea[GetIndex_(SAR, SARindex)]) * (1-Semelparous[GetIndex_(SAT, SATindex)]);

      }
    }
  }

  // age and mortality
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      for (int age=0; age<(nAge-1); age++) {
        IntegerVector SATRNowindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SARNowindex = IntegerVector::create(st, age, area);
        IntegerVector SATRNextindex = IntegerVector::create(st, age+1, TSindex+1, area);
        NumberAtAgeArea[GetIndex_(SATR, SATRNextindex)] = NumberAtAgeArea[GetIndex_(SATR, SATRNowindex)] * Surv[GetIndex_(SAR, SARNowindex)];
      }
    }

    Rcpp::RObject ages = Ages[st];
    bool plusgroup = ages.slot("PlusGroup");

    if (plusgroup) {
      IntegerVector ageClasses = ages.slot("Classes");
      Rcout << "plusgroup " << std::endl;
      int LastAge = max(ageClasses);
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRNowLastAgeindex = IntegerVector::create(st, LastAge, TSindex, area);
        IntegerVector SARNowLastAgeindex = IntegerVector::create(st, LastAge, area); 
        IntegerVector SATRNextLastAgeindex = IntegerVector::create(st, LastAge, TSindex+1, area);
      

        NumberAtAgeArea[GetIndex_(SATR, SATRNextLastAgeindex)] += NumberAtAgeArea[GetIndex_(SATR, SATRNowLastAgeindex)] * 
          Surv[GetIndex_(SAR, SARNowLastAgeindex)];
      }
    }
  }

  return(NumberAtAgeArea);
}

// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// for a SINGLE simulation
// [[Rcpp::export]]
List CalcPopDynamics_(List PopulationList,
                      List FleetList,
                      CharacterVector TimeSteps) {
  
  // deep copy (for development at least)
  List PopulationListOut = clone(PopulationList);
  List FleetListOut = clone(FleetList);
  
  List AgesList = PopulationListOut["Ages"];
  List Length = PopulationListOut["Length"];
  NumericVector LengthAtAge = Length["MeanAtAge"];
  
  List Weight = PopulationListOut["Weight"];
  NumericVector WeightAtAge = Weight["MeanAtAge"];
  
  List NaturalMortality = PopulationListOut["NaturalMortality"];
  NumericVector NaturalMortalityAtAge = NaturalMortality["MeanAtAge"];
  
  List Maturity = PopulationListOut["Maturity"];
  NumericVector MaturityAtAge = Maturity["MeanAtAge"];
  NumericVector Semelparous = Maturity["Semelparous"];
  
  List Fecundity = PopulationListOut["Fecundity"];
  NumericVector FecundityAtAge = Fecundity["MeanAtAge"];
  
  List SRR = PopulationListOut["SRR"];
  List SRRPars = SRR["SRRPars"];
  List SRRModel = SRR["SRRModel"];
  NumericVector R0 = SRR["R0"];
  IntegerVector SPFrom = SRR["SPFrom"];
  NumericVector RecDevs = SRR["RecDevs"];
  NumericVector SpawnTimeFrac = SRR["SpawnTimeFrac"];
  
  NumericVector SP0 = PopulationListOut["SP0"];
  
  List Spatial = PopulationListOut["Spatial"];
  NumericVector UnfishedDist = Spatial["UnfishedDist"];
  NumericVector RelativeSize = Spatial["RelativeSize"];
  NumericVector Movement = Spatial["Movement"];
  
  
  NumericVector NumberAtAgeArea = PopulationListOut["NumberAtAgeArea"];
  NumericVector BiomassArea = PopulationListOut["BiomassArea"];
  NumericVector SProduction = PopulationListOut["SProduction"];
  
  List FishingMortality = FleetListOut["FishingMortality"];
  NumericVector ApicalF = FishingMortality["ApicalF"];
  NumericVector FDeadAtAge = FishingMortality["DeadAtAge"];
  NumericVector FRetainAtAge = FishingMortality["RetainAtAge"];
  
  List DiscardMortality = FleetListOut["DiscardMortality"];
  NumericVector DiscardMortalityAtAge = DiscardMortality["MeanAtAge"]; 
  
  List EffortList = FleetListOut["Effort"];
  NumericVector Effort = EffortList["Effort"]; 
  NumericVector Catchability = EffortList["Catchability"];
  
  List Selectivity = FleetListOut["Selectivity"];
  NumericVector SelectivityAtAge = Selectivity["MeanAtAge"];
  
  List Retention = FleetListOut["Retention"];
  NumericVector RetentionAtAge = Retention["MeanAtAge"]; 
  
  List Distribution = FleetListOut["Distribution"];
  NumericVector ClosureArea = Distribution["Closure"];
  
  NumericVector EffortArea = FleetListOut["EffortArea"];
  NumericVector DensityArea = FleetListOut["DensityArea"];
  NumericVector VBiomassArea = FleetListOut["VBiomassArea"];
  NumericVector FDeadAtAgeArea = FleetListOut["FDeadAtAgeArea"];
  NumericVector FRetainAtAgeArea = FleetListOut["FRetainAtAgeArea"];
  NumericVector RemovalAtAgeArea = FleetListOut["RemovalAtAgeArea"];
  NumericVector RetainAtAgeArea = FleetListOut["RetainAtAgeArea"];
  NumericVector RemovalNumberAtAge = FleetListOut["RemovalNumberAtAge"];
  NumericVector RetainNumberAtAge = FleetListOut["RetainNumberAtAge"];
  NumericVector RemovalBiomassAtAge = FleetListOut["RemovalBiomassAtAge"];
  NumericVector RetainBiomassAtAge = FleetListOut["RetainBiomassAtAge"];
  NumericVector FleetWeightAtAge = FleetListOut["FleetWeightAtAge"];
  
  IntegerVector SATFR = CalcDims_(NumberAtAgeArea); // stock, age, time step, region
  int nTS = SATFR[2];
  

  for (int ts=0; ts<TimeSteps.size(); ts++) {
    int TSindex = MatchTimeStep_(NumberAtAgeArea, as<CharacterVector>(TimeSteps[ts]));
    
    // Biomass by Area beginning of this time step
    BiomassArea = CalcBiomass_(BiomassArea,
                               NumberAtAgeArea,
                               WeightAtAge,
                               TSindex); 
    
    // VB by Area
    VBiomassArea = CalcVBiomass_(VBiomassArea, 
                                 NumberAtAgeArea, 
                                 FleetWeightAtAge, 
                                 SelectivityAtAge,
                                 ClosureArea,
                                 TSindex);
    
    // Relative VB Density by Area & Fleet
    DensityArea = CalcDensity_(DensityArea, 
                               VBiomassArea,
                               RelativeSize, 
                               TSindex);
    
    // Distribute Effort over Areas (currently proportional to VB Density)
    EffortArea = DistEffort_(EffortArea, DensityArea, Effort, TSindex);
    
    // Calc F and Catch Removal and Retain by Area
    List CatchList = CalcCatch_(NaturalMortalityAtAge,
                                SelectivityAtAge,
                                RetentionAtAge,
                                DiscardMortalityAtAge,
                                FleetWeightAtAge,
                                EffortArea,
                                Catchability,
                                DensityArea,
                                NumberAtAgeArea,
                                FDeadAtAgeArea,
                                FRetainAtAgeArea,
                                RemovalAtAgeArea,
                                RetainAtAgeArea,
                                RemovalNumberAtAge,
                                RetainNumberAtAge,
                                RemovalBiomassAtAge,
                                RetainBiomassAtAge,
                                TSindex);
    
    FDeadAtAgeArea = CatchList["FDeadAtAgeArea"];
    FRetainAtAgeArea = CatchList["FRetainAtAgeArea"];
    RemovalAtAgeArea = CatchList["RemovalAtAgeArea"];
    RetainAtAgeArea = CatchList["RetainAtAgeArea"];
    RemovalNumberAtAge = CatchList["RemovalNumberAtAge"];
    RetainNumberAtAge = CatchList["RetainNumberAtAge"];
    RemovalBiomassAtAge = CatchList["RemovalBiomassAtAge"];
    RetainBiomassAtAge = CatchList["RetainBiomassAtAge"];
    
    // Calculate F over all areas 
    List Foverall = CalcFfromCatch_(FDeadAtAge,
                                    FRetainAtAge,
                                    NumberAtAgeArea,
                                    RemovalNumberAtAge,
                                    NaturalMortalityAtAge,
                                    SelectivityAtAge,
                                    RetentionAtAge,
                                    DiscardMortalityAtAge,
                                    FDeadAtAgeArea,
                                    FRetainAtAgeArea,
                                    TSindex);
    
    FDeadAtAge = Foverall["FDeadAtAge"]; // by fleet
    FRetainAtAge = Foverall["FRetainAtAge"]; // by fleet
    NumericVector FDeadTotalAtAge = Foverall["FDeadTotalAtAge"]; // summed over fleets
    
    // Calc Spawning Production
    SProduction = CalcSpawnProduction_(SProduction,
                                       NaturalMortalityAtAge,
                                       FDeadTotalAtAge,
                                       FecundityAtAge,
                                       NumberAtAgeArea,
                                       SpawnTimeFrac,
                                       SPFrom,
                                       TSindex);
    
    // Calc Recruitment
    NumericVector Recruits = CalcRecruitment_(SProduction,
                                              R0,
                                              SP0,
                                              RecDevs,
                                              SRRModel,
                                              SRRPars,
                                              TSindex);
    
    Rcout << "Recruits " << Recruits<< std::endl;
    
    // Add Recruits
    // TODO option to add to beginning of next time-step i.e age 1
    NumberAtAgeArea = AddRecruits_(NumberAtAgeArea,
                                   Recruits,
                                   UnfishedDist,
                                   TSindex);


    if (ts<nTS) {
      
      // Update Number beginning of next Time Step
      NumberAtAgeArea = CalcNumberNext_(NumberAtAgeArea,
                                        NaturalMortalityAtAge,
                                        FDeadAtAgeArea,
                                        Semelparous,
                                        AgesList,
                                        TSindex);
      
      // Move Population at beginning
      // NumberAtAgeArea = MoveStock_(NumberAtAgeArea, Movement, TSindex);

    }
   

    
  }


  PopulationListOut["NumberAtAgeArea"] = NumberAtAgeArea;
  PopulationListOut["BiomassArea"] = BiomassArea;
  FleetListOut["VBiomassArea"] = VBiomassArea; 
  FleetListOut["DensityArea"] = DensityArea;
  FleetListOut["EffortArea"] = EffortArea; 
  
  
  
  List L = List::create(Named("PopulationList")=PopulationListOut,
                        Named("FleetList")=FleetListOut
  ); 
                        

  return(L);
}





