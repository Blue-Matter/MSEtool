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
                            int TSindex) {
  
  // Note nTime Steps may be longer for NumberAtAgeArea
  IntegerVector STFR = CalcDims_(VBiomassArea); // stock, time step, fleet, area
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SATF = CalcDims_(FleetWeightAtAge);// stock, age, time step, fleet
  
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
            FleetWeightAtAge[GetIndex_(SATF, SATFindex)] * SelectivityAtAge[GetIndex_(SATF, SATFindex)];
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

// [[Rcpp::export]]
List CalcCatch_(NumericVector NumberAtAgeArea, 
                NumericVector DensityArea,
                NumericVector NaturalMortalityAtAge,
                NumericVector FleetWeightAtAge,
                
                NumericVector EffortArea, 
                NumericVector Catchability, 
                
                NumericVector SelectivityAtAge,
                NumericVector RetentionAtAge,
                NumericVector DiscardMortalityAtAge,
                
                NumericVector FDeadArea,
                NumericVector FRetainArea,
                NumericVector RemovalArea,
                NumericVector RetainArea,
                
                NumericVector RemovalNAtAge,
                NumericVector RetainNAtAge,
                
                NumericVector RemovalBAtAge,
                NumericVector RetainBAtAge,
                
                int TSindex) {
  
  IntegerVector SATFR = CalcDims_(FDeadArea); // stock, age, time step, fleet, area
  IntegerVector SAT  = SATFR[IntegerVector{0,1,2}];
  IntegerVector SATF  = SATFR[IntegerVector{0,1,2,3}];
  IntegerVector SAR  = SATFR[IntegerVector{0,1,4}];
  IntegerVector SATR  = SATFR[IntegerVector{0,1,2,4}];
  IntegerVector STFR = SATFR[IntegerVector{0,2,3,4}];
  IntegerVector STF = SATFR[IntegerVector{0,2,3}];
  IntegerVector SFR = SATFR[IntegerVector{0,3,4}];
  IntegerVector SR = SFR[IntegerVector{0,2}];
  IntegerVector SF = SFR[IntegerVector{0,1}];
  
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  NumericVector ZDeadTotal = (nStock * nAge * nArea); // Total Z by Area
  
  for (int st=0; st<nStock; st++) {
    // Fishing mortality by area
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
          FRetainArea[GetIndex_(SATFR, SATFRindex)] = FInteract * RetentionAtAge[GetIndex_(SATF, SATFindex)];
          double FDiscardTotal = FInteract - FRetainArea[GetIndex_(SATF, SATFindex)];
          double FDiscardDead = FDiscardTotal * DiscardMortalityAtAge[GetIndex_(SATF, SATFindex)];
          
          FDeadArea[GetIndex_(SATFR, SATFRindex)] = FDiscardDead + FRetainArea[GetIndex_(SATFR, SATFRindex)];
        }
      }
    }

    
    for (int area=0; area<nArea; area++) {
      for(int age=0; age<nAge; age++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        IntegerVector SARindex = IntegerVector::create(st, age, area);
        
  
        // Z by Area
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          ZDeadTotal[GetIndex_(SAR, SARindex)] += FDeadArea[GetIndex_(SATFR, SATFRindex)] + NaturalMortalityAtAge[GetIndex_(SAT, SATindex)];
        }
        
        // Z by Area
        for (int fl=0; fl<nFleet; fl++) {
          
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          
          
          // number
          RemovalArea[GetIndex_(SATFR, SATFRindex)] = FDeadArea[GetIndex_(SATFR, SATFRindex)]/ZDeadTotal[GetIndex_(SAR, SARindex)] *
            NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * (1-exp(-ZDeadTotal[GetIndex_(SAR, SARindex)]));
          
          RetainArea[GetIndex_(SATFR, SATFRindex)] = FRetainArea[GetIndex_(SATFR, SATFRindex)]/ZDeadTotal[GetIndex_(SAR, SARindex)] *
            NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * (1-exp(-ZDeadTotal[GetIndex_(SAR, SARindex)]));
          
          RemovalNAtAge[GetIndex_(SATF, SATFindex)] += RemovalArea[GetIndex_(SATFR, SATFRindex)];
          RetainNAtAge[GetIndex_(SATF, SATFindex)] += RetainArea[GetIndex_(SATFR, SATFRindex)];
          
          // biomass
          RemovalArea[GetIndex_(SATFR, SATFRindex)] = RemovalArea[GetIndex_(SATFR, SATFRindex)] * FleetWeightAtAge[GetIndex_(SATF, SATFindex)];
          RetainArea[GetIndex_(SATFR, SATFRindex)] = RetainArea[GetIndex_(SATFR, SATFRindex)] * FleetWeightAtAge[GetIndex_(SATF, SATFindex)];
          
          RemovalBAtAge[GetIndex_(SATF, SATFindex)] += RemovalArea[GetIndex_(SATFR, SATFRindex)];
          RetainBAtAge[GetIndex_(SATF, SATFindex)] += RetainArea[GetIndex_(SATFR, SATFRindex)];
       
          
        }
      }
    }
  }

  List L = List::create(Named("FDeadArea")=FDeadArea,
                        Named("FRetainArea") = FRetainArea,
                        Named("RemovalArea") = RemovalArea,
                        Named("RetainArea") = RetainArea,
                        Named("RemovalNAtAge") = RemovalNAtAge,
                        Named("RetainNAtAge") = RetainNAtAge,
                        Named("RemovalBAtAge") = RemovalBAtAge,
                        Named("RetainBAtAge") = RetainBAtAge
  ); 
  
  return(L);
}

// [[Rcpp::export]]
List CalcFfromCatch_(NumericVector FDeadAtAge,
                     NumericVector FRetainAtAge,
                     NumericVector NumberAtAgeArea,
                     NumericVector RemovalNAtAge,
                     NumericVector NaturalMortalityAtAge,
                     
                     NumericVector SelectivityAtAge,
                     NumericVector RetentionAtAge,
                     NumericVector DiscardMortalityAtAge,
                     
                     NumericVector FDeadArea,
                     NumericVector FRetainArea,
                     
                     int TSindex) {

  int MaxIt = 300;
  double tolF = 1E-4;
  
  // if (control.isNotNull()){
  //   List Control(control);
  //   MaxIt      = Control["MaxIt"];
  //   tolF      = Control["tolF"];
  // } 
  
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  int nArea = SATR[3];
  
  IntegerVector SATF = CalcDims_(SelectivityAtAge); // stock, age, time step, fleet
  int nStock = SATF[0];
  int nAge = SATF[1];
  int nFleet = SATF[3];
  
  IntegerVector SATFR = CalcDims_(FDeadArea);
  IntegerVector SA =  SATFR[IntegerVector{0,1}];
  IntegerVector SAR =  SATFR[IntegerVector{0,1,4}]; 
  IntegerVector SAT =  SATFR[IntegerVector{0,1,3}];
  IntegerVector AF = SATFR[IntegerVector{1,3}]; 
  
  NumericVector FDeadTotalAtAge(nStock*nAge);
    
  // 1 Area
  if (nArea<2) {
    for (int st=0; st<nStock; st++) {
      for (int fl=0; fl<nFleet; fl++) {
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SAindex = IntegerVector::create(st, age);
          for (int area=0; area<nArea; area++) {
            IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] += FDeadArea[GetIndex_(SATFR, SATFRindex)];
            
            FRetainAtAge[GetIndex_(SATF, SATFindex)] += FRetainArea[GetIndex_(SATFR, SATFRindex)];  
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
        NumberAtAge[GetIndex_(SA, SAindex)] += NumberAtAgeArea[GetIndex_(SATR, SATRindex)]; 
      }
      
      Number[st] += NumberAtAge[GetIndex_(SA, SAindex)];
      for (int fl=0; fl<nFleet; fl++) {
        IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
        Catch[st] += RemovalNAtAge[GetIndex_(SATF, SATFindex)];
      }
      
    }
  }
  
  // Calculate apical F
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
      apicalF(fl) = Catch[st]/ Number[st];
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
            RemovalTotal[fl] += RemovalNAtAge[GetIndex_(SATF, SATFindex)];
          }
          
          apicalF(fl) = apicalF[fl] - (PredictRemovalTotal[fl] - RemovalTotal[fl])/(0.8*dct[fl]);
          
        }
        
        converge = Rcpp::abs((PredictRemovalTotal - RemovalTotal)/RemovalTotal) <tolF;
        
        // check for convergence
        if (all(converge).is_true())
          break;
      }
      
      
      for (int fl=0; fl<nFleet; fl++) {
        
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SAindex = IntegerVector::create(st, age);
          for (int area=0; area<nArea; area++) {
            IntegerVector AFindex = IntegerVector::create(age, fl);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] =  FDeadTotal[GetIndex_(AF, AFindex)];
            FRetainAtAge[GetIndex_(SATF, SATFindex)] = FRetain[GetIndex_(AF, AFindex)];
          }
          FDeadTotalAtAge[GetIndex_(SA, SAindex)] +=  FDeadAtAge[GetIndex_(SATF, SATFindex)];
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
                                   NumericVector FDeadAtAge, // sum over fleets
                                   NumericVector FecundityAtAge,
                                   NumericVector NumberAtAgeArea, 
                                   NumericVector SpawnTimeFrac,
                                   IntegerVector SPFrom,
                                   int TSindex) {
  // sum N over areas
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SAT = SATR[IntegerVector{0,1,2}];
  IntegerVector SAR = SATR[IntegerVector{0,1,3}];
  IntegerVector SA = SATR[IntegerVector{0,1}];
  IntegerVector ST =  SATR[IntegerVector{0,2}];
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nArea = SATR[3];
  
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
  
  
  NumericVector totalmortality(nStock*nAge);
  for (int st=0; st<nStock; st++) {
    NumericVector NSpawn(nAge);
    IntegerVector STindex = IntegerVector::create(st, TSindex);
    for (int age=0; age<nAge; age++) {
      IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
      IntegerVector SAindex = IntegerVector::create(st, age);
      if (SpawnTimeFrac[st] > 0) {
        totalmortality[GetIndex_(SA, SAindex)] =  NaturalMortalityAtAge[GetIndex_(SAT, SATindex)] +  FDeadAtAge[GetIndex_(SAT, SATindex)];
        NSpawn[age] = NumberAtAge[GetIndex_(SA, SAindex)] * exp(-totalmortality[GetIndex_(SA, SAindex)]*SpawnTimeFrac[st]);
        SProduction[GetIndex_(ST, STindex)] +=  NSpawn[age] * FecundityAtAge[GetIndex_(SAT, SATindex)];
      } else {
        SProduction[GetIndex_(ST, STindex)] += NumberAtAge[GetIndex_(SA, SAindex)] * FecundityAtAge[GetIndex_(SAT, SATindex)];
      }
    }
  }

  // SPFrom
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
                               int Sim,
                               int TSindex) {
  
  IntegerVector ST = CalcDims_(SProduction); // stock, time step
  int nStock = ST[0];
  NumericVector Recruits(nStock);
  
  for (int st=0; st<nStock; st++) {
    IntegerVector STindex = IntegerVector::create(st, TSindex);
    IntegerVector SimTindex = IntegerVector::create(Sim, TSindex);
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
      double arg = argVec[GetIndex_(SimT, SimTindex)];
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
// [[Rcpp::export]]
NumericVector MoveStock_(NumericVector NumberAtAgeArea,
                         NumericVector Movement,
                         int TSindex) {
  
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, region
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nTS = SATR[2];
  int nArea = SATR[3];
  
  IntegerVector SRRAT = {nStock, nArea, nArea, nAge, nTS};
  
  for (int st=0; st<nStock; st++) {
    for (int age=0; age<nAge; age++) {
      for (int fromArea=0; fromArea<nArea; fromArea++) {
        NumericVector temp(nArea);
        for (int toArea=0; toArea<nArea; toArea++) {
          IntegerVector SATFromRindex = IntegerVector::create(st, age, TSindex, fromArea);
          IntegerVector SATYToRindex = IntegerVector::create(st, age, TSindex, toArea);
          IntegerVector Moveindex = IntegerVector::create(st, fromArea, toArea, age, TSindex);
          
          temp[toArea] += NumberAtAgeArea[GetIndex_(SATR, SATFromRindex)] * Movement[GetIndex_(SRRAT, Moveindex)];
        }
        for (int toArea=0; toArea<nArea; toArea++) {
          IntegerVector SATYToRindex = IntegerVector::create(st, age, TSindex, toArea);
          NumberAtAgeArea[GetIndex_(SATR, SATYToRindex)] += temp[toArea];
        }
      }
    }
  }
  
 return(NumberAtAgeArea); 
}

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
  
  List WeightList = PopulationListOut["Weight"];
  NumericVector WeightAtAge = WeightList["MeanAtAge"];
  
  NumericVector NumberAtAgeArea = PopulationListOut["NumberAtAgeArea"];
  NumericVector BiomassArea = PopulationListOut["BiomassArea"];
  
  List SpatialList = PopulationListOut["Spatial"];
  NumericVector RelativeSize = SpatialList["RelativeSize"];
  
  List SelectivityList = FleetListOut["Selectivity"];
  NumericVector SelectivityAtAge = SelectivityList["MeanAtAge"]; 
  
  NumericVector FleetWeightAtAge = FleetListOut["FleetWeightAtAge"];
  NumericVector VBiomassArea = FleetListOut["VBiomassArea"];
  NumericVector DensityArea = FleetListOut["DensityArea"];
  
  
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
                                 TSindex);
    
    // Relative VB Density by Area & Fleet
    DensityArea = CalcDensity_(DensityArea, 
                               VBiomassArea,
                               RelativeSize, 
                               TSindex);
  }
      
  //   
 
  //   
  //   // Distribute Effort over Areas (currently proportional to VB Density)
  //   EffortArea = DistEffort_(EffortArea, DensityArea, Effort, TSindex);
  //   
  //   // Calc F and Catch Removal and Retain by Area
  //   List CatchList = CalcCatch_(NumberAtAgeArea,
  //                               DensityArea,
  //                               NaturalMortalityAtAge,
  //                               FleetWeightAtAge,
  //                               EffortArea,
  //                               Catchability,
  //                               SelectivityAtAge,
  //                               RetentionAtAge,
  //                               DiscardMortalityAtAge,
  //                               FDeadArea,
  //                               FRetainArea,
  //                               RemovalArea,
  //                               RetainArea,
  //                               RemovalNAtAge,
  //                               RetainNAtAge,
  //                               RemovalBAtAge,
  //                               RetainBAtAge,
  //                               TSindex);
  //   
  //   FDeadArea = CatchList["FDeadArea"];
  //   FRetainArea = CatchList["FRetainArea"];
  //   RemovalArea = CatchList["RemovalArea"];
  //   RetainArea = CatchList["RetainArea"];
  //   RemovalNAtAge = CatchList["RemovalNAtAge"];
  //   RetainNAtAge = CatchList["RetainNAtAge"];
  //   RemovalBAtAge = CatchList["RemovalBAtAge"];
  //   RetainBAtAge = CatchList["RetainBAtAge"];
  //   
  //   
  //   
  //   // Calculate overall F 
  //   List Foverall = CalcFfromCatch_(FDeadAtAge,
  //                                   FRetainAtAge,
  //                                   NumberAtAgeArea,
  //                                   RemovalNAtAge,
  //                                   NaturalMortalityAtAge,
  //                                   SelectivityAtAge,
  //                                   RetentionAtAge,
  //                                   DiscardMortalityAtAge,
  //                                   FDeadArea,
  //                                   FRetainArea,
  //                                   TSindex);
  //   
  //   
  //   
  //   // Calc Spawning Production 
  //   
  //   
  //   SProduction = CalcSpawnProduction_(SProduction,
  //                                      NaturalMortalityAtAge,
  //                                      Foverall["FDeadTotalAtAge"], // summed over fleets and areas
  //                                              FecundityAtAge,
  //                                              NumberAtAgeArea, 
  //                                              SpawnTimeFrac,
  //                                              SPFrom,
  //                                              TSindex);
  //   
  //   
  //   
  //   
  //   // Calc Recruitment 
  //   NumericVector Recruits = CalcRecruitment_(SProduction,
  //                                             R0,
  //                                             SP0,
  //                                             RecDevs,
  //                                             SRRModel,
  //                                             SRRPars,
  //                                             Sim,
  //                                             TSindex);
  //   // Add Recruits 
  //   // TODO option to add to beginning of next time-step i.e age 1
  //   
  //   NumberAtAgeArea = AddRecruits_(NumberAtAgeArea,
  //                                  Recruits,
  //                                  UnfishedDist,
  //                                  TSindex);
  //   
  //   
  //   
  //   
  //   // Update Number beginning of next Time Step 
  //   NumberAtAgeArea = CalcNumberNext_(NumberAtAgeArea,
  //                                     NaturalMortalityAtAge,
  //                                     CatchList["FDeadArea"],
  //                                              Semelparous,
  //                                              Ages,
  //                                              TSindex);
  //   
  //   // Move Population at beginning
  //   // NumberAtAgeArea = MoveStock_(NumberAtAgeArea,
  //   //                              Movement,
  //   //                              TSindex);
  //   
  // }
  // 
  // 

  PopulationListOut["BiomassArea"] = BiomassArea;
  
  FleetListOut["VBiomassArea"] = VBiomassArea; 
  FleetListOut["DensityArea"] = DensityArea;

  List L = List::create(Named("PopulationList")=PopulationListOut,
                        Named("FleetList")=FleetListOut
  ); 
                        

  return(L);
}





