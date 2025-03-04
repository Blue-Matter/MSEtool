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
NumericVector CalcBiomass_(NumericVector BiomassAtAgeArea,
                           NumericVector NumberAtAgeArea,
                           NumericVector WeightAtAge,
                           int TSindex) {
  
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SAT = SATR[IntegerVector{0,1,2}];
  
  int nStock = SATR[0];
  int nAge = SATR[1];
  int nArea = SATR[3];
  
  for (int st=0; st<nStock; st++) {
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        IntegerVector SRindex = IntegerVector::create(st, area);
        
        Rcout << "SATRindex " << SATRindex << std::endl;
        Rcout << "SRindex " << SRindex << std::endl;

        BiomassAtAgeArea[GetIndex_(SATR, SATRindex)] =
          NumberAtAgeArea[GetIndex_(SATR, SATRindex)] *
          WeightAtAge[GetIndex_(SAT, SATindex)];
      }
    }
  }
  
  return(BiomassAtAgeArea);
}

NumericVector CalcVBiomass_(NumericVector VBiomassAtAgeArea,
                            NumericVector NumberAtAgeArea,
                            NumericVector FleetWeightAtAge,
                            NumericVector SelectivityAtAge,
                            int TSindex) {
  
  IntegerVector SATFR = CalcDims_(VBiomassAtAgeArea); // stock, age, time step, fleet, area
  IntegerVector SATF = SATFR[IntegerVector{0,1,2,3}];
  IntegerVector SATR = SATFR[IntegerVector{0,1,2,4}];
  
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  for (int st=0; st<nStock; st++) {
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        for (int fl=0; fl<nFleet; fl++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          VBiomassAtAgeArea[GetIndex_(SATFR, SATFRindex)] = NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * 
            FleetWeightAtAge[GetIndex_(SATF, SATFindex)] * SelectivityAtAge[GetIndex_(SATF, SATFindex)];
        }
      }
    }
  }
  
  return(VBiomassAtAgeArea);
}

NumericVector CalcDensity_(NumericVector DensityArea, 
                           NumericVector VBiomassAtAgeArea,
                           NumericVector RelativeSize, 
                           int TSindex) {
  
  IntegerVector SATFR = CalcDims_(VBiomassAtAgeArea); // stock, age, time step, fleet, area
  IntegerVector STFR = SATFR[IntegerVector{0,2,3,4}];
  IntegerVector SFR = SATFR[IntegerVector{0,3,4}];
  IntegerVector SR = SFR[IntegerVector{0,2}];
  IntegerVector SF = SFR[IntegerVector{0,1}];
  int nStock = SATFR[0];
  int nAge = SATFR[1];
  int nFleet = SATFR[3];
  int nArea = SATFR[4];
  
  NumericVector VBiomassArea = (nStock * nFleet * nArea);
  NumericVector DensityTotal = (nStock * nFleet);
  
  for (int st=0; st<nStock; st++) {
    for (int area=0; area<nArea; area++) {
      IntegerVector SRindex = IntegerVector::create(st, area);
      for (int fl=0; fl<nFleet; fl++) {
        IntegerVector SFRindex = IntegerVector::create(st, fl, area);
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        for(int age=0; age<nAge; age++) {
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          VBiomassArea[GetIndex_(SFR, SFRindex)] += VBiomassAtAgeArea[GetIndex_(SATFR, SATFRindex)];
        }
        DensityArea[GetIndex_(STFR, STFRindex)] = VBiomassArea[GetIndex_(SFR, SFRindex)]/ RelativeSize[GetIndex_(SR, SRindex)];
      }
    }
  }
  
  for (int st=0; st<nStock; st++) {
    for (int fl=0; fl<nFleet; fl++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        IntegerVector SFindex = IntegerVector::create(st, fl);
        DensityTotal[GetIndex_(SF, SFindex)] +=  DensityArea[GetIndex_(STFR, STFRindex)];
      }
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        IntegerVector SFindex = IntegerVector::create(st, fl);
        DensityArea[GetIndex_(STFR, STFRindex)] =  DensityArea[GetIndex_(STFR, STFRindex)]/DensityTotal[GetIndex_(SF, SFindex)];
      }
    }
  }
        
  return(DensityArea);
}


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


List CalcCatch_(NumericVector NumberAtAgeArea, 
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
      for (int area=0; area<nArea; area++) {
        IntegerVector STFRindex = IntegerVector::create(st, TSindex, fl, area);
        double effort = EffortArea[GetIndex_(STFR, STFRindex)];
        for(int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
          
          double FInteract = effort * SelectivityAtAge[GetIndex_(SATF, SATFindex)];
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
  IntegerVector AF = SATFR[IntegerVector{1,3}]; 
  
    
  // 1 Area
  if (nArea<2) {
    for (int st=0; st<nStock; st++) {
      for (int fl=0; fl<nFleet; fl++) {
        for (int age=0; age<nAge; age++) {
          IntegerVector SATFindex = IntegerVector::create(st, age, TSindex, fl);
          for (int area=0; area<nArea; area++) {
            IntegerVector SATFRindex = IntegerVector::create(st, age, TSindex, fl, area);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] += FDeadArea[GetIndex_(SATFR, SATFRindex)];
            FRetainAtAge[GetIndex_(SATF, SATFindex)] += FRetainArea[GetIndex_(SATFR, SATFRindex)];  
          }
        }
      }
    }
    List L = List::create(Named("FDeadAtAge")=FDeadAtAge,
                          Named("FRetainAtAge") = FRetainAtAge
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
        IntegerVector SARindex = IntegerVector::create(st, age, area);
        NumberAtAge[GetIndex_(SA, SAindex)] += NumberAtAgeArea[GetIndex_(SAR, SARindex)]; 
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
          for (int area=0; area<nArea; area++) {
            IntegerVector AFindex = IntegerVector::create(age, fl);
            FDeadAtAge[GetIndex_(SATF, SATFindex)] =  FDeadTotal[GetIndex_(AF, AFindex)];
            FRetainAtAge[GetIndex_(SATF, SATFindex)] = FRetain[GetIndex_(AF, AFindex)];
          }
        }
      }
      
    }
  }
  

  List L = List::create(Named("FDeadAtAge")=FDeadAtAge,
                        Named("FRetainAtAge") = FRetainAtAge
  ); 
  
  return(L);

}

// NumericVector CalcSurvival_(NumericVector NaturalMortalityAtAge,
//                             NumericVector FDeadAtAge,
//                             PlusGroup
//   
// ) {
//   
// }

// NumericVector CalcSpawnProduction_(NumericVector SProduction, 
//                                    NumericVector NaturalMortalityAtAge,
//                                    NumericVector FDeadAtAge,
//                                    NumericVector FecundityAtAge,
//                                    NumericVector NumberAtAge,
//                                    NumericVector Semelparous,
//                                    NumericVector SpawnTimeFrac,
//                                    int TSindex) {
//   
//   
//   
//   SProduction[GetIndex_(ST, STFindex)] +=  
//  
//   
//   return(SProduction);
// }


// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// [[Rcpp::export]]
List CalcPopDynamics_(NumericVector NumberAtAgeArea,
                      NumericVector BiomassAtAgeArea,
                      
                      NumericVector WeightAtAge,
                      NumericVector NaturalMortalityAtAge, 
                      NumericVector FecundityAtAge,
                      
                      NumericVector R0,
                      NumericVector RecDevs,
                      Function  SRRModel,
                      List SRRPars,
                      NumericVector SProduction,
                      
                      NumericVector RelativeSize,
                      
                      NumericVector Effort,
                      NumericVector EffortArea,
                      NumericVector Catchability,
                      
                      NumericVector SelectivityAtAge,
                      NumericVector RetentionAtAge,
                      NumericVector DiscardMortalityAtAge,
                      
                      NumericVector VBiomassAtAgeArea,
                      NumericVector FleetWeightAtAge,
                      NumericVector DensityArea,
                      
                      NumericVector FDeadArea,
                      NumericVector FRetainArea,
                      NumericVector RemovalArea,
                      NumericVector RetainArea,
                      
                      NumericVector FDeadAtAge,
                      NumericVector FRetainAtAge,
                      
                      NumericVector RemovalNAtAge,
                      NumericVector RetainNAtAge,
                      NumericVector RemovalBAtAge,
                      NumericVector RetainBAtAge,
                   
                      CharacterVector TimeStep) {
  
  int TSindex = MatchTimeStep_(NumberAtAgeArea, TimeStep);
  
  // Biomass by Area beginning of this time step
  BiomassAtAgeArea = CalcBiomass_(BiomassAtAgeArea, NumberAtAgeArea, WeightAtAge, TSindex);
  
  // VB by Area
  VBiomassAtAgeArea = CalcVBiomass_(VBiomassAtAgeArea, NumberAtAgeArea, FleetWeightAtAge, SelectivityAtAge, TSindex);
  
  // Relative VB Density by Area & Fleet
  DensityArea = CalcDensity_(DensityArea, VBiomassAtAgeArea, RelativeSize, TSindex);
 
  // Distribute Effort over Areas (currently proportional to VB Density)
  EffortArea = DistEffort_(EffortArea, DensityArea, Effort, TSindex);
  
  // Calc F and Catch Removal and Retain by Area
  List CatchList = CalcCatch_(NumberAtAgeArea,
                              NaturalMortalityAtAge,
                              FleetWeightAtAge,
                              EffortArea,
                              Catchability,
                              SelectivityAtAge,
                              RetentionAtAge,
                              DiscardMortalityAtAge,
                              FDeadArea,
                              FRetainArea,
                              RemovalArea,
                              RetainArea,
                              RemovalNAtAge,
                              RetainNAtAge,
                              RemovalBAtAge,
                              RetainBAtAge,
                              TSindex);
  
  // Calculate overall F 
  List Foverall = CalcFfromCatch_(FDeadAtAge,
                                  FRetainAtAge,
                                  NumberAtAgeArea,
                                  RemovalNAtAge,
                                  NaturalMortalityAtAge,
                                  SelectivityAtAge,
                                  RetentionAtAge,
                                  DiscardMortalityAtAge,
                                  FDeadArea,
                                  FRetainArea,
                                  TSindex);

  // Calc Spawning Production 
  
  // Recruitment (TODO differnet age class)
  
         
  // Update Number beginning of next Time Step 
  
  // Move Population at beginning
  

  
  
  List L = List::create(Named("NumberAtAgeArea")=NumberAtAgeArea,
                        Named("BiomassAtAgeArea") = BiomassAtAgeArea,
                        Named("WeightAtAge") = WeightAtAge,
                        Named("Effort") = Effort,
                        Named("SelectivityAtAge") = SelectivityAtAge,
                        Named("VBiomassAtAge") = VBiomassAtAgeArea,
                        Named("DensityArea") = DensityArea,
                        Named("EffortArea") = EffortArea,
                        Named("FDeadArea") = CatchList["FDeadArea"],
                        Named("FRetainArea") = CatchList["FRetainArea"],
                        Named("RemovalArea") = CatchList["RemovalArea"],
                        Named("RetainArea") = CatchList["RetainArea"],
                        Named("FDeadAtAge") = Foverall["FDeadAtAge"]                                                      
                          
  ); 
                        

  return(L);
}





