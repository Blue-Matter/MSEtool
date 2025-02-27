#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

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
  int TSindex = TSMatch[0];
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

// Given N and Effort at beginning of Time Step, calculates catch etc this time
// step and N at beginning of next time step
// returns updated arrays
// [[Rcpp::export]]
List CalcPopDynamics_(NumericVector NumberAtAgeArea,
                      NumericVector BiomassAtAgeArea,
                      
                      NumericVector WeightAtAge,
                      NumericVector Effort,
                      CharacterVector TimeStep,
                      int nStock,
                      int nAge,
                      int nFleet,
                      int nArea
                      ) {
  
  int TSindex = MatchTimeStep_(NumberAtAgeArea, TimeStep);
  IntegerVector SATR = CalcDims_(NumberAtAgeArea); // stock, age, time step, area
  IntegerVector SAT = CalcDims_(WeightAtAge);; // stock, age, time step

  // Calc MICE (if applicable)
  // Hist <- CalcMICE(Hist, TimeStep=TimeStep)
  
  
  for (int st=0; st<nStock; st++) {
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector SATRindex = IntegerVector::create(st, age, TSindex, area);
        IntegerVector SATindex = IntegerVector::create(st, age, TSindex);
        
        // Biomass this time step 
        BiomassAtAgeArea[GetIndex_(SATR, SATRindex)] =
          NumberAtAgeArea[GetIndex_(SATR, SATRindex)] * 
          WeightAtAge[GetIndex_(SAT, SATindex)];
        
        // Vulnerable Biomass this time step
        
        // VB Density this time step
        
        // Effort this time step
        
        // Effort by Area
        
        // Catches this Time Step
        
        // Spawning Production and Recruitment this Time Step
        
        // Update Number beginning of next Time Step and Move Population
        
        
      }
    }
    
  }
  
  
  // Calculate Density (Vulnerable) by Area & Fleet 
  
  // Distribute Effort across Areas 
  
  // Hist <- DistributeEffort(Hist, TimeStep)

  
  List L (1);
  L(0) = BiomassAtAgeArea;
  return(L);
}






// [[Rcpp::export]]
NumericVector CalcBiomass_(NumericVector NumberAtAgeArea, 
                           NumericVector WeightAtAge,
                           CharacterVector TimeStep) { 
  
  List dimnames = NumberAtAgeArea.attr("dimnames");
  IntegerVector M = match(TimeStep, as<CharacterVector>(dimnames["Time Step"]));

  int TSInd = M(0)- 1;
  
  int len = NumberAtAgeArea.length();
  int Ndim = dimnames.length();

  IntegerVector dim(Ndim);

  for (int i=0; i<Ndim; i++) {
    dim(i) = as<CharacterVector>(dimnames[i]).size();
  }
  dim.names() = dimnames.names();
  
  NumericVector BiomassAgeArea(len);  
  int nStock = dim["Stock"];
  int nAge = dim["Age"];
  int nTimeStep = dim["Time Step"];
  int nArea = dim["Area"];

  IntegerVector dim2 = IntegerVector::create(nStock, nAge, nTimeStep);
  
  for (int st=0; st<nStock; st++) {
    for(int age=0; age<nAge; age++) {
      for (int area=0; area<nArea; area++) {
        IntegerVector Nselect = IntegerVector::create(st, age, TSInd, area);
    
        int Nindex = GetIndex_(dim, Nselect);
        IntegerVector Wselect = IntegerVector::create(st, age, TSInd);
        int Windex = GetIndex_(dim2, Wselect);
        BiomassAgeArea[Nindex] = NumberAtAgeArea[Nindex] * WeightAtAge[Windex];
      }
    }

  }

  BiomassAgeArea.attr("dim") = dim;
  BiomassAgeArea.attr("dimnames") = dimnames;
  return BiomassAgeArea;
}





/*** R


# L = test(NumberAtAgeArea)
# L[[2]]
# L$name1
# 
# 
# dimSizes <- dim(NumberAtAgeArea)
# dimIndices <- c(2,1,15,4,2)
# 
# # flat_index <- function(dimSizes, dimIndices){
# #   dimIndices[1] + sum((dimIndices[-1] - 1) * cumprod(dimSizes[-length(dimSizes)]))
# # }
# 
# # flat_index(dimSizes, dimIndices)
# GetIndex_(dimSizes, dimIndices)


*/
