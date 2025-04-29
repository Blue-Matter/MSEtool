#include <cpp11.hpp>
#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List CalcFfromCatch_(arma::vec NumberAtAge, // nAge
                     arma::mat RemovalNAtAge, // nAge, nFleet (number) 
                     arma::mat SelectivityAtAge,  // nAge, nFleet
                     arma::mat RetentionAtAge,  // nAge, nFleet
                     arma::mat DiscardMortalityAtAge,  // nAge, nFleet
                     arma::vec NaturalMortalityAtAge, // nAge
                     int MaxIt=200,   
                     double tolF=1E-4) {
  
  int nAge = RemovalNAtAge.n_rows;
  int nFleet = RemovalNAtAge.n_cols;
  
  arma::vec TotalRemovalsFleet = sum(RemovalNAtAge,0); // nFleet
  
  double TotalNumber = arma::accu(NumberAtAge);
  
  // initial guess at apicalF
  arma::vec apicalF = TotalRemovalsFleet/TotalNumber;
  arma::mat FDeadAtAge(nAge, nFleet);
  arma::mat FRetainAtAge(nAge, nFleet);
  
  for (int i=0; i<MaxIt; i++) {
    // Calculate F-at-age fleet
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FInteract = apicalF(fl) * SelectivityAtAge.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAge.col(fl);
      arma::vec FDiscard = FInteract - FRetain;
      arma::vec FDeadDiscard = FDiscard % DiscardMortalityAtAge.col(fl);
      arma::vec FDead = FDeadDiscard + FRetain;
      FRetainAtAge.col(fl) = FDead;
      FDeadAtAge.col(fl) = FRetain;
    }
    
    arma::vec ZatAge = arma::sum(FDeadAtAge,1) + NaturalMortalityAtAge;
    arma::vec PopDeadAtAge = (1-exp(-ZatAge)) % NumberAtAge;
    
    // derivative of catch wrt ft
    arma::vec dct(nFleet);
    arma::vec predRemovals(nFleet);
    for (int fl=0; fl<nFleet; fl++) {
      arma::vec FDead = FDeadAtAge.col(fl);
      predRemovals(fl) = arma::accu(FDead/ZatAge % (1-exp(-ZatAge)) % NumberAtAge);
      arma::vec temp = PopDeadAtAge / ZatAge - ((FDead % PopDeadAtAge / pow(ZatAge,2))) + FDead/ZatAge % exp(-ZatAge)%NumberAtAge;
      dct(fl) = accu(temp);
    }
    
    apicalF = apicalF - (predRemovals - TotalRemovalsFleet)/(0.8*dct);
    NumericVector diff = as<NumericVector>(Rcpp::wrap((predRemovals - TotalRemovalsFleet)));
    NumericVector totalremovals = as<NumericVector>(Rcpp::wrap((TotalRemovalsFleet)));
    
    LogicalVector converge = (Rcpp::abs(diff)/totalremovals) < tolF;
    if (all(converge).is_true())
      break;
  }
  
  
  List L = List::create(Named("FDeadAtAge") = FDeadAtAge,
                        Named("FRetainAtAge") = FRetainAtAge);
  return(L);
  
}

List CalcAggF_(arma::cube FDeadAtAgeAreaThisTS, // nAge, nFleet, nArea
               arma::cube FRetainAtAgeAreaThisTS, // nAge, nFleet, nArea
               arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
               arma::mat RemovalNumberAtAgeThisTS, // nAge, nFleet
               arma::mat RetainNumberAtAgeThisTS, // nAge, nFleet
               arma::mat SelectivityAtAgeThisTS, // nAge, nFleet
               arma::mat RetentionAtAgeThisTS, // nAge, nFleet
               arma::mat DiscardMortalityAtAgeThisTS, // nAge, nFleet
               arma::mat NaturalMortalityAtAgeThisTS) {
  
  int nArea = NumberAtAgeAreaThisTS.n_cols;
  
  if (nArea<2) {
    // no spatial structure
    List L = List::create(Named("FDeadAtAgeThisTS") = FDeadAtAgeAreaThisTS.slice(0),
                          Named("FRetainAtAgeThisTS") = FRetainAtAgeAreaThisTS.slice(0));
    return(L);
  }
  
  // spatial structure - need to calculate overall 
  int nAge = RetainNumberAtAgeThisTS.n_rows;
  int nFleet = RetainNumberAtAgeThisTS.n_cols;
  
  arma::vec NumberAtAge = arma::sum(NumberAtAgeAreaThisTS,1); // summed over areas
  
  arma::vec TotalRemovalsFleet = sum(RemovalNumberAtAgeThisTS,0); // nFleet
  
  LogicalVector ZeroCatch(nFleet);
  ZeroCatch = TotalRemovalsFleet < 1E-4;
  
  if ((all(ZeroCatch).is_true())) {
    // no catches for any fleets
    // return F=0
    arma::mat FZeros(nAge, nFleet);
    
    List L = List::create(Named("FDeadAtAgeThisTS") = FZeros,
                          Named("FRetainAtAgeThisTS") = FZeros);
    return(L);
  }
  
  // check if F same in all areas
  LogicalVector IdenticalF(nFleet);
  for (int fl=0; fl<nFleet; fl++) {
    arma::mat fdeadfleet = FDeadAtAgeAreaThisTS(arma::span(0, nAge-1), arma::span(fl,fl), arma::span(0,nArea-1));
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
    List L = List::create(Named("FDeadAtAgeThisTS") = FDeadAtAgeAreaThisTS.slice(0),
                          Named("FRetainAtAgeThisTS") = FRetainAtAgeAreaThisTS.slice(0));
    return(L);
  }
  
  
  // Solve for overall F given overall Catch (Number) and Numbers
  List Foverall = CalcFfromCatch_(NumberAtAge,
                                  RemovalNumberAtAgeThisTS,
                                  SelectivityAtAgeThisTS,
                                  RetentionAtAgeThisTS,
                                  DiscardMortalityAtAgeThisTS,
                                  NaturalMortalityAtAgeThisTS
  );
  
  List L = List::create(Named("FDeadAtAgeThisTS") = Foverall["FDeadAtAge"],
                        Named("FRetainAtAgeThisTS") = Foverall["FRetainAtAge"]);
  return(L);
  
}

// [[Rcpp::export]]
List CalcAggregateF_(Rcpp::List OMListSim,
                    Rcpp::NumericVector TimeSteps) {
  
  List OMListSimOut = clone(OMListSim);
  List TimeStepsList = OMListSim["TimeSteps"];
  NumericVector TimeStepsAll = TimeStepsList[0];
  int nTS = TimeSteps.size();
  
  List NumberAtAgeAreaList = OMListSimOut["NumberAtAgeArea"];
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = OMListSimOut["FDeadAtAgeArea"];
  List FRetainAtAgeAreaList = OMListSimOut["FRetainAtAgeArea"];
  
  List FDeadAtAgeList = OMListSimOut["FDeadAtAge"];
  List FRetainAtAgeList = OMListSimOut["FRetainAtAge"];
  
  List RemovalNumberAtAgeList = OMListSimOut["RemovalNumberAtAge"];
  List RetainNumberAtAgeList = OMListSimOut["RetainNumberAtAge"]; 
  
  List Selectivity = OMListSimOut["Selectivity"];
  List SelectivityAtAgeList = Selectivity["MeanAtAge"];

  List Retention = OMListSimOut["Retention"];
  List RetentionAtAgeList = Retention["MeanAtAge"];
  
  List DiscardMortality = OMListSimOut["DiscardMortality"];
  List DiscardMortalityAtAgeList = DiscardMortality["MeanAtAge"];
  
  List NaturalMortality = OMListSimOut["NaturalMortality"];
  List NaturalMortalityAtAgeList = NaturalMortality["MeanAtAge"];

  
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);
    
    for (int st=0; st<nStock; st++) {
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];

      arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      
      arma::cube FDeadAtAge = FDeadAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube FRetainAtAge = FRetainAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      
      arma::cube RemovalNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetainNumberAtAge = RemovalNumberAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::cube SelectivityAtAge = SelectivityAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube RetentionAtAge = RetentionAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube DiscardMortalityAtAge = DiscardMortalityAtAgeList[st]; // nAge, nTS, nFleet
      
      arma::mat NaturalMortalityAtAge = NaturalMortalityAtAgeList[st]; // nAge, nTS
      
      // Calculate aggregate F over all areas
      List AggF = CalcAggF_(FDeadAtAgeAreaThisTS,
                            FRetainAtAgeAreaThisTS,
                            NumberAtAgeArea.col(TSindex),
                            RemovalNumberAtAge.col(TSindex),
                            RetainNumberAtAge.col(TSindex),
                            SelectivityAtAge.col(TSindex),
                            RetentionAtAge.col(TSindex),
                            DiscardMortalityAtAge.col(TSindex),
                            NaturalMortalityAtAge.col(TSindex)
      );
      
      arma::mat FDeadAtAgeThisTS = AggF["FDeadAtAgeThisTS"];
      arma::mat FRetainAtAgeThisTS = AggF["FRetainAtAgeThisTS"];
      
      FDeadAtAge.col(TSindex) = FDeadAtAgeThisTS;
      FRetainAtAge.col(TSindex) = FRetainAtAgeThisTS;
      
      FDeadAtAgeList[st] = FDeadAtAge;
      FRetainAtAgeList[st] = FRetainAtAge;
    } // end of stock loop
   
  } // end of time step loop
  
  OMListSim["FDeadAtAge"] = FDeadAtAgeList;
  OMListSim["FRetainAtAge"] = FRetainAtAgeList;
  
  return(OMListSim);
}
  


  