#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
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
S4 CalcAggregateF_(S4 HistSimIn,
                   Rcpp::NumericVector TimeSteps) {
  
  S4 HistSim = clone(HistSimIn);
  S4 OM = HistSim.slot("OM");
  List StockList = OM.slot("Stock");
  List FleetList = OM.slot("Fleet");
  
  NumericVector TimeStepsAll = OM.slot("TimeSteps");
  int nTS = TimeSteps.size();
  IntegerVector MatchTS = match(TimeSteps, TimeStepsAll);
  
  List NumberAtAgeAreaList = HistSim.slot("Number"); // nStock
  int nStock = NumberAtAgeAreaList.size();
  
  List FDeadAtAgeAreaList = HistSim.slot("FDeadAtAgeArea");
  List FRetainAtAgeAreaList = HistSim.slot("FRetainAtAgeArea");
  
  List FDeadAtAgeList =  HistSim.slot("FDeadAtAge");
  List FRetainAtAgeList = HistSim.slot("FRetainAtAge");
  
  List RemovalsList =  HistSim.slot("Removals"); // nStock
  List LandingsList = HistSim.slot("Landings");
  
 
  for (int timestep=0; timestep<nTS; timestep++) {
    NumericVector TSmatch = abs(TimeStepsAll - TimeSteps[timestep]);
    int TSindex = which_min(TSmatch);
    
    for (int st=0; st<nStock; st++) {
      
      S4 Stock = StockList[st];
      
      S4 NaturalMortality = Stock.slot("NaturalMortality");
      arma::mat NaturalMortalityAtAge = NaturalMortality.slot("MeanAtAge");
      
    
      S4 Fleet = FleetList[st];
      
      S4 FleetEffort = Fleet.slot("Effort");
      arma::mat Catchability = FleetEffort.slot("Catchability"); // nTS, nFleet
      
      S4 Selectivity = Fleet.slot("Selectivity");
      arma::cube SelectivityAtAge = Selectivity.slot("MeanAtAge"); // nAge, nTS, nFleet
      
      
      S4 Retention = Fleet.slot("Retention");
      arma::cube RetentionAtAge = Retention.slot("MeanAtAge"); // nAge, nTS, nFleet
      
      S4 DiscardMortality = Fleet.slot("DiscardMortality");
      arma::cube DiscardMortalityAtAge = DiscardMortality.slot("MeanAtAge"); // nAge, nTS, nFleet
      
      S4 Distribution = Fleet.slot("Distribution"); 
      arma::cube ClosureArea = Distribution.slot("Closure"); // nTS, nFleet, nArea
      
      
      List RemovalsStock = RemovalsList[st]; // nTS
      List LandingsStock = LandingsList[st]; // nTS
      
      arma::cube RemovalsTS = RemovalsStock[TSindex]; // nAge, nFleet, nArea
      arma::cube LandingsTS = LandingsStock[TSindex]; // nAge, nFleet, nArea
      
      arma::cube FleetWeightAtAge = Fleet.slot("WeightFleet") ; // nAge, nTS, nFleet
      
      int nAge = RemovalsTS.n_rows;
      int nFleet = RemovalsTS.n_cols;
      int nArea = RemovalsTS.n_slices;
      
      arma::mat RemovalNumberAtAge(nAge, nFleet);
      arma::mat RetainNumberAtAge(nAge, nFleet);
      
      // convert to Catch in Numbers
      for (int area=0; area<nArea; area++) {
        for (int fl=0; fl<nFleet; fl++) {
          arma::vec removals = arma::vectorise(RemovalsTS.subcube(arma::span(0, nAge-1), arma::span(fl), arma::span(area)));
          arma::vec landings = arma::vectorise(LandingsTS.subcube(arma::span(0, nAge-1), arma::span(fl), arma::span(area)));
          arma::vec weight = arma::vectorise(FleetWeightAtAge.subcube(arma::span(0, nAge-1), arma::span(TSindex), arma::span(fl)));
          RemovalNumberAtAge.col(fl) = removals/weight;
          RetainNumberAtAge.col(fl) = landings/weight;
        }  
      }
      
      List FDeadAtAgeAreaStock = FDeadAtAgeAreaList[st];
      List FRetainAtAgeAreaStock = FRetainAtAgeAreaList[st];
      
      arma::cube FDeadAtAgeAreaThisTS = FDeadAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      arma::cube FRetainAtAgeAreaThisTS = FRetainAtAgeAreaStock[TSindex]; // nAge, nFleet, nArea
      
      arma::cube FDeadAtAge = FDeadAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube FRetainAtAge = FRetainAtAgeList[st]; // nAge, nTS, nFleet
      arma::cube NumberAtAgeArea = NumberAtAgeAreaList[st]; // nAge, nTS, nArea
      
      // Calculate aggregate F over all areas
      List AggF = CalcAggF_(FDeadAtAgeAreaThisTS,
                            FRetainAtAgeAreaThisTS,
                            NumberAtAgeArea.col(TSindex),
                            RemovalNumberAtAge,
                            RetainNumberAtAge,
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
  
  HistSim.slot("FDeadAtAge") = FDeadAtAgeList;
  HistSim.slot("FRetainAtAge") = FRetainAtAgeList;
  
  return(HistSim);
}
