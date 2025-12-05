#include <RcppArmadillo.h>
#include "check.h"


List CalcFMortality_(arma::mat EffortFleetArea, // nFleet, nArea
                    arma::vec Catchability, // nFleet
                    arma::mat qArea, // Fleet, nArea
                    arma::vec RelativeSize, // nArea
                    arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                    arma::mat RetentionAtAgeFleet, // nAge, nFleet
                    arma::mat DiscardMortalityAtAgeFleet, // nAge, nFleet
                    int nArea,
                    int debug=0) {
  
  // int nFleet = EffortFleetArea.n_rows;
  if (nArea==1)
    EffortFleetArea = EffortFleetArea.t();

  if (nArea==1)
    qArea = qArea.t();
  
  int nFleet = Catchability.size();
  
  // if (debug) {
  //   Rcout << "*********************"  << std::endl;
  //   Rcout << "CalcFMortality_ Function " << nArea << std::endl;
  //   Rcout << "nArea = " << nArea << std::endl;
  //   Rcout << "qArea = " << qArea << std::endl;
  //   Rcout << "nFleet = " << nFleet << std::endl;
  //   Rcout << "Catchability = " << Catchability << std::endl;
  // }
  
  
  int nAge = SelectivityAtAgeFleet.n_rows;
  
  CheckLength(Catchability.size(), nFleet, "Catchability", "nFleet (nrow(EffortFleetArea))");
  CheckLength(RelativeSize.size(), nArea, "RelativeSize", "nArea (ncol(EffortFleetArea))");
  
  CheckDimensions(nFleet, SelectivityAtAgeFleet.n_cols, "SelectivityAtAgeFleet", "nFleet", 1);
  CheckDimensions(nAge, RetentionAtAgeFleet.n_rows, "RetentionAtAgeFleet", "nAge");
  CheckDimensions(nFleet, RetentionAtAgeFleet.n_cols, "RetentionAtAgeFleet", "nFleet", 1);
  CheckDimensions(nAge, DiscardMortalityAtAgeFleet.n_rows, "DiscardMortalityAtAgeFleet", "nAge");
  CheckDimensions(nFleet, DiscardMortalityAtAgeFleet.n_cols, "DiscardMortalityAtAgeFleet", "nFleet", 1);
  
  arma::cube FDeadFleetArea(nAge, nFleet, nArea);
  arma::cube FRetainFleetArea(nAge, nFleet, nArea);
  
  for (int fl=0; fl<nFleet; fl++) {
    for (int area=0; area<nArea; area++) {
      
      double catchabilityArea = arma::as_scalar(qArea.row(fl).col(area));
      
      if (catchabilityArea < 1E-5) {
        catchabilityArea = Catchability(fl)/RelativeSize(area);
        qArea.row(fl).col(area) = catchabilityArea;
      }
      
      double effortarea = arma::as_scalar(EffortFleetArea.row(fl).col(area));

      arma::vec FInteract = effortarea * catchabilityArea * SelectivityAtAgeFleet.col(fl);
      arma::vec FRetain = FInteract % RetentionAtAgeFleet.col(fl);
      arma::vec Discard = FInteract - FRetain;
      arma::vec DeadDiscard = Discard % DiscardMortalityAtAgeFleet.col(fl);
      FRetainFleetArea.subcube(0, fl, area, nAge-1, fl, area) = FRetain;
      FDeadFleetArea.subcube(0, fl, area, nAge-1,fl, area) = FRetain + DeadDiscard;
    }
  }
  // if (debug) {
  //   Rcout << "*********************"  << std::endl;
  // }
  
  List L = List::create(Named("FDeadFleetArea") = FDeadFleetArea,
                        Named("FRetainFleetArea") = FRetainFleetArea,
                        Named("qArea") = qArea);
  return(L);
}

