#ifndef CalcFMortality_H
#define CalcFMortality_H

Rcpp::List CalcFMortality_(arma::mat EffortFleetArea, // nFleet, nArea
                     arma::vec Catchability, // nFleet
                     arma::mat qArea, // nFleet, nArea
                     arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                     arma::mat RetentionAtAgeFleet, // nAge, nFleet
                     arma::mat DiscardMortalityAtAgeFleet, // nAge, nFleet
                     int nArea);

#endif