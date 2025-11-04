#ifndef CalcVBiomass_H
#define CalcVBiomass_H

arma::mat CalcVBiomass_(arma::mat NumberAtAgeArea, // nAge, nArea
                       arma::mat FleetWeightAtAgeFleet, // nAge, nFleet
                       arma::mat SelectivityAtAgeFleet, // nAge, nFleet
                       arma::mat ClosureFleetArea,
                       int debug);

#endif