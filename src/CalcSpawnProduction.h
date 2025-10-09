#ifndef CalcSpawnProduction_H
#define CalcSpawnProduction_H

#include <RcppArmadillo.h>
arma::vec CalcSpawnProduction_(arma::mat NumberAtAgeArea, // nAge, nArea
                              arma::vec FecundityAtAge, // nAge
                              arma::vec MaturityAtAge, // nAge,
                              arma::vec WeightAtAge, // nAge
                              arma::vec NaturalMortalityAtAge, // nAge
                              arma::mat FDeadAtAgeArea, // nAge, nArea
                              arma::vec SpawnMortality, // nAge
                              double SpawnTimeFrac);

#endif