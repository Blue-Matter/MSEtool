#ifndef CalcEffortDistribution_H
#define CalcEffortDistribution_H

arma::mat CalcEffortDistribution_(arma::mat VBiomassFleetArea, // nFleet, nArea
                                  arma::vec Effort,
                                  int nArea);

#endif