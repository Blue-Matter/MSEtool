#ifndef CalcNumberNext_H
#define CalcNumberNext_H

arma::mat CalcNumberNext_(arma::mat NumberAtAgeAreaThisTS, // nAge, nArea
                          arma::mat NumberAtAgeAreaNextTS, // nAge, nArea
                          arma::vec Semelparous,  // nAge
                          arma::cube FDeadAtAgeAreaThisTS,  // nAge, nFleet, nArea
                          arma::vec NaturalMortalityAtAgeThisTS,
                          bool plusgroup,
                          int nAge,
                          int nArea);

#endif