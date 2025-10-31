#ifndef CalcStockMovement_H
#define CalcStockMovement_H

arma::cube CalcStockMovement_(arma::cube NumberAtAgeArea,
                              arma::cube Movement, // FromArea, ToArea, nAge 
                              int nAge,
                              int nArea,
                              int TSindex);
                              
#endif                              