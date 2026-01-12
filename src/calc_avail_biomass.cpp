#include <Rcpp.h>
#include "calc_avail_biomass.h"
#include "calc_vbiomass.h"
#include "array2d.h"
#include "array3d.h"
#include "array4d.h"

//' Calculate Available Biomass per Unit Effort
//'
//' Computes available biomass per unit effort for each simulation,
//' area, and fleet by summing vulnerable biomass across ages and
//' scaling by fleet-specific catchability.
//'
//'
//' Broadcasting rules:
//' \itemize{
//'   \item The simulation dimension (`Sim`) may be length 1 or `nSim`
//'   \item The area dimension of `Sel` and `Ret` may be length 1 or `nArea`
//' }
//'
//' @param Num Array3D. Numbers-at-age with dimensions
//'   `(Sim, Age, Area)`.
//'
//' @param Weight Array3D. Individual weight-at-age with dimensions
//'   `(Sim, Age, Area)`.
//'
//' @param Sel Array4D. Selectivity-at-age with dimensions
//'   `(Sim, Age, Fleet, Area)`.
//'
//' @param Ret Array4D. Retention-at-age with dimensions
//'   `(Sim, Age, Fleet, Area)`.
//'
//' @param q Array2D. Catchability with dimensions
//'   `(Sim, Fleet)`.
//'
//' @return Array3D with dimensions `(Sim, Area, Fleet)` giving available
//'   biomass per unit effort.
//'
//' @details
//' This function does not apply fishing mortality, effort, or quota
//' constraints. It strictly computes biological availability to fleets
//' and is intended for use in spatial utility and effort allocation
//' models.
//' 
 Array3D CalcAvailBiomass_(const Array3D& Num,
                           const Array3D& Weight,
                           const Array4D& Sel,
                           const Array4D& Ret,
                           const Array2D& q) {
   
   Array3D VB = CalcVBiomass_(Num, Sel, Ret, Weight); // sim, fleet, area
   
   const int nSim   = std::max(VB.n1, q.n1);
   const int nFleet = VB.n2;
   const int nArea  = VB.n3;

   Array3D AB(nSim, nFleet, nArea, 0.0);
   
   for (int s = 0; s < nSim; ++s) {
     for (int a = 0; a < nArea; ++a) {
       for (int f = 0; f < nFleet; ++f) {
         AB(s, f, a) = q(s, f) * VB(s, f, a);
       }
     }
   }
   
   return AB;
 }

 
                           