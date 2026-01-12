#ifndef MSETOOL_CALC_VBIOMASS_H
#define MSETOOL_CALC_VBIOMASS_H

#include "array_nd.h"
#include "array_views.h"

/**
 *  Calculate Vulnerable Biomass
 *
 * Internal calculation of vulnerable biomass-at-time-step for a single stock.
 * This function aggregates numbers-at-age across ages, applying fleet
 * selectivity, retention, and weight-at-age.
 *
 * Expected dimensions:
 *
 * \itemize{
 *   \item \code{Num}:  sim × age × area
 *   \item \code{Sel}:  sim × age × fleet × area
 *   \item \code{Ret}:  sim × age × fleet × area
 *   \item \code{Wgt}:  sim × age × fleet
 * }
 *
 *
 * Broadcasting rules:
 * \itemize{
 *   \item If \code{Num.dim[0] == 1}, numbers are broadcast across simulations
 *   \item If \code{Sel.dim[3] == 1} or \code{Ret.dim[3] == 1}, values are
 *         broadcast across areas
 *   \item If \code{Wgt.dim[0] == 1}, weights are broadcast across simulations
 * }
 *
 * No memory allocation occurs outside the returned array.
 *
 * @param Num Numbers-at-age (sim × age × area)
 * @param Sel Selectivity-at-age (sim × age × fleet × area)
 * @param Ret Retention-at-age (sim × age × fleet × area)
 * @param Wgt Weight-at-age (sim × age × fleet)
 *
 * @return Array3D sim × fleet × area of vulnerable biomass
 *
 */
inline Array3D CalcVBiomass_(
    const Array3D& Num,
    const Array4D& Sel,
    const Array4D& Ret,
    const Array3D& Wgt
) {
  
  // Dimensions
  const int nSim   = Sel.dim[0];
  const int nAge   = Sel.dim[1];
  const int nFleet = Sel.dim[2];
  const int nArea  = Sel.dim[3];
   
  // Output: sim × fleet × area
  Array3D VB(nSim, nFleet, nArea);
  VB.zero();
   
  for (int sim = 0; sim < nSim; ++sim) {
    for (int fl = 0; fl < nFleet; ++fl) {
      for (int ar = 0; ar < nArea; ++ar) {
        double vb = 0.0;
        for (int age = 0; age < nAge; ++age) {
          vb +=
            Num(sim, age, ar) *
            Sel(sim, age, fl, ar) *
            Ret(sim, age, fl, ar) *
            Wgt(sim, age, fl);
        }
        VB(sim, fl, ar) = vb;
      }
    } 
  } 
  
  return VB;
} 

#endif

