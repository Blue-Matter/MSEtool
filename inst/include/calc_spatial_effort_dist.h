#ifndef CALC_SPATIAL_DISTRIBUTION_H
#define CALC_SPATIAL_DISTRIBUTION_H

#include <Rcpp.h>
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>

#include "array_nd.h"
#include "array_views.h"
#include "array_types.h"


/**
 * Calculate spatial effort distribution across areas (single simulation)
 */
inline void CalcSpatialEffortDistribution(
    const int y,
    Array3D& Distribution,                    // year, fleet, area
    const std::vector<Array3D>& Number,       // [stock] age, year, area
    const std::vector<ConstArrayView3D>& WeightFleet, // [stock] age, year, fleet
    const std::vector<ConstArrayView4D>& SelAge,      // [stock] age, year, fleet, area
    const std::vector<ConstArrayView4D>& RetAge,      // [stock] age, year, fleet, area
    const ConstArrayView3D& q,                // stock, year, fleet
    const ConstArrayView4D& Closure,          // stock, year, fleet, area
    const ConstArrayView2D& Targeting,        // year, fleet
    const Array2D& Effort,                    // year, fleet
    const ConstArrayView1D& RelSize,          // area
    const int nStock,
    const int nFleet,
    const int nArea
  ) {

  // Single Area
  if (nArea == 1) {
    for (int fl = 0; fl < nFleet; ++fl)
      Distribution(y, fl, 0) = 1.0;
    return;
  }

  // Utility: fleet × area
  Array2D Util({nFleet, nArea}, 0.0);



  // Loop over stocks
  for (int st = 0; st < nStock; ++st) {
    Array2D B_hat({nFleet, nArea}, 0.0);

    const Array3D& Num_st = Number[st];
    const ConstArrayView3D& Wgt_st = WeightFleet[st];
    const ConstArrayView4D& Sel_st = SelAge[st];
    const ConstArrayView4D& Ret_st = RetAge[st];

    const int nAge = Num_st.dim[0];

    // Exploitable biomass per unit effort
    for (int fl = 0; fl < nFleet; ++fl) {
      for (int ar = 0; ar < nArea; ++ar) {
        if (Closure(st, y, fl, ar) <= 0.0) continue;

        double B_sfr = 0.0;
        for (int age = 0; age < nAge; ++age) {
          B_sfr +=
            Num_st(age, y, ar) *
            Wgt_st(age, y, fl) *
            Sel_st(age, y, fl, ar) *
            Ret_st(age, y, fl, ar);
        }
        B_hat(fl, ar) = q(st, y, fl) * B_sfr;
      }
    }

     // Within-season saturation
    for (int fl = 0; fl < nFleet; ++fl) {
      const double phi = q(st, y, fl) * Effort(y, fl);

      // Median B_ref across areas
      std::vector<double> Bvec(nArea);
      for (int ar = 0; ar < nArea; ++ar)
        Bvec[ar] = B_hat(fl, ar);

      std::nth_element(
        Bvec.begin(),
        Bvec.begin() + nArea / 2,
        Bvec.end()
      );
      const double Bref = Bvec[nArea / 2];

      if (Bref <= 0.0) continue;

      // Apply stock-specific utility
      for (int ar = 0; ar < nArea; ++ar) {
        const double B = B_hat(fl, ar);
        const double A = RelSize(ar);

        if (A > 0.0 && B > 0.0) {
          const double Gamma = B / (A * Bref);
          Util(fl, ar) += B / (1.0 + phi * Gamma);
        }
      }
    }
  } // end stock loop

  // Normalize utility across areas
  for (int fl = 0; fl < nFleet; ++fl) {
    double total = 0.0;
    for (int ar = 0; ar < nArea; ++ar)
      total += Util(fl, ar);

    if (total > 0.0) {
      for (int ar = 0; ar < nArea; ++ar)
        Util(fl, ar) /= total;
    }
  }


  // Calculate Effort Distribution
  for (int fl = 0; fl < nFleet; ++fl) {
    const double theta = Targeting(y,fl);
    if (theta <= 0.0) continue;

    double total = 0.0;

    // cache Util^theta
    std::vector<double> UtilTheta(nArea, 0.0);
    for (int ar = 0; ar < nArea; ++ar) {
      const double u = Util(fl, ar);
      if (u > 0.0) {
        const double ut = std::pow(u, theta);
        UtilTheta[ar] = ut;
        total += ut;
      }
    }

    if (total > 0.0) {
      const double inv_total = 1.0 / total;
      for (int ar = 0; ar < nArea; ++ar) {
        if (Distribution(y, fl, ar) <= 1E-6) {
          Distribution(y, fl, ar) = UtilTheta[ar] * inv_total;
        }

      }
    }
  }


}

#endif // CALC_SPATIAL_DISTRIBUTION_H