#ifndef MSEtool_CALCAVAILBIOMASS_H
#define MSEtool_CALCAVAILBIOMASS_H

#include <Rcpp.h>
#include "array2d.h"
#include "array3d.h"
#include "array4d.h"

Array3D CalcAvailBiomass_(const Array3D& Num,
                          const Array3D& Weight,
                          const Array4D& Sel,
                          const Array4D& Ret,
                          const Array2D& q);

#endif
