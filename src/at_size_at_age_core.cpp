#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector AtSize2AtAgeCore_(
    const NumericVector& MeanAtSize, const IntegerVector& dimMeanAtSize,
    const NumericVector& ASK, const IntegerVector& dimASK,
    int nSim, int nAge, int nTS, int nArea,
    bool bySim, bool byArea, bool allow_shortcut) {

  const int nSimASK = dimASK[0];
  const int nAgeASK = dimASK[1];
  const int nClassASK = dimASK[2];

  const int nSimMAS = dimMeanAtSize[0];
  const int nClassMAS = dimMeanAtSize[1];
  const int nTSMAS = dimMeanAtSize[2];

  const long outLen = byArea ?
    (long)nSim * nAge * nTS * nArea : (long)nSim * nAge * nTS;
  NumericVector out(outLen);

  const double* askp = ASK.begin();
  const double* masp = MeanAtSize.begin();
  double* outp = out.begin();

  for (int year = 0; year < nTS; ++year) {
    for (int sim = 0; sim < nSim; ++sim) {
      const int s = bySim ? sim : 0;
      const long askBase = (long)s + (long)nSimASK * (0 + (long)nAgeASK * (0 + (long)nClassASK * year));

      const int nAreaLoop = byArea ? nArea : 1;
      for (int area = 0; area < nAreaLoop; ++area) {
        const long masBase = byArea ?
          (long)sim + (long)nSimMAS * (0 + (long)nClassMAS * (year + (long)nTSMAS * area)) :
          (long)sim + (long)nSimMAS * (0 + (long)nClassMAS * year);

        bool shortcut = allow_shortcut;
        if (shortcut) {
          for (int c = 0; c < nClassASK; ++c) {
            if (masp[masBase + (long)nSimMAS * c] <= 0.99) { shortcut = false; break; }
          }
        }

        for (int a = 0; a < nAge; ++a) {
          double val;
          if (shortcut) {
            val = 1.0;
          } else {
            double acc = 0.0;
            for (int c = 0; c < nClassASK; ++c) {
              const double mas = masp[masBase + (long)nSimMAS * c];
              const double ask = askp[askBase + (long)nSimASK * a + (long)nSimASK * nAgeASK * c];
              acc += mas * ask;
            }
            val = acc;
          }

          const long outIdx = byArea ?
            (long)sim + (long)nSim * (a + (long)nAge * (year + (long)nTS * area)) :
            (long)sim + (long)nSim * (a + (long)nAge * year);
          outp[outIdx] = val;
        }
      }
    }
  }

  IntegerVector outdim = byArea ?
    IntegerVector::create(nSim, nAge, nTS, nArea) :
    IntegerVector::create(nSim, nAge, nTS);
  out.attr("dim") = outdim;
  return out;
}
