#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector AtAge2AtSizeCore_(
    const NumericVector& ObjectMeanAtAge, const IntegerVector& dimOMA,
    const NumericVector& ASK, const IntegerVector& dimASK,
    int nSim, int nYear, bool hasArea, int nArea) {

  const int nSimASK = dimASK[0];
  const int nAge = dimASK[1];
  const int nClass = dimASK[2];
  const int nYearASK = dimASK[3];

  const int nSimOMA = dimOMA[0];
  const int nYearOMA = dimOMA[2];

  const double* askp = ASK.begin();
  const double* omap = ObjectMeanAtAge.begin();

  const int nAreaLoop = hasArea ? nArea : 1;

  const long outLen = hasArea ?
    (long)nSim * nClass * nYear * nArea : (long)nSim * nClass * nYear;
  NumericVector out(outLen);
  double* outp = out.begin();

  // Precompute standardized ASK and no_mass for each distinct (ASK_s, ASK_y) pair
  const int nCache = nSimASK * nYearASK;
  std::vector<double> askStand((long)nCache * nAge * nClass);
  std::vector<char> noMass((long)nCache * nClass);
  std::vector<char> cached(nCache, 0);

  auto ensureCached = [&](int ask_s, int ask_y) -> int {
    const int key = ask_s + nSimASK * ask_y;
    if (cached[key]) return key;

    const long askBase = (long)ask_s + (long)nSimASK * (0 + (long)nAge * (0 + (long)nClass * ask_y));
    std::vector<double> classSum(nClass, 0.0);
    for (int c = 0; c < nClass; ++c) {
      double s = 0.0;
      for (int a = 0; a < nAge; ++a)
        s += askp[askBase + (long)nSimASK * a + (long)nSimASK * nAge * c];
      classSum[c] = s;
    }

    double* standOut = &askStand[(long)key * nAge * nClass];
    char* noMassOut = &noMass[(long)key * nClass];
    for (int c = 0; c < nClass; ++c) {
      const bool zeroSum = (classSum[c] == 0.0);
      noMassOut[c] = zeroSum ? 1 : 0;
      for (int a = 0; a < nAge; ++a) {
        const double v = askp[askBase + (long)nSimASK * a + (long)nSimASK * nAge * c];
        double stand = v / classSum[c];
        if (!R_FINITE(stand)) stand = 0.0;
        standOut[a + (long)nAge * c] = stand;
      }
    }
    cached[key] = 1;
    return key;
  };

  std::vector<double> atlength(nClass);
  std::vector<char> isna(nClass);

  for (int y = 0; y < nYear; ++y) {
    const int ask_y = std::min(nYearASK - 1, y);
    for (int s = 0; s < nSim; ++s) {
      const int ask_s = std::min(nSimASK - 1, s);
      const int obj_s = std::min(nSimOMA - 1, s);
      const int obj_y = std::min(nYearOMA - 1, y);
      const int key = ensureCached(ask_s, ask_y);
      const double* standp = &askStand[(long)key * nAge * nClass];
      const char* noMassp = &noMass[(long)key * nClass];

      for (int area = 0; area < nAreaLoop; ++area) {
        const long omaBase = hasArea ?
          (long)obj_s + (long)nSimOMA * (0 + (long)nAge * (obj_y + (long)nYearOMA * area)) :
          (long)obj_s + (long)nSimOMA * (0 + (long)nAge * obj_y);

        for (int c = 0; c < nClass; ++c) {
          if (noMassp[c]) {
            atlength[c] = 0.0;
            isna[c] = 1;
          } else {
            double acc = 0.0;
            for (int a = 0; a < nAge; ++a)
              acc += omap[omaBase + (long)nSimOMA * a] * standp[a + (long)nAge * c];
            atlength[c] = acc;
            isna[c] = 0;
          }
        }

        // forward fill
        double last = 0.0; bool hasLast = false;
        for (int c = 0; c < nClass; ++c) {
          if (!isna[c]) { last = atlength[c]; hasLast = true; }
          else if (hasLast) { atlength[c] = last; isna[c] = 0; }
        }
        // backward fill
        hasLast = false;
        for (int c = nClass - 1; c >= 0; --c) {
          if (!isna[c]) { last = atlength[c]; hasLast = true; }
          else if (hasLast) { atlength[c] = last; isna[c] = 0; }
        }
        // remaining NA (all classes had no mass) -> 0
        for (int c = 0; c < nClass; ++c)
          if (isna[c]) atlength[c] = 0.0;

        const long outBase = hasArea ?
          (long)s + (long)nSim * (0 + (long)nClass * (y + (long)nYear * area)) :
          (long)s + (long)nSim * (0 + (long)nClass * y);
        for (int c = 0; c < nClass; ++c)
          outp[outBase + (long)nSim * c] = atlength[c];
      }
    }
  }

  IntegerVector outdim = hasArea ?
    IntegerVector::create(nSim, nClass, nYear, nArea) :
    IntegerVector::create(nSim, nClass, nYear);
  out.attr("dim") = outdim;
  return out;
}
