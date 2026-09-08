#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;


// Cumulative survival-at-age, mirroring CalcSurvival() (R/calc-survival.R)
// for a single stock/sim/year.
static std::vector<double> CalcSurvivalVec(
    const std::vector<double>& M,
    const std::vector<double>& F,   // pass all-zero for the unfished case
    bool plusgroup,
    double spawn_time_frac,
    const std::vector<double>& semelparous
) {
  int nAge = (int)M.size();
  std::vector<double> Z(nAge);
  for (int a = 0; a < nAge; a++) Z[a] = M[a] + F[a];

  std::vector<double> surv(nAge);
  surv[0] = std::exp(-Z[0] * spawn_time_frac);
  for (int a = 1; a < nAge; a++) {
    surv[a] = surv[a - 1] *
      std::exp(-(Z[a - 1] * (1.0 - spawn_time_frac) + Z[a] * spawn_time_frac)) *
      (1.0 - semelparous[a - 1]);
  }
  if (plusgroup) {
    surv[nAge - 1] = surv[nAge - 1] / (1.0 - std::exp(-Z[nAge - 1]));
  }
  return surv;
}

// Safe divide matching ArrayDivide()'s NaN/Inf-zeroing behaviour
// (R/utils-array-operations.R): 0/0 and x/0 both become 0.
static inline double SafeDivide(double num, double den) {
  if (den == 0.0) return 0.0;
  double out = num / den;
  if (!R_finite(out)) return 0.0;
  return out;
}

//' Internal C++ per-recruit calculation (annual/non-seasonal, parallel implementation)
//'
//' Computes the apicalF-dependent per-recruit quantities for one sim/year,
//' mirroring `.CalcPerRecruitFScalar()`
//' (`R/calc-per-recruit.R`). 
//'
//' @param apicalF Scalar apical fishing mortality.
//' @param StockFleetAllocation nStock x nFleet matrix -- fraction of apicalF
//'   allocated to each fleet, per stock (constant across age).
//' @param NaturalMortalityList,MaturityList,SemelparousList,WeightList,FecundityList,NPR0List,NPR0_SPList
//'   Named lists (one element per stock) of per-age `NumericVector`s.
//' @param PlusGroupVec,SpawnTimeFracVec Per-stock scalars (length nStock).
//' @param WeightFleetRetainedList,WeightFleetSelectedList,SelectivityFleetList,RetentionFleetList,DiscardMortalityFleetList
//'   Named lists (one element per stock) of nAge x nFleet `NumericMatrix`.
//' @param IsSpawnTimeFrac Logical. Whether any stock has a non-zero spawn-time fraction.
//' @return A named `List` of per-stock (length nStock) `NumericVector`s:
//'   NPR0, NPRF, NPR0_SP, NPRF_SP (all summed-over-age, matching what
//'   `.CalcPerRecruitFScalar()` stores in the `perrecruit` slots of the same
//'   name), SPRF, Biomass, SBiomass, SProduction, Landings, Discards, Removals.
// [[Rcpp::export]]
List CalcPerRecruitFScalarCpp_(
    double apicalF,
    NumericMatrix StockFleetAllocation,
    List NaturalMortalityList,
    LogicalVector PlusGroupVec,
    List MaturityList,
    List SemelparousList,
    List WeightList,
    NumericVector SpawnTimeFracVec,
    List FecundityList,
    List WeightFleetRetainedList,
    List WeightFleetSelectedList,
    List SelectivityFleetList,
    List RetentionFleetList,
    List DiscardMortalityFleetList,
    List NPR0List,
    List NPR0_SPList,
    bool IsSpawnTimeFrac
) {
  int nStock = NaturalMortalityList.size();
  int nFleet = StockFleetAllocation.ncol();

  std::vector<int> nAgeVec(nStock);
  std::vector<std::vector<double>> FInteract(nStock), FRetain(nStock),
    FDiscardDead(nStock), FDeadTotal(nStock);

  // Pass 1: F components at the nominal apicalF.
  double ActualApicalF = 0.0;
  for (int s = 0; s < nStock; s++) {
    NumericVector M = NaturalMortalityList[s];
    int nAge = M.size();
    nAgeVec[s] = nAge;
    NumericMatrix Sel = SelectivityFleetList[s];
    NumericMatrix Ret = RetentionFleetList[s];
    NumericMatrix Dmort = DiscardMortalityFleetList[s];

    FInteract[s].assign((size_t)nAge * nFleet, 0.0);
    FRetain[s].assign((size_t)nAge * nFleet, 0.0);
    FDiscardDead[s].assign((size_t)nAge * nFleet, 0.0);
    FDeadTotal[s].assign(nAge, 0.0);

    for (int f = 0; f < nFleet; f++) {
      double apicalFAge = apicalF * StockFleetAllocation(s, f);
      for (int a = 0; a < nAge; a++) {
        size_t idx = (size_t)a + (size_t)f * nAge;
        double fi = apicalFAge * Sel(a, f);
        double fr = fi * Ret(a, f);
        double fdt = fi - fr;
        double fdd = fdt * Dmort(a, f);
        double fd = fr + fdd;
        FInteract[s][idx] = fi;
        FRetain[s][idx] = fr;
        FDiscardDead[s][idx] = fdd;
        FDeadTotal[s][a] += fd;
      }
    }
    double maxF = 0.0;
    for (int a = 0; a < nAge; a++) if (FDeadTotal[s][a] > maxF) maxF = FDeadTotal[s][a];
    if (maxF > ActualApicalF) ActualApicalF = maxF;
  }

  // Conditional apical-F rescale (mirrors the `if` block in .CalcPerRecruitFScalar()).
  if (apicalF > 0 && std::fabs(SafeDivide(ActualApicalF, apicalF) - 1.0) > 1E-2) {
    double adjust = SafeDivide(apicalF, ActualApicalF);
    for (int s = 0; s < nStock; s++) {
      int nAge = nAgeVec[s];
      NumericMatrix Ret = RetentionFleetList[s];
      NumericMatrix Dmort = DiscardMortalityFleetList[s];
      std::fill(FDeadTotal[s].begin(), FDeadTotal[s].end(), 0.0);
      for (int f = 0; f < nFleet; f++) {
        for (int a = 0; a < nAge; a++) {
          size_t idx = (size_t)a + (size_t)f * nAge;
          double fi = FInteract[s][idx] * adjust;
          double fr = fi * Ret(a, f);
          double fdt = fi - fr;
          double fdd = fdt * Dmort(a, f);
          double fd = fr + fdd;
          FInteract[s][idx] = fi;
          FRetain[s][idx] = fr;
          FDiscardDead[s][idx] = fdd;
          FDeadTotal[s][a] += fd;
        }
      }
    }
  }

  NumericVector NPR0out(nStock), NPRFout(nStock), NPR0_SPout(nStock), NPRF_SPout(nStock),
    SPRFout(nStock), Biomass(nStock), SBiomass(nStock), SProduction(nStock),
    Landings(nStock), Discards(nStock), Removals(nStock);

  CharacterVector stockNames = NaturalMortalityList.names();

  for (int s = 0; s < nStock; s++) {
    int nAge = nAgeVec[s];
    NumericVector M = NaturalMortalityList[s];
    NumericVector Mat = MaturityList[s];
    NumericVector Wt = WeightList[s];
    NumericVector Fec = FecundityList[s];
    NumericVector Semel = SemelparousList[s];
    NumericVector NPR0 = NPR0List[s];
    NumericVector NPR0_SP = NPR0_SPList[s];
    NumericMatrix WFRet = WeightFleetRetainedList[s];
    NumericMatrix WFSel = WeightFleetSelectedList[s];
    bool plusgroup = PlusGroupVec[s];
    double stf = SpawnTimeFracVec[s];

    std::vector<double> Mv(M.begin(), M.end());
    std::vector<double> Semelv(Semel.begin(), Semel.end());
    std::vector<double> Fvec(FDeadTotal[s]);

    std::vector<double> NPRF = CalcSurvivalVec(Mv, Fvec, plusgroup, 0.0, Semelv);
    std::vector<double> NPRF_SP = IsSpawnTimeFrac
      ? CalcSurvivalVec(Mv, Fvec, plusgroup, stf, Semelv)
      : NPRF;

    std::vector<double> Z(nAge), NDead(nAge);
    double sprf = 0.0, biomass = 0.0, sbiomass = 0.0, sproduction = 0.0;
    double npr0sum = 0.0, nprfsum = 0.0, npr0spsum = 0.0, nprfspsum = 0.0;
    for (int a = 0; a < nAge; a++) {
      Z[a] = Mv[a] + Fvec[a];
      NDead[a] = NPRF[a] * (1.0 - std::exp(-Z[a]));
      sprf += NPRF_SP[a] * Fec[a];
      biomass += NPRF[a] * Wt[a];
      sbiomass += NPRF_SP[a] * Wt[a] * Mat[a];
      sproduction += NPRF_SP[a] * Fec[a];
      npr0sum += NPR0[a];
      nprfsum += NPRF[a];
      npr0spsum += NPR0_SP[a];
      nprfspsum += NPRF_SP[a];
    }

    double landings = 0.0, discards = 0.0;
    for (int a = 0; a < nAge; a++) {
      double Zval = Z[a];
      for (int f = 0; f < nFleet; f++) {
        size_t idx = (size_t)a + (size_t)f * nAge;
        double fi_ratio = SafeDivide(FInteract[s][idx], Zval);
        double fr_ratio = SafeDivide(FRetain[s][idx], Zval);
        double fdd_ratio = SafeDivide(FDiscardDead[s][idx], Zval);

        double Inum = fi_ratio * NDead[a];
        double Lnum = fr_ratio * NDead[a];
        double Dnum = fdd_ratio * NDead[a];
        double IW = Inum * WFSel(a, f);
        double LW = Lnum * WFRet(a, f);
        double discTotN = Inum - Lnum;
        double DW = SafeDivide((IW - LW) * Dnum, discTotN);

        landings += Lnum * WFRet(a, f);
        discards += DW;
      }
    }

    NPR0out[s] = npr0sum;
    NPRFout[s] = nprfsum;
    NPR0_SPout[s] = npr0spsum;
    NPRF_SPout[s] = nprfspsum;
    SPRFout[s] = sprf;
    Biomass[s] = biomass;
    SBiomass[s] = sbiomass;
    SProduction[s] = sproduction;
    Landings[s] = landings;
    Discards[s] = discards;
    Removals[s] = landings + discards;
  }

  NPR0out.names() = stockNames;
  NPRFout.names() = stockNames;
  NPR0_SPout.names() = stockNames;
  NPRF_SPout.names() = stockNames;
  SPRFout.names() = stockNames;
  Biomass.names() = stockNames;
  SBiomass.names() = stockNames;
  SProduction.names() = stockNames;
  Landings.names() = stockNames;
  Discards.names() = stockNames;
  Removals.names() = stockNames;

  return List::create(
    _["NPR0"] = NPR0out,
    _["NPRF"] = NPRFout,
    _["NPR0_SP"] = NPR0_SPout,
    _["NPRF_SP"] = NPRF_SPout,
    _["SPRF"] = SPRFout,
    _["Biomass"] = Biomass,
    _["SBiomass"] = SBiomass,
    _["SProduction"] = SProduction,
    _["Landings"] = Landings,
    _["Discards"] = Discards,
    _["Removals"] = Removals
  );
}



static inline int FloorMod(int x, int m) {
  int r = x % m;
  if (r < 0) r += m;
  return r;
}

static inline int SeasAtAge0(int s0, int a0, int nSeason) {
  return (s0 + a0) % nSeason;
}

static std::vector<double> CalcNPRSeasonalMat(
    const std::vector<double>& Z,
    bool plusgroup,
    double spawn_time_frac,
    const std::vector<double>& semelparous,
    int nAge, int nSeason
) {
  std::vector<double> NPR((size_t)nAge * nSeason, 0.0);

  for (int s0 = 0; s0 < nSeason; s0++) {
    int sa0 = SeasAtAge0(s0, 0, nSeason);
    NPR[0 + s0 * nAge] = std::exp(-Z[0 + sa0 * nAge] * spawn_time_frac);

    for (int a0 = 1; a0 < nAge; a0++) {
      int s_prev = SeasAtAge0(s0, a0 - 1, nSeason);
      int s_curr = SeasAtAge0(s0, a0, nSeason);
      double semel_prev = semelparous[(a0 - 1) + s_prev * nAge];
      NPR[a0 + s0 * nAge] = NPR[(a0 - 1) + s0 * nAge] *
        std::exp(-(Z[(a0 - 1) + s_prev * nAge] * (1.0 - spawn_time_frac) +
                   Z[a0 + s_curr * nAge] * spawn_time_frac)) *
        (1.0 - semel_prev);
    }

    if (plusgroup) {
      int s_last = SeasAtAge0(s0, nAge - 1, nSeason);
      double z_last = Z[(nAge - 1) + s_last * nAge];
      double denom = 1.0 - std::exp(-z_last);
      if (denom > 1e-300) {
        NPR[(nAge - 1) + s0 * nAge] = NPR[(nAge - 1) + s0 * nAge] / denom;
      }
    }
  }
  return NPR;
}

static double AggSeasonalProduct(
    const std::vector<double>& NPR_bs,
    const std::vector<double>& q_sa,
    const std::vector<double>& pi_s,
    int nAge, int nSeason
) {
  double result = 0.0;
  for (int s0 = 0; s0 < nSeason; s0++) {
    double Y_s = 0.0;
    for (int a0 = 0; a0 < nAge; a0++) {
      int sa0 = SeasAtAge0(s0, a0, nSeason);
      Y_s += NPR_bs[a0 + s0 * nAge] * q_sa[a0 + sa0 * nAge];
    }
    result += pi_s[s0] * Y_s;
  }
  return result;
}

static double AggCrossSectional(
    const std::vector<double>& NPR_bs,
    const std::vector<double>& q_sa,
    const std::vector<double>& pi_s,
    int s_ref0, int nAge, int nSeason
) {
  double result = 0.0;
  for (int b0 = 0; b0 < nSeason; b0++) {
    int a0_start = FloorMod(s_ref0 - b0, nSeason);
    double Y_b = 0.0;
    for (int a0 = a0_start; a0 < nAge; a0 += nSeason) {
      Y_b += NPR_bs[a0 + b0 * nAge] * q_sa[a0 + s_ref0 * nAge];
    }
    result += pi_s[b0] * Y_b;
  }
  return result;
}

static double AggRefSeason(
    const std::vector<double>& NPR_bs,
    const std::vector<double>& q_sa,
    const std::vector<double>& pi_s,
    const std::vector<double>& RefSeasonWeights,
    int nAge, int nSeason
) {
  double result = 0.0;
  for (int s0 = 0; s0 < nSeason; s0++) {
    double w = RefSeasonWeights[s0];
    if (w == 0.0) continue;
    result += w * AggCrossSectional(NPR_bs, q_sa, pi_s, s0, nAge, nSeason);
  }
  return result;
}

static double AggSeasonalYield(
    const std::vector<double>& NPR_bs,
    const std::vector<double>& F_use_saf,
    const std::vector<double>& ZTotal_sa,
    const std::vector<double>& WeightFleet_saf,
    const std::vector<double>& pi_s,
    int nAge, int nSeason, int nFleet
) {
  double result = 0.0;
  for (int s0 = 0; s0 < nSeason; s0++) {
    double Y_s = 0.0;
    for (int a0 = 0; a0 < nAge; a0++) {
      int sa0 = SeasAtAge0(s0, a0, nSeason);
      double Z_a = ZTotal_sa[a0 + sa0 * nAge];
      double denom = std::max(Z_a, 2.220446e-16);
      double one_minus_exp = 1.0 - std::exp(-Z_a);
      for (int f0 = 0; f0 < nFleet; f0++) {
        size_t col = (size_t)sa0 + (size_t)f0 * nSeason;
        double F_af = F_use_saf[a0 + col * nAge];
        double W_af = WeightFleet_saf[a0 + col * nAge];
        Y_s += NPR_bs[a0 + s0 * nAge] * one_minus_exp * (F_af / denom) * W_af;
      }
    }
    result += pi_s[s0] * Y_s;
  }
  return result;
}

static double AggSeasonalDiscards(
    const std::vector<double>& NPR_bs,
    const std::vector<double>& FInteract_saf,
    const std::vector<double>& FRetain_saf,
    const std::vector<double>& FDiscDead_saf,
    const std::vector<double>& ZTotal_sa,
    const std::vector<double>& WeightFleetSelected_saf,
    const std::vector<double>& WeightFleetRetained_saf,
    const std::vector<double>& pi_s,
    int nAge, int nSeason, int nFleet
) {
  double result = 0.0;
  for (int s0 = 0; s0 < nSeason; s0++) {
    double Y_s = 0.0;
    for (int a0 = 0; a0 < nAge; a0++) {
      int sa0 = SeasAtAge0(s0, a0, nSeason);
      double Z_a = ZTotal_sa[a0 + sa0 * nAge];
      double denom = std::max(Z_a, 2.220446e-16);
      double Ndead_a = NPR_bs[a0 + s0 * nAge] * (1.0 - std::exp(-Z_a));

      for (int f0 = 0; f0 < nFleet; f0++) {
        size_t col = (size_t)sa0 + (size_t)f0 * nSeason;
        double Inum = Ndead_a * (FInteract_saf[a0 + col * nAge] / denom);
        double Lnum = Ndead_a * (FRetain_saf[a0 + col * nAge] / denom);
        double Dnum = Ndead_a * (FDiscDead_saf[a0 + col * nAge] / denom);

        double IW = Inum * WeightFleetSelected_saf[a0 + col * nAge];
        double LW = Lnum * WeightFleetRetained_saf[a0 + col * nAge];
        double discTotN = Inum - Lnum;
        double DW = (discTotN > 1e-12) ? (IW - LW) * Dnum / discTotN : 0.0;

        Y_s += DW;
      }
    }
    result += pi_s[s0] * Y_s;
  }
  return result;
}

//' Internal C++ per-recruit calculation (seasonal, parallel implementation)
//'
//' Computes the apicalF-dependent seasonal per-recruit quantities for one
//' sim/calendar-year, mirroring `.CalcPerRecruitFScalarSeasonal()`
//' (`R/calc-per-recruit.R`). NPR0/NPR0_SP and RefSeasonWeights (both
//' F-invariant) are precomputed in R and passed in.
//'
//' @param apicalF Scalar apical fishing mortality.
//' @param StockFleetAllocationList Named list (one per stock) of
//'   nSeason x nFleet `NumericMatrix` -- fraction of apicalF allocated to
//'   each fleet in each season, per stock.
//' @param NaturalMortalityList,MaturityList,SemelparousList,WeightList,FecundityList,NPR0_noList,NPR0_spList
//'   Named lists (one per stock) of nAge x nSeason `NumericMatrix`.
//' @param PlusGroupVec,SpawnTimeFracVec Per-stock scalars (length nStock).
//' @param WeightFleetRetainedList,WeightFleetSelectedList,SelectivityFleetList,RetentionFleetList,DiscardMortalityFleetList
//'   Named lists (one per stock) of nAge x (nSeason*nFleet) `NumericMatrix`
//'   (column index `season + nSeason*fleet`, 0-indexed).
//' @param SeasonalWeightsList Named list (one per stock) of length-nSeason
//'   `NumericVector` (`pi_s`, seasonal recruitment weights).
//' @param RefSeasonWeights Length-nSeason `NumericVector` (search-invariant,
//'   precomputed once per calendar year).
//' @param SPFromVec Integer, 1-based stock index (length nStock) -- which
//'   stock's spawning production each stock's SPR is computed from.
//' @param IsSpawnTimeFrac Logical. Whether any stock has a non-zero spawn-time fraction.
//' @param nSeason Integer number of seasons per calendar year.
//' @return A named `List`: per-stock (length nStock) `NumericVector`s NPR0,
//'   NPRF, NPR0_SP, NPRF_SP, SPR0f (unfished SP-per-recruit), SPRFf (fished
//'   SP-per-recruit), Biomass, SBiomass, SProduction, Landings, Discards,
//'   Removals, plus a scalar `F_annual_apical`.
// [[Rcpp::export]]
List CalcPerRecruitFScalarSeasonalCpp_(
    double apicalF,
    List StockFleetAllocationList,
    List NaturalMortalityList,
    LogicalVector PlusGroupVec,
    List MaturityList,
    List SemelparousList,
    List WeightList,
    NumericVector SpawnTimeFracVec,
    List FecundityList,
    List WeightFleetRetainedList,
    List WeightFleetSelectedList,
    List SelectivityFleetList,
    List RetentionFleetList,
    List DiscardMortalityFleetList,
    List SeasonalWeightsList,
    List NPR0_noList,
    List NPR0_spList,
    NumericVector RefSeasonWeights,
    IntegerVector SPFromVec,
    bool IsSpawnTimeFrac,
    int nSeason
) {
  int nStock = NaturalMortalityList.size();
  CharacterVector stockNames = NaturalMortalityList.names();

  std::vector<int> nAgeVec(nStock);
  std::vector<std::vector<double>> FInteract(nStock), FRetain(nStock), FDiscardDead(nStock),
    FDeadTotal(nStock);   // FDeadTotal: nAge x nSeason; others: nAge x (nSeason*nFleet)

  int nFleet = 0;

  // Pass 1: F components at the nominal apicalF.
  std::vector<double> ActualApicalF(nSeason, 0.0);
  for (int s = 0; s < nStock; s++) {
    NumericMatrix M = NaturalMortalityList[s];
    int nAge = M.nrow();
    nAgeVec[s] = nAge;

    NumericMatrix Sel = SelectivityFleetList[s];
    NumericMatrix Ret = RetentionFleetList[s];
    NumericMatrix Dmort = DiscardMortalityFleetList[s];
    nFleet = Sel.ncol() / nSeason;

    NumericMatrix StockAlloc = StockFleetAllocationList[s];  // nSeason x nFleet

    FInteract[s].assign((size_t)nAge * nSeason * nFleet, 0.0);
    FRetain[s].assign((size_t)nAge * nSeason * nFleet, 0.0);
    FDiscardDead[s].assign((size_t)nAge * nSeason * nFleet, 0.0);
    FDeadTotal[s].assign((size_t)nAge * nSeason, 0.0);

    for (int f = 0; f < nFleet; f++) {
      for (int se = 0; se < nSeason; se++) {
        double apicalFAge = apicalF * StockAlloc(se, f);
        size_t col = (size_t)se + (size_t)f * nSeason;
        for (int a = 0; a < nAge; a++) {
          size_t idx = (size_t)a + col * nAge;
          double fi = apicalFAge * Sel(a, col);
          double fr = fi * Ret(a, col);
          double fdt = fi - fr;
          double fdd = fdt * Dmort(a, col);
          double fd = fr + fdd;
          FInteract[s][idx] = fi;
          FRetain[s][idx] = fr;
          FDiscardDead[s][idx] = fdd;
          FDeadTotal[s][(size_t)a + (size_t)se * nAge] += fd;
        }
      }
    }

    // Per-season max-over-age, then max-over-stock into ActualApicalF.
    for (int se = 0; se < nSeason; se++) {
      double maxF = 0.0;
      for (int a = 0; a < nAge; a++) {
        double v = FDeadTotal[s][(size_t)a + (size_t)se * nAge];
        if (v > maxF) maxF = v;
      }
      if (maxF > ActualApicalF[se]) ActualApicalF[se] = maxF;
    }
  }

  // Conditional apical-F rescale (mirrors the `if` block in
  // .CalcPerRecruitFScalarSeasonal()) 
  bool anyOffTarget = false;
  if (apicalF > 0) {
    for (int se = 0; se < nSeason; se++) {
      if (std::fabs(SafeDivide(ActualApicalF[se], apicalF) - 1.0) > 1E-2) { anyOffTarget = true; break; }
    }
  }
  if (anyOffTarget) {
    std::vector<double> adjust(nSeason);
    for (int se = 0; se < nSeason; se++) adjust[se] = SafeDivide(apicalF, ActualApicalF[se]);

    for (int s = 0; s < nStock; s++) {
      int nAge = nAgeVec[s];
      NumericMatrix Ret = RetentionFleetList[s];
      NumericMatrix Dmort = DiscardMortalityFleetList[s];
      std::fill(FDeadTotal[s].begin(), FDeadTotal[s].end(), 0.0);
      for (int f = 0; f < nFleet; f++) {
        for (int se = 0; se < nSeason; se++) {
          size_t col = (size_t)se + (size_t)f * nSeason;
          for (int a = 0; a < nAge; a++) {
            size_t idx = (size_t)a + col * nAge;
            double fi = FInteract[s][idx] * adjust[se];
            double fr = fi * Ret(a, col);
            double fdt = fi - fr;
            double fdd = fdt * Dmort(a, col);
            double fd = fr + fdd;
            FInteract[s][idx] = fi;
            FRetain[s][idx] = fr;
            FDiscardDead[s][idx] = fdd;
            FDeadTotal[s][(size_t)a + (size_t)se * nAge] += fd;
          }
        }
      }
    }
  }

  NumericVector NPR0out(nStock), NPRFout(nStock), NPR0_SPout(nStock), NPRF_SPout(nStock),
    SPR0f_out(nStock), SPRFf_out(nStock), Biomass(nStock), SBiomass(nStock),
    SProduction(nStock), Landings(nStock), Discards(nStock), Removals(nStock);
  std::vector<double> FDeadTotalAnnualByAge_max(nStock, 0.0);

  for (int s = 0; s < nStock; s++) {
    int nAge = nAgeVec[s];
    NumericMatrix M = NaturalMortalityList[s];
    NumericMatrix Mat = MaturityList[s];
    NumericMatrix Wt = WeightList[s];
    NumericMatrix Fec = FecundityList[s];
    NumericMatrix Semel = SemelparousList[s];
    NumericMatrix NPR0_no = NPR0_noList[s];
    NumericMatrix NPR0_sp = NPR0_spList[s];
    NumericVector pi_s = SeasonalWeightsList[s];
    bool plusgroup = PlusGroupVec[s];
    double stf = SpawnTimeFracVec[s];

    std::vector<double> Mv(M.begin(), M.end());
    std::vector<double> Semelv(Semel.begin(), Semel.end());
    std::vector<double> pi_sv(pi_s.begin(), pi_s.end());

    // ZDeadTotal = FDeadTotal + M, both nAge x nSeason
    std::vector<double> Z((size_t)nAge * nSeason);
    for (size_t i = 0; i < Z.size(); i++) Z[i] = Mv[i] + FDeadTotal[s][i];

    std::vector<double> NPRF_no = CalcNPRSeasonalMat(Z, plusgroup, 0.0, Semelv, nAge, nSeason);
    std::vector<double> NPRF_sp = IsSpawnTimeFrac
      ? CalcNPRSeasonalMat(Z, plusgroup, stf, Semelv, nAge, nSeason)
      : NPRF_no;

    std::vector<double> ones((size_t)nAge * nSeason, 1.0);
    std::vector<double> NPR0_no_v(NPR0_no.begin(), NPR0_no.end());
    std::vector<double> NPR0_sp_v(NPR0_sp.begin(), NPR0_sp.end());
    std::vector<double> Fecv(Fec.begin(), Fec.end());
    std::vector<double> Wtv(Wt.begin(), Wt.end());
    std::vector<double> Matv(Mat.begin(), Mat.end());
    std::vector<double> WMatv((size_t)nAge * nSeason);
    for (size_t i = 0; i < WMatv.size(); i++) WMatv[i] = Wtv[i] * Matv[i];

    double npr0_ann    = AggSeasonalProduct(NPR0_no_v, ones, pi_sv, nAge, nSeason);
    double npr0sp_ann  = AggSeasonalProduct(NPR0_sp_v, ones, pi_sv, nAge, nSeason);
    double nprf_ann    = AggSeasonalProduct(NPRF_no,   ones, pi_sv, nAge, nSeason);
    double nprfsp_ann  = AggSeasonalProduct(NPRF_sp,   ones, pi_sv, nAge, nSeason);

    double spr0f_ann = AggSeasonalProduct(NPR0_sp_v, Fecv, pi_sv, nAge, nSeason);
    double sprff_ann = AggSeasonalProduct(NPRF_sp,   Fecv, pi_sv, nAge, nSeason);

    double biomass_ann  = AggRefSeason(NPRF_no, Wtv,  pi_sv, std::vector<double>(RefSeasonWeights.begin(), RefSeasonWeights.end()), nAge, nSeason);
    double sbiomass_ann = AggRefSeason(NPRF_sp, WMatv, pi_sv, std::vector<double>(RefSeasonWeights.begin(), RefSeasonWeights.end()), nAge, nSeason);

    NumericMatrix WFRet = WeightFleetRetainedList[s];
    NumericMatrix WFSel = WeightFleetSelectedList[s];
    std::vector<double> WFRetv(WFRet.begin(), WFRet.end());
    std::vector<double> WFSelv(WFSel.begin(), WFSel.end());

    double landings_ann = AggSeasonalYield(NPRF_no, FRetain[s], Z, WFRetv, pi_sv, nAge, nSeason, nFleet);
    double discards_ann = AggSeasonalDiscards(NPRF_no, FInteract[s], FRetain[s], FDiscardDead[s],
                                              Z, WFSelv, WFRetv, pi_sv, nAge, nSeason, nFleet);

    NPR0out[s]     = npr0_ann;
    NPR0_SPout[s]  = npr0sp_ann;
    NPRFout[s]     = nprf_ann;
    NPRF_SPout[s]  = nprfsp_ann;
    SPR0f_out[s]   = spr0f_ann;
    SPRFf_out[s]   = sprff_ann;
    Biomass[s]     = biomass_ann;
    SBiomass[s]    = sbiomass_ann;
    SProduction[s] = sprff_ann;
    Landings[s]    = landings_ann;
    Discards[s]    = discards_ann;
    Removals[s]    = landings_ann + discards_ann;

    // Annual apical F for this stock: sum FDeadTotal over seasons per age, max over age.
    double stock_max = 0.0;
    for (int a = 0; a < nAge; a++) {
      double annual_by_age = 0.0;
      for (int se = 0; se < nSeason; se++) annual_by_age += FDeadTotal[s][(size_t)a + (size_t)se * nAge];
      if (annual_by_age > stock_max) stock_max = annual_by_age;
    }
    FDeadTotalAnnualByAge_max[s] = stock_max;
  }

  double F_annual_apical = 0.0;
  for (int s = 0; s < nStock; s++) if (FDeadTotalAnnualByAge_max[s] > F_annual_apical) F_annual_apical = FDeadTotalAnnualByAge_max[s];

  // Apply SPFrom: SPR[i] = SPRFf[SPFrom[i]] / max(SPR0f[SPFrom[i]], eps).
  NumericVector SPR(nStock);
  for (int i = 0; i < nStock; i++) {
    int spf = SPFromVec[i] - 1;  // 1-based -> 0-based
    double denom = std::max(SPR0f_out[spf], 2.220446e-16);
    SPR[i] = SPRFf_out[spf] / denom;
  }

  NPR0out.names() = stockNames;
  NPRFout.names() = stockNames;
  NPR0_SPout.names() = stockNames;
  NPRF_SPout.names() = stockNames;
  SPR0f_out.names() = stockNames;
  SPRFf_out.names() = stockNames;
  SPR.names() = stockNames;
  Biomass.names() = stockNames;
  SBiomass.names() = stockNames;
  SProduction.names() = stockNames;
  Landings.names() = stockNames;
  Discards.names() = stockNames;
  Removals.names() = stockNames;

  return List::create(
    _["NPR0"] = NPR0out,
    _["NPRF"] = NPRFout,
    _["NPR0_SP"] = NPR0_SPout,
    _["NPRF_SP"] = NPRF_SPout,
    _["SPR0f"] = SPR0f_out,
    _["SPRFf"] = SPRFf_out,
    _["SPR"] = SPR,
    _["Biomass"] = Biomass,
    _["SBiomass"] = SBiomass,
    _["SProduction"] = SProduction,
    _["Landings"] = Landings,
    _["Discards"] = Discards,
    _["Removals"] = Removals,
    _["F_annual_apical"] = F_annual_apical
  );
}
