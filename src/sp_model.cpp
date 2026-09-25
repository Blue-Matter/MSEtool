#include <Rcpp.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

struct SPPars {
  double FMSY;
  double MSY;
  double n;
  double K;
  double Rate;
  bool Fox;
};

SPPars MakeSPPars(double FMSY, double MSY, double n, double FoxTol) {
  SPPars p;
  p.FMSY = FMSY;
  p.MSY  = MSY;
  p.n    = n;
  p.Fox  = std::fabs(n - 1.0) < FoxTol;
  double BMSYK = p.Fox ? std::exp(-1.0) : std::pow(n, 1.0 / (1.0 - n));
  p.K = MSY / (FMSY * BMSYK);
  double Gamma = p.Fox ? std::exp(1.0) : std::pow(n, n / (n - 1.0)) / (n - 1.0);
  p.Rate = Gamma * MSY / p.K;
  return p;
}

inline double Growth(double B, const SPPars& p, double& dGrowth) {
  double x = B / p.K;
  if (p.Fox) {
    dGrowth = -p.Rate / B;
    return -p.Rate * std::log(x);
  }
  double xn1 = p.n == 2.0 ? x : std::pow(x, p.n - 1.0);
  dGrowth = -p.Rate * (p.n - 1.0) * xn1 / B;
  return p.Rate * (1.0 - xn1);
}

struct YearStep {
  double BEnd;
  double Catch;
  double dCatch;
};

// sub-steps integrate dB/dt = (g(B) - F) B exactly for g held at its start-of-step value
YearStep StepYear(double B0, double F, const SPPars& p, int nSub,
                  std::vector<double>* BSub = nullptr, std::vector<double>* ZSub = nullptr) {
  const double dt = 1.0 / nSub;
  double B = B0, dB = 0.0, C = 0.0, dC = 0.0;
  for (int s = 0; s < nSub; ++s) {
    double dg;
    double g  = Growth(B, p, dg);
    double z  = g - F;
    double dz = dg * dB - 1.0;
    double e  = std::exp(z * dt);
    double h, dh;
    if (std::fabs(z * dt) < 1e-6) {
      h  = dt * (1.0 + z * dt / 2.0);
      dh = dt * dt / 2.0 * (1.0 + 2.0 * z * dt / 3.0);
    } else {
      h  = (e - 1.0) / z;
      dh = (dt * e * z - (e - 1.0)) / (z * z);
    }
    if (BSub) (*BSub)[s] = B;
    if (ZSub) (*ZSub)[s] = z;
    C  += F * B * h;
    dC += B * h + F * h * dB + F * B * dh * dz;
    dB  = e * (dB + B * dt * dz);
    B   = B * e;
  }
  YearStep out = {B, C, dC};
  return out;
}

double SolveF(double B0, double CatchObs, const SPPars& p, int nSub, int nItF, double Fmax) {
  if (!(CatchObs > 0.0)) return 0.0;
  double F = std::min(CatchObs / B0, Fmax);
  for (int it = 0; it < nItF; ++it) {
    YearStep ys = StepYear(B0, F, p, nSub);
    if (!(ys.dCatch > 0.0)) break;
    double Diff = ys.Catch - CatchObs;
    if (std::fabs(Diff) < 1e-12 * CatchObs) break;
    F -= Diff / ys.dCatch;
    if (F < 0.0) F = 0.0;
    if (F > Fmax) F = Fmax;
  }
  return F;
}

inline double BAtTiming(const std::vector<double>& BSub, const std::vector<double>& ZSub,
                        double Timing, int nSub) {
  const double dt = 1.0 / nSub;
  int s = static_cast<int>(std::floor(Timing * nSub));
  if (s < 0) s = 0;
  if (s > nSub - 1) s = nSub - 1;
  return BSub[s] * std::exp(ZSub[s] * (Timing - s * dt));
}

}

// [[Rcpp::export]]
List SPModel_cpp(NumericVector Pars, NumericVector Catch, NumericMatrix Index,
                 NumericMatrix SD, NumericVector Timing, NumericVector Weight,
                 LogicalVector EstSD, int nSub, int nItF, double Fmax,
                 double FPenalty, double FoxTol, double MinSD, bool Report) {

  const int nYear  = Catch.size();
  const int nIndex = Index.ncol();
  SPPars p = MakeSPPars(Pars[0], Pars[1], Pars[3], FoxTol);

  NumericVector B(nYear + 1), F(nYear), CatchPred(nYear);
  NumericMatrix BIndex(nYear, nIndex);
  std::vector<double> BSub(nSub), ZSub(nSub);

  B[0] = Pars[2] * p.K;
  double Penalty = 0.0;
  for (int y = 0; y < nYear; ++y) {
    F[y] = SolveF(B[y], Catch[y], p, nSub, nItF, Fmax);
    YearStep ys = StepYear(B[y], F[y], p, nSub, &BSub, &ZSub);
    CatchPred[y] = ys.Catch;
    B[y + 1] = ys.BEnd;
    for (int i = 0; i < nIndex; ++i)
      BIndex(y, i) = BAtTiming(BSub, ZSub, Timing[i], nSub);
    if (Catch[y] > 0.0) {
      double d = std::log(Catch[y]) - std::log(std::max(ys.Catch, 1e-300));
      Penalty += FPenalty * d * d;
    }
  }

  NumericVector q(nIndex, NA_REAL), Sigma(nIndex, NA_REAL), NLLIndex(nIndex);
  double NLL = Penalty;
  for (int i = 0; i < nIndex; ++i) {
    std::vector<double> d, s;
    for (int y = 0; y < nYear; ++y) {
      double obs = Index(y, i);
      if (!R_finite(obs) || obs <= 0.0 || !(BIndex(y, i) > 0.0)) continue;
      d.push_back(std::log(obs) - std::log(BIndex(y, i)));
      if (!EstSD[i]) s.push_back(SD(y, i));
    }
    const int n = d.size();
    if (n == 0) continue;
    double nll = 0.0;
    if (EstSD[i]) {
      double logq = 0.0;
      for (int k = 0; k < n; ++k) logq += d[k];
      logq /= n;
      double ss = 0.0;
      for (int k = 0; k < n; ++k) ss += (d[k] - logq) * (d[k] - logq);
      double sig = std::max(std::sqrt(ss / n), MinSD);
      nll = n * std::log(sig) + ss / (2.0 * sig * sig);
      q[i] = std::exp(logq);
      Sigma[i] = sig;
    } else {
      double sw = 0.0, swd = 0.0;
      for (int k = 0; k < n; ++k) {
        double w = 1.0 / (s[k] * s[k]);
        sw  += w;
        swd += w * d[k];
      }
      double logq = swd / sw;
      for (int k = 0; k < n; ++k) {
        double r = d[k] - logq;
        nll += std::log(s[k]) + r * r / (2.0 * s[k] * s[k]);
      }
      q[i] = std::exp(logq);
      Sigma[i] = std::sqrt(n / sw);
    }
    NLLIndex[i] = nll;
    NLL += Weight[i] * nll;
  }

  if (!Report)
    return List::create(Named("NLL") = NLL);

  return List::create(Named("NLL") = NLL,
                      Named("NLLIndex") = NLLIndex,
                      Named("Penalty") = Penalty,
                      Named("B") = B,
                      Named("F") = F,
                      Named("CatchPred") = CatchPred,
                      Named("BIndex") = BIndex,
                      Named("q") = q,
                      Named("Sigma") = Sigma,
                      Named("K") = p.K);
}

// [[Rcpp::export]]
List SPProject_cpp(NumericVector Pars, double B0, NumericVector Value, IntegerVector Type,
                   int nSub, int nItF, double Fmax, double FoxTol) {
  const int nYear = Value.size();
  SPPars p = MakeSPPars(Pars[0], Pars[1], Pars[3], FoxTol);
  NumericVector B(nYear + 1), F(nYear), Catch(nYear);
  B[0] = B0;
  for (int y = 0; y < nYear; ++y) {
    F[y] = Type[y] == 0 ? SolveF(B[y], Value[y], p, nSub, nItF, Fmax) : Value[y];
    YearStep ys = StepYear(B[y], F[y], p, nSub);
    Catch[y] = ys.Catch;
    B[y + 1] = ys.BEnd;
  }
  return List::create(Named("B") = B, Named("F") = F, Named("Catch") = Catch);
}
