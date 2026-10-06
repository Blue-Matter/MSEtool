#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

constexpr int NP = 4;

// value, gradient, and Hessian with respect to N estimated parameters
template <int N>
struct HD {
  double v;
  double g[N];
  double h[N][N];
  HD(double x = 0.0) : v(x) {
    for (int i = 0; i < N; ++i) {
      g[i] = 0.0;
      for (int j = 0; j < N; ++j) h[i][j] = 0.0;
    }
  }
};

inline double Val(double x) { return x; }
template <int N> inline double Val(const HD<N>& x) { return x.v; }

template <int N>
inline HD<N> Chain(const HD<N>& a, double f, double d1, double d2) {
  HD<N> r(f);
  for (int i = 0; i < N; ++i) {
    r.g[i] = d1 * a.g[i];
    for (int j = 0; j < N; ++j) r.h[i][j] = d1 * a.h[i][j] + d2 * a.g[i] * a.g[j];
  }
  return r;
}

template <int N>
inline HD<N> operator+(const HD<N>& a, const HD<N>& b) {
  HD<N> r(a.v + b.v);
  for (int i = 0; i < N; ++i) {
    r.g[i] = a.g[i] + b.g[i];
    for (int j = 0; j < N; ++j) r.h[i][j] = a.h[i][j] + b.h[i][j];
  }
  return r;
}
template <int N> inline HD<N> operator-(const HD<N>& a) { return Chain(a, -a.v, -1.0, 0.0); }
template <int N> inline HD<N> operator-(const HD<N>& a, const HD<N>& b) { return a + (-b); }
template <int N>
inline HD<N> operator*(const HD<N>& a, const HD<N>& b) {
  HD<N> r(a.v * b.v);
  for (int i = 0; i < N; ++i) {
    r.g[i] = a.g[i] * b.v + b.g[i] * a.v;
    for (int j = 0; j < N; ++j)
      r.h[i][j] = a.h[i][j] * b.v + b.h[i][j] * a.v + a.g[i] * b.g[j] + b.g[i] * a.g[j];
  }
  return r;
}
template <int N>
inline HD<N> Inv(const HD<N>& a) {
  return Chain(a, 1.0 / a.v, -1.0 / (a.v * a.v), 2.0 / (a.v * a.v * a.v));
}
template <int N> inline HD<N> operator/(const HD<N>& a, const HD<N>& b) { return a * Inv(b); }

template <int N> inline HD<N> operator+(const HD<N>& a, double b) { HD<N> r(a); r.v += b; return r; }
template <int N> inline HD<N> operator+(double a, const HD<N>& b) { return b + a; }
template <int N> inline HD<N> operator-(const HD<N>& a, double b) { return a + (-b); }
template <int N> inline HD<N> operator-(double a, const HD<N>& b) { return (-b) + a; }
template <int N> inline HD<N> operator*(const HD<N>& a, double b) { return Chain(a, a.v * b, b, 0.0); }
template <int N> inline HD<N> operator*(double a, const HD<N>& b) { return b * a; }
template <int N> inline HD<N> operator/(const HD<N>& a, double b) { return a * (1.0 / b); }
template <int N> inline HD<N> operator/(double a, const HD<N>& b) { return a * Inv(b); }
template <int N> inline HD<N>& operator+=(HD<N>& a, const HD<N>& b) { a = a + b; return a; }

template <int N>
inline HD<N> exp(const HD<N>& a) { double e = std::exp(a.v); return Chain(a, e, e, e); }
template <int N>
inline HD<N> log(const HD<N>& a) { return Chain(a, std::log(a.v), 1.0 / a.v, -1.0 / (a.v * a.v)); }
template <int N>
inline HD<N> sqrt(const HD<N>& a) {
  double s = std::sqrt(a.v);
  return Chain(a, s, 0.5 / s, -0.25 / (s * a.v));
}
template <int N>
inline HD<N> pow(const HD<N>& a, const HD<N>& b) { return exp(b * log(a)); }

inline double PowN1(double x, double n) { return n == 2.0 ? x : std::pow(x, n - 1.0); }
template <int N> inline HD<N> PowN1(const HD<N>& x, const HD<N>& n) { return pow(x, n - 1.0); }

template <class T>
struct SPPars {
  T FMSY;
  T MSY;
  T n;
  T K;
  T Rate;
  bool Fox;
};

template <class T>
SPPars<T> MakeSPPars(const T& FMSY, const T& MSY, const T& n, double FoxTol) {
  using std::exp; using std::pow;
  SPPars<T> p;
  p.FMSY = FMSY;
  p.MSY  = MSY;
  p.n    = n;
  p.Fox  = std::fabs(Val(n) - 1.0) < FoxTol;
  if (p.Fox) {
    p.K    = MSY / (FMSY * std::exp(-1.0));
    p.Rate = std::exp(1.0) * MSY / p.K;
  } else {
    T BMSYK = pow(n, 1.0 / (1.0 - n));
    p.K     = MSY / (FMSY * BMSYK);
    T Gamma = pow(n, n / (n - 1.0)) / (n - 1.0);
    p.Rate  = Gamma * MSY / p.K;
  }
  return p;
}

template <class T>
inline T Growth(const T& B, const SPPars<T>& p, T& dGrowth) {
  using std::log;
  T x = B / p.K;
  if (p.Fox) {
    dGrowth = -p.Rate / B;
    return -p.Rate * log(x);
  }
  T xn1 = PowN1(x, p.n);
  dGrowth = -p.Rate * (p.n - 1.0) * xn1 / B;
  return p.Rate * (1.0 - xn1);
}

template <class T>
struct YearStep {
  T BEnd;
  T Catch;
  T dCatch;
};

// sub-steps integrate dB/dt = (g(B) - F) B exactly for g held at its start-of-step value
template <class T>
YearStep<T> StepYear(const T& B0, const T& F, const SPPars<T>& p, int nSub,
                     std::vector<T>* BSub = nullptr, std::vector<T>* ZSub = nullptr) {
  using std::exp;
  const double dt = 1.0 / nSub;
  T B = B0, dB = 0.0, C = 0.0, dC = 0.0;
  for (int s = 0; s < nSub; ++s) {
    T dg;
    T g  = Growth(B, p, dg);
    T z  = g - F;
    T dz = dg * dB - 1.0;
    T e  = exp(z * dt);
    T h, dh;
    if (std::fabs(Val(z) * dt) < 1e-6) {
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
  YearStep<T> out = {B, C, dC};
  return out;
}

double SolveFValue(double B0, double CatchObs, const SPPars<double>& p, int nSub, int nItF,
                   double Fmax) {
  double F = std::min(CatchObs / B0, Fmax);
  for (int it = 0; it < nItF; ++it) {
    YearStep<double> ys = StepYear(B0, F, p, nSub);
    if (!(ys.dCatch > 0.0)) break;
    double Diff = ys.Catch - CatchObs;
    if (std::fabs(Diff) < 1e-12 * CatchObs) break;
    F -= Diff / ys.dCatch;
    if (F < 0.0) F = 0.0;
    if (F > Fmax) F = Fmax;
  }
  return F;
}

inline SPPars<double> ValPars(const SPPars<double>& p) { return p; }
template <int N>
inline SPPars<double> ValPars(const SPPars<HD<N>>& p) {
  SPPars<double> q = {p.FMSY.v, p.MSY.v, p.n.v, p.K.v, p.Rate.v, p.Fox};
  return q;
}

// two Newton steps from the converged value carry the implicit derivatives of F
template <class T>
T SolveF(const T& B0, double CatchObs, const SPPars<T>& p, int nSub, int nItF, double Fmax) {
  if (!(CatchObs > 0.0)) return T(0.0);
  double F0 = SolveFValue(Val(B0), CatchObs, ValPars(p), nSub, nItF, Fmax);
  if (!(F0 > 0.0) || !(F0 < Fmax)) return T(F0);
  T F = F0;
  for (int it = 0; it < 2; ++it) {
    YearStep<T> ys = StepYear(B0, F, p, nSub);
    if (!(Val(ys.dCatch) > 0.0)) return T(F0);
    F = F - (ys.Catch - CatchObs) / ys.dCatch;
  }
  if (!(Val(F) > 0.0) || !(Val(F) < Fmax)) return T(F0);
  return F;
}

template <class T>
inline T BAtTiming(const std::vector<T>& BSub, const std::vector<T>& ZSub, double Timing, int nSub) {
  using std::exp;
  const double dt = 1.0 / nSub;
  int s = static_cast<int>(std::floor(Timing * nSub));
  if (s < 0) s = 0;
  if (s > nSub - 1) s = nSub - 1;
  return BSub[s] * exp(ZSub[s] * (Timing - s * dt));
}

template <class T>
struct SPResult {
  T NLL;
  std::vector<T> B, F, CatchPred;
  std::vector<double> BIndex, q, Sigma, NLLIndex;
  double Penalty;
  double K;
};

template <class T>
SPResult<T> SPRun(const T* Pars, const NumericVector& Catch, const NumericMatrix& Index,
                  const NumericMatrix& SD, const NumericVector& Timing,
                  const NumericVector& Weight, const LogicalVector& EstSD, int nSub, int nItF,
                  double Fmax, double FPenalty, double FoxTol, double MinSD) {
  using std::log; using std::sqrt;
  const int nYear  = Catch.size();
  const int nIndex = Index.ncol();
  SPPars<T> p = MakeSPPars(Pars[0], Pars[1], Pars[3], FoxTol);

  SPResult<T> r;
  r.B.assign(nYear + 1, T(0.0));
  r.F.assign(nYear, T(0.0));
  r.CatchPred.assign(nYear, T(0.0));
  r.BIndex.assign(nYear * nIndex, NA_REAL);
  r.q.assign(nIndex, NA_REAL);
  r.Sigma.assign(nIndex, NA_REAL);
  r.NLLIndex.assign(nIndex, 0.0);
  r.K = Val(p.K);

  std::vector<T> BIndex(nYear * nIndex);
  std::vector<T> BSub(nSub), ZSub(nSub);

  r.B[0] = Pars[2] * p.K;
  T Penalty = 0.0;
  for (int y = 0; y < nYear; ++y) {
    r.F[y] = SolveF(r.B[y], Catch[y], p, nSub, nItF, Fmax);
    YearStep<T> ys = StepYear(r.B[y], r.F[y], p, nSub, &BSub, &ZSub);
    r.CatchPred[y] = ys.Catch;
    r.B[y + 1] = ys.BEnd;
    for (int i = 0; i < nIndex; ++i) {
      BIndex[y + i * nYear]   = BAtTiming(BSub, ZSub, Timing[i], nSub);
      r.BIndex[y + i * nYear] = Val(BIndex[y + i * nYear]);
    }
    if (Catch[y] > 0.0) {
      T d = Val(ys.Catch) > 1e-300 ? std::log(Catch[y]) - log(ys.Catch) : T(std::log(Catch[y]) - std::log(1e-300));
      Penalty += FPenalty * d * d;
    }
  }
  r.Penalty = Val(Penalty);

  T NLL = Penalty;
  for (int i = 0; i < nIndex; ++i) {
    std::vector<T> d;
    std::vector<double> s;
    for (int y = 0; y < nYear; ++y) {
      double obs = Index(y, i);
      const T& bi = BIndex[y + i * nYear];
      if (!R_finite(obs) || obs <= 0.0 || !(Val(bi) > 0.0)) continue;
      d.push_back(std::log(obs) - log(bi));
      if (!EstSD[i]) s.push_back(SD(y, i));
    }
    const int n = d.size();
    if (n == 0) continue;
    T nll = 0.0;
    if (EstSD[i]) {
      T logq = 0.0;
      for (int k = 0; k < n; ++k) logq += d[k];
      logq = logq / static_cast<double>(n);
      T ss = 0.0;
      for (int k = 0; k < n; ++k) ss += (d[k] - logq) * (d[k] - logq);
      T sig = Val(ss) / n > MinSD * MinSD ? sqrt(ss / static_cast<double>(n)) : T(MinSD);
      nll = n * log(sig) + ss / (2.0 * sig * sig);
      r.q[i]     = std::exp(Val(logq));
      r.Sigma[i] = Val(sig);
    } else {
      double sw = 0.0;
      T swd = 0.0;
      for (int k = 0; k < n; ++k) {
        double w = 1.0 / (s[k] * s[k]);
        sw  += w;
        swd += w * d[k];
      }
      T logq = swd / sw;
      for (int k = 0; k < n; ++k) {
        T res = d[k] - logq;
        nll += std::log(s[k]) + res * res / (2.0 * s[k] * s[k]);
      }
      r.q[i]     = std::exp(Val(logq));
      r.Sigma[i] = std::sqrt(n / sw);
    }
    r.NLLIndex[i] = Val(nll);
    NLL += Weight[i] * nll;
  }
  r.NLL = NLL;
  return r;
}

template <class T>
NumericVector Values(const std::vector<T>& x) {
  NumericVector out(x.size());
  for (size_t i = 0; i < x.size(); ++i) out[i] = Val(x[i]);
  return out;
}

template <int N>
NumericVector LogGrad(const HD<N>& x) {
  NumericVector out(N);
  for (int i = 0; i < N; ++i) out[i] = x.v > 0.0 ? x.g[i] / x.v : NA_REAL;
  return out;
}

template <int N>
List SPDeriv(const NumericVector& Pars, const IntegerVector& Active, const NumericVector& Catch,
             const NumericMatrix& Index, const NumericMatrix& SD, const NumericVector& Timing,
             const NumericVector& Weight, const LogicalVector& EstSD, int nSub, int nItF,
             double Fmax, double FPenalty, double FoxTol, double MinSD) {
  HD<N> P[NP];
  for (int j = 0; j < NP; ++j) P[j] = HD<N>(Pars[j]);
  for (int k = 0; k < N; ++k) {
    int j = Active[k] - 1;
    P[j].g[k]    = Pars[j];
    P[j].h[k][k] = Pars[j];
  }
  SPResult<HD<N>> r = SPRun(P, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF, Fmax,
                            FPenalty, FoxTol, MinSD);
  NumericVector Grad(N);
  NumericMatrix Hess(N, N);
  for (int i = 0; i < N; ++i) {
    Grad[i] = r.NLL.g[i];
    for (int j = 0; j < N; ++j) Hess(i, j) = r.NLL.h[i][j];
  }
  const int nYear = Catch.size();
  return List::create(Named("NLL") = r.NLL.v,
                      Named("Grad") = Grad,
                      Named("Hess") = Hess,
                      Named("GradLogB") = LogGrad(r.B[nYear]),
                      Named("GradLogF") = LogGrad(r.F[nYear - 1]));
}

}

// [[Rcpp::export]]
List SPModel_cpp(NumericVector Pars, NumericVector Catch, NumericMatrix Index,
                 NumericMatrix SD, NumericVector Timing, NumericVector Weight,
                 LogicalVector EstSD, int nSub, int nItF, double Fmax,
                 double FPenalty, double FoxTol, double MinSD, bool Report) {
  double P[NP] = {Pars[0], Pars[1], Pars[2], Pars[3]};
  SPResult<double> r = SPRun(P, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF, Fmax,
                             FPenalty, FoxTol, MinSD);
  if (!Report)
    return List::create(Named("NLL") = r.NLL);

  NumericMatrix BIndex(Catch.size(), Index.ncol());
  std::copy(r.BIndex.begin(), r.BIndex.end(), BIndex.begin());
  return List::create(Named("NLL") = r.NLL,
                      Named("NLLIndex") = wrap(r.NLLIndex),
                      Named("Penalty") = r.Penalty,
                      Named("B") = Values(r.B),
                      Named("F") = Values(r.F),
                      Named("CatchPred") = Values(r.CatchPred),
                      Named("BIndex") = BIndex,
                      Named("q") = wrap(r.q),
                      Named("Sigma") = wrap(r.Sigma),
                      Named("K") = r.K);
}

// derivatives are with respect to the log of the Active (1-based) elements of Pars
// [[Rcpp::export]]
List SPModelDeriv_cpp(NumericVector Pars, IntegerVector Active, NumericVector Catch,
                      NumericMatrix Index, NumericMatrix SD, NumericVector Timing,
                      NumericVector Weight, LogicalVector EstSD, int nSub, int nItF,
                      double Fmax, double FPenalty, double FoxTol, double MinSD) {
  switch (Active.size()) {
  case 1: return SPDeriv<1>(Pars, Active, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF,
                             Fmax, FPenalty, FoxTol, MinSD);
  case 2: return SPDeriv<2>(Pars, Active, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF,
                             Fmax, FPenalty, FoxTol, MinSD);
  case 3: return SPDeriv<3>(Pars, Active, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF,
                             Fmax, FPenalty, FoxTol, MinSD);
  case 4: return SPDeriv<4>(Pars, Active, Catch, Index, SD, Timing, Weight, EstSD, nSub, nItF,
                             Fmax, FPenalty, FoxTol, MinSD);
  default: stop("Active must have 1 to 4 elements");
  }
}

// [[Rcpp::export]]
List SPProject_cpp(NumericVector Pars, double B0, NumericVector Value, IntegerVector Type,
                   int nSub, int nItF, double Fmax, double FoxTol) {
  const int nYear = Value.size();
  SPPars<double> p = MakeSPPars<double>(Pars[0], Pars[1], Pars[3], FoxTol);
  NumericVector B(nYear + 1), F(nYear), Catch(nYear);
  B[0] = B0;
  for (int y = 0; y < nYear; ++y) {
    F[y] = Type[y] == 0 ? SolveF<double>(B[y], Value[y], p, nSub, nItF, Fmax) : Value[y];
    YearStep<double> ys = StepYear<double>(B[y], F[y], p, nSub);
    Catch[y] = ys.Catch;
    B[y + 1] = ys.BEnd;
  }
  return List::create(Named("B") = B, Named("F") = F, Named("Catch") = Catch);
}
