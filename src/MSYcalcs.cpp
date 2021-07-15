#include <Rcpp.h>
using namespace Rcpp;

//'  Internal function to calculate MSY Reference Points
//'
//' @param logF log fishing mortality
//' @param M_at_Age Vector of M-at-age
//' @param Wt_at_Age Vector of weight-at-age
//' @param Mat_at_Age Vector of maturity-at-age
//' @param Fec_at_Age Vector of mature weight-at-age
//' @param V_at_Age Vector of selectivity-at-age
//' @param maxage Maximum age
//' @param R0x R0 for this simulation
//' @param SRrelx SRR type for this simulation. Use 3 for per-recruit calculations, i.e. constant recruitment.
//' @param hx numeric. Steepness value for this simulation
//' @param opt Option. 1 = return -Yield, 2= return all MSY calcs
//' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
//' @return See `opt`
// [[Rcpp::export]]
NumericVector MSYCalcs(double logF,
                  NumericVector M_at_Age,
                  NumericVector Wt_at_Age,
                  NumericVector Mat_at_Age,
                  NumericVector Fec_at_Age,
                  NumericVector V_at_Age,
                  int maxage,
                  double R0x,
                  int SRrelx,
                  double hx,
                  int opt=1,
                  int plusgroup=1) {
  // Box 3.1 Walters & Martell 2004
  int n_age = maxage + 1;
  double FF = std::exp(logF);
  NumericVector lx(n_age, 1.0);
  NumericVector l0(n_age, 1.0);

  // no doubt there's a neater way to do this...
  IntegerVector idx = seq(1, n_age-1);
  IntegerVector idx2 = seq(0, n_age-2);
  NumericVector temp = M_at_Age[idx2];
  NumericVector temp2 = cumsum(temp);
  NumericVector temp3 = exp(-1.0 * temp2);
  l0[idx] = temp3;

  NumericVector F_at_Age = FF * V_at_Age;
  NumericVector Z_at_Age = M_at_Age + F_at_Age;
  NumericVector surv = Rcpp::cumprod(exp(-Z_at_Age));
  lx[idx] = surv[idx2];

  if (plusgroup>0) {
    l0[n_age-1] = l0[n_age-1]/(1-exp(-M_at_Age[n_age-1]));
    lx[n_age-1] = lx[n_age-1]/(1-exp(-Z_at_Age[n_age-1]));
  }

  double Egg0 = sum(l0 * Fec_at_Age);
  double EggF = sum(lx * Fec_at_Age);
  double vB0 = sum(l0 * Wt_at_Age * V_at_Age); // unfished and fished vuln. biomass per-recruit
  double vBF = sum(lx * Wt_at_Age * V_at_Age);
  double SB0 = sum(l0 * Fec_at_Age); // spawning biomass per-recruit - same as eggs atm
  double SBF = sum(lx * Fec_at_Age);
  double B0 = sum(l0 * Wt_at_Age); // biomass-per-recruit
  double BF = sum(lx * Wt_at_Age);

  // double SPR = EggF/Egg0;
  double RelRec = 0;
  if (hx >0.999) hx = 0.999;

  if (SRrelx==1) {
    double recK = (4*hx)/(1-hx); // Goodyear compensation ratio
    double reca = recK/Egg0;
    double recb = (reca*Egg0-1)/(R0x*Egg0);
    RelRec = (reca*EggF-1)/(recb*EggF);
  }
  if (SRrelx==2) {
    double bR = (log(5*hx)/(0.8*SB0));
    double aR = exp(bR*SB0)/(SB0/R0x);
    RelRec = (log(aR*EggF/R0x))/(bR*EggF/R0x);
  }
  if (SRrelx==3) {
    RelRec = R0x;
  }
  if (RelRec<0) RelRec = 0;

  double YPR =  sum(lx * Wt_at_Age * F_at_Age * (1 - exp(-Z_at_Age))/Z_at_Age);
  double Yield = YPR * RelRec;

  if (opt ==1) {
    NumericVector out(1);
    out[0] = -Yield;
    return(out);
  } else {

    NumericVector out(11);
    out[0] = Yield;
    out[1] = FF;
    out[2] = SBF * RelRec;
    out[3] = (SBF * RelRec)/(SB0 * R0x);
    out[4] = (BF * RelRec)/(B0 * R0x);
    out[5] = BF * RelRec;
    out[6] = vBF * RelRec;
    out[7] = (vBF * RelRec)/(vB0 * R0x);
    out[8]=RelRec;
    out[9] = SB0*R0x;
    out[10] = B0*R0x;
    out.names() = CharacterVector::create("Yield", "F", "SB", "SB_SB0", "B_B0", "B", "VB", "VB_VB0", "RelRec", "SB0", "B0");

    return(out);
  }

}


// [[Rcpp::export]]
NumericMatrix Ref_int_cpp(NumericVector F_search,
              NumericVector M_at_Age,
              NumericVector Wt_at_Age,
              NumericVector Mat_at_Age,
              NumericVector Fec_at_Age,
              NumericVector V_at_Age,
              int SRrelx,
              int maxage,
              int plusgroup=1) {

  int ncol = F_search.length();
  NumericMatrix out(3,ncol);

  for (int i=0; i<ncol; i++) {
    double logF = log(F_search[i]);
    NumericVector msys = MSYCalcs(logF, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age,
                                      V_at_Age, maxage, 1, SRrelx, 1,2,plusgroup);
    out(0,i) = msys[0];
    out(1,i) = msys[3];
    out(2,i) = msys[8]/msys[2];
  }

  return(out);
}
