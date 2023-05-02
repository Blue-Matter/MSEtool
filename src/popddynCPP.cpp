#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;


//' Aging and Mortality for one time-step
//'
//' Project population forward one time-step given current numbers-at-age and total mortality
//'
//' @param nareas The number of spatial areas
//' @param maxage The maximum age
//' @param Ncurr A numeric matrix (maxage+1, nareas) with current numbers-at-age in each area
//' @param Zcurr A numeric matrix (maxage+1, nareas) with total mortality-at-age in each area
//'
//' @author A. Hordyk
//'
//' @keywords internal
//[[Rcpp::export]]
arma::mat popdynOneTScpp(double nareas, double maxage,
                         NumericMatrix Ncurr,  Rcpp::NumericMatrix Zcurr,
                         int plusgroup=1) {

  int n_age = maxage + 1;
  arma::mat Nnext(n_age, nareas);
  
  for (int A=0; A < nareas; A++) {
    Nnext(0, A) = 0; // Recruitment calculated later
    // Mortality
    for (int age=1; age<n_age; age++) {
      Nnext(age, A) = Ncurr(age-1, A) * exp(-Zcurr(age-1, A)); // Total mortality
    }

    if (plusgroup > 0) {
      Nnext(maxage, A) += Ncurr(maxage, A) * exp(-Zcurr(maxage, A)); // Total mortality
    }
  }

  return Nnext;
}


//' Apply the movement model to the stock for one time-step
//'
//'
//'
//' @param nareas The number of spatial areas
//' @param maxage The maximum age
//' @param mov Numeric matrix (nareas by nareas) with the movement matrix
//' @param Number A numeric matrix (maxage+1, nareas) with current numbers-at-age in each area
//'
//' @author A. Hordyk
//'
//' @export
//[[Rcpp::export]]
arma::mat movestockCPP(double nareas, double maxage, arma::cube mov, NumericMatrix Number) {

  int n_age = maxage + 1;
  arma::mat Nstore(n_age, nareas);
  arma::mat tempMat2(nareas, nareas);

  // move age-classes 1+ (age-0 recruitment already specified in R0a)
  for (int age=1; age<n_age; age++) {
    for (int AA = 0; AA < nareas; AA++) {   // (from areas)
      for (int BB = 0; BB < nareas; BB++) { // (to areas)
        arma::vec temp = mov.subcube(age, AA, BB, age, AA, BB);
        double temp2 = temp(0);
        tempMat2(BB, AA) = Number(age, AA) * temp2; // movement fractions
      }
    }
    for (int BB = 0; BB < nareas; BB++) { // to areas
      Nstore(age, BB) = sum(tempMat2.row(BB));   // sums all rows (from areas) in each column (to areas)
    }
  }
  for (int AA = 0; AA < nareas; AA++) {
     Nstore(0,AA) = Number(0,AA);
  }


  return Nstore;
}



//' Population dynamics model in CPP
//'
//' Project population forward pyears given current numbers-at-age and total mortality, etc
//' for the future years
//'
//' @param nareas The number of spatial areas
//' @param maxage The maximum age
//' @param SSBcurr A numeric vector of length nareas with the current spawning biomass in each area
//' @param Ncurr A numeric matrix (maxage+1, nareas) with current numbers-at-age in each area
//' @param pyears The number of years to project the population forward
//' @param M_age Numeric matrix (maxage+1, pyears) with natural mortality by age and year
//' @param Asize_c Numeric vector (length nareas) with size of each area
//' @param MatAge Numeric vector with proportion mature by age
//' @param WtAge Numeric matrix (maxage+1, pyears) with weight by age and year
//' @param FecAge Numeric matrix (maxage+1, pyears) with mature female weight by age and year
//' @param Vuln Numeric matrix (maxage+1, pyears) with vulnerability by age and year
//' @param Retc Numeric matrix (maxage+1, pyears) with retention by age and year
//' @param Prec Numeric vector (pyears) with recruitment error
//' @param movc Numeric array (nareas by nareas) with the movement matrix
//' @param SRrelc Integer indicating the stock-recruitment relationship to use (1 for Beverton-Holt, 2 for Ricker)
//' @param Effind Numeric vector (length pyears) with the fishing effort by year
//' @param Spat_targc Integer. Spatial targeting
//' @param hc Numeric. Steepness of stock-recruit relationship
//' @param R0c Numeric vector of length nareas with unfished recruitment by area
//' @param SSBpRc Numeric vector of length nareas with unfished spawning per recruit by area
//' @param aRc Numeric. Ricker SRR a value by area
//' @param bRc Numeric. Ricker SRR b value by area
//' @param Qc Numeric. Catchability coefficient
//' @param Fapic Numeric. Apical F value
//' @param maxF A numeric value specifying the maximum fishing mortality for any single age class
//' @param MPA Spatial closure by year and area
//' @param control Integer. 1 to use q and effort to calculate F, 2 to use Fapic (apical F) and
//' vulnerability to calculate F.
//' @param SRRfun Optional. A stock-recruit function used if `SRrelc =3` 
//' @param SRRpars Optional. A named list of arguments for `SRRfun`
//' @param plusgroup Integer. Include a plus-group (1) or not (0)?
//' @param spawn_time_frac Numeric. Fraction of the year when spawning occurs. Default = 0.
//'
//' @author A. Hordyk
//' @keywords internal
//[[Rcpp::export]]
List popdynCPP(double nareas, double maxage, arma::mat Ncurr, double pyears,
               arma::mat M_age, arma::vec Asize_c, arma::mat MatAge, arma::mat WtAge,
               arma::mat FecAge,
               arma::mat Vuln, arma::mat Retc, arma::vec Prec,
               List movc, double SRrelc, arma::vec Effind,
               double Spat_targc, double hc, NumericVector R0c, NumericVector SSBpRc,
               NumericVector aRc, NumericVector bRc, double Qc, double Fapic, double maxF,
               arma::mat MPA, int control, double SSB0c, Function SRRfun, List SRRpars,
               int plusgroup=0,
               double spawn_time_frac=0) {

  int n_age =maxage+1; // number of age classes (including age 0)
  arma::cube Narray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube N_sbarray(n_age, pyears, nareas, arma::fill::zeros); // spawning
  arma::cube Barray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube SSNarray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube SBarray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube VBarray(n_age, pyears, nareas, arma::fill::zeros);

  arma::cube Marray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube FMarray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube FMretarray(n_age, pyears, nareas, arma::fill::zeros);
  arma::cube Zarray(n_age, pyears, nareas, arma::fill::zeros);

  NumericVector tempVec(nareas);
  arma::vec fishdist(nareas);

  // make clones of spawning/recruit by area because they are updated (sometimes)
  NumericVector R0c2(clone(R0c));
  NumericVector aRc2(clone(aRc));
  NumericVector bRc2(clone(bRc));

  NumericVector SSB0a(nareas);
  double R0 = sum(R0c);

  // Beginning of Initial year
  Narray.subcube(0, 0, 0, maxage, 0, nareas-1) = Ncurr;
  N_sbarray.subcube(0, 0, 0, maxage, 0, nareas-1) = Ncurr;
  
  // account for spawn_time_frac 
  arma::mat Ncurr_sp(n_age, nareas, arma::fill::zeros); 
  for (int age=0; age<n_age; age++) {
    for (int A=0; A<nareas; A++) {
      N_sbarray(age, 0, A) = N_sbarray(age, 0, A) * exp(-M_age(age, 0)*spawn_time_frac);
      Ncurr_sp(age, A) = N_sbarray(age, 0, A);
    }
  }

  for (int A=0; A<nareas; A++) {
    Barray.subcube(0, 0, A, maxage, 0, A) = Ncurr.col(A) % WtAge.col(0);
    SSNarray.subcube(0, 0, A, maxage, 0, A) = Ncurr_sp.col(A) % MatAge.col(0);
    SBarray.subcube(0, 0, A, maxage, 0, A) = Ncurr_sp.col(A) % FecAge.col(0);
    SBarray.subcube(0, 0, A, 0, 0, A) = 0; // first age class (recruits) is always immature
    VBarray.subcube(0, 0, A, maxage, 0, A) = Ncurr.col(A) % WtAge.col(0) % Vuln.col(0);
    Marray.subcube(0, 0, A, maxage, 0, A) = M_age.col(0);
    tempVec(A) = accu(VBarray.slice(A));
  }
  
  // distribute F across areas
  fishdist = (pow(tempVec, Spat_targc))/sum((pow(tempVec, Spat_targc)));

  // calculate F at age for first year
  if (control == 1) {
    for (int A=0; A<nareas; A++) {
      FMarray.subcube(0,0, A, maxage, 0, A) =  (Effind(0) * Qc * fishdist(A) * Vuln.col(0))/Asize_c(A);
      FMretarray.subcube(0,0, A, maxage, 0, A) =  (Effind(0) * Qc * fishdist(A) * Retc.col(0))/Asize_c(A);
    }
  }
  if (control == 2) {
    for (int A=0; A<nareas; A++) {
      FMarray.subcube(0,0, A, maxage, 0, A) =  (Fapic * fishdist(A) * Vuln.col(0))/Asize_c(A);
      FMretarray.subcube(0,0, A, maxage, 0, A) =  (Fapic * fishdist(A) * Retc.col(0))/Asize_c(A);
    }
  }

  // apply Fmax condition
  arma::uvec tempvals = arma::find(FMarray > maxF);
  FMarray.elem(tempvals).fill(maxF);
  arma::uvec tempvals2 = arma::find(FMretarray > maxF);
  FMretarray.elem(tempvals2).fill(maxF);

  Zarray.subcube(0,0, 0, maxage, 0, nareas-1) = Marray.subcube(0,0, 0, maxage, 0, nareas-1) + FMarray.subcube(0,0, 0, maxage, 0, nareas-1);

  for (int yr=0; yr<(pyears-1); yr++) { //
    // Rcpp::Rcout << "yr = " << yr << std::endl;
    arma::vec SB(nareas);

    for (int A=0; A<nareas; A++) SB(A) = accu(SBarray.subcube(0, yr, A, maxage, yr, A));
    if ((yr >0) & (control==3)) SB = SSB0a;
    arma::mat Ncurr2 = Narray.subcube(0, yr, 0, maxage, yr, nareas-1);
    arma::mat Zcurr = Zarray.subcube(0, yr, 0, maxage, yr, nareas-1);
   
    // Mortality & aging
    arma::mat Nnext = popdynOneTScpp(nareas, maxage,
                             wrap(Ncurr2), wrap(Zcurr),
                             plusgroup);
  
    // Move stock - ages 1+
    arma::cube movcy = movc(yr+1);
    arma::mat NextYrN = movestockCPP(nareas, maxage, movcy, wrap(Nnext));

    // Calculate biomass after recruitment and movement
    for (int A=0; A<nareas; A++) {
      SBarray.subcube(0, yr+1, A, 0, yr+1, A) = 0;
      Barray.subcube(0, yr+1, A, maxage, yr+1, A) = NextYrN.col(A) % WtAge.col(yr+1);
      SSNarray.subcube(0, yr+1, A, maxage, yr+1, A) = NextYrN.col(A) % MatAge.col(yr+1);
      VBarray.subcube(0, yr+1, A, maxage, yr+1, A) = NextYrN.col(A) % WtAge.col(yr+1) % Vuln.col(yr+1);
      Marray.subcube(0, yr+1, A, maxage, yr+1, A) = M_age.col(yr+1);
      tempVec(A) = accu(VBarray.subcube(0, yr+1, A, maxage-1, yr+1, A));
    }

    Narray.subcube(0, yr+1, 0, maxage, yr+1, nareas-1) = NextYrN;
   
    fishdist = (pow(tempVec, Spat_targc))/sum((pow(tempVec, Spat_targc)));

    arma::vec d1(nareas);
    for (int A=0; A<nareas; A++) {
      d1(A) = MPA(yr+1,A) * fishdist(A);// historical closures
    }
    double fracE = sum(d1); // fraction of current effort in open areas
    arma::vec fracE2(nareas);
    for (int A=0; A<nareas; A++) {
      fracE2(A) = d1(A) * (fracE + (1-fracE))/fracE;
    }
    fishdist = fracE2;
  
    // calculate F at age for next year
    if (control == 1) {
      for (int A=0; A<nareas; A++) {
        FMarray.subcube(0,yr+1, A, maxage, yr+1, A) =  (Effind(yr+1) * Qc * fishdist(A) * Vuln.col(yr+1))/Asize_c(A);
        FMretarray.subcube(0,yr+1, A, maxage, yr+1, A) =  (Effind(yr+1) * Qc * fishdist(A) * Retc.col(yr+1))/Asize_c(A);
      }
    }

    if (control == 2) {
      for (int A=0; A<nareas; A++) {
        FMarray.subcube(0,yr+1, A, maxage, yr+1, A) =  (Fapic * fishdist(A) * Vuln.col(yr+1))/Asize_c(A);
        FMretarray.subcube(0,yr+1, A, maxage, yr+1, A) =  (Fapic * fishdist(A) * Retc.col(yr+1))/Asize_c(A);
      }
    }

    if (control ==3) { // simulate unfished dynamics & update recruitment by area
      Zarray.subcube(0,yr+1, 0, maxage, yr+1, nareas-1) = Marray.subcube(0,yr+1, 0, maxage, yr+1, nareas-1);

      // get spatial distribution
      for (int A=0; A<nareas; A++) {
        SSB0a(A) = accu(SBarray.subcube(0, yr+1, A, maxage, yr+1, A));
        R0c2(A) = accu(SBarray.subcube(0, yr, A, maxage, yr, A));
      }

      SSB0a =   SSB0a/ (sum(SSB0a)/SSB0c);
      R0c2 = R0c2/ (sum(R0c2)/R0);

      // recalculate recruitment parameters
      for (int A=0; A<nareas; A++) {
        bRc2(A) = log(5 * hc)/(0.8 * SSB0a(A));
        aRc2(A) = exp(bRc2(A) * SSB0a(A))/ SSBpRc(A);
      }

    } else {
      // apply Fmax condition
      arma::uvec tempvals3 = arma::find(FMarray > maxF);
      FMarray.elem(tempvals3).fill(maxF);
      arma::uvec tempvals4 = arma::find(FMretarray > maxF);
      FMretarray.elem(tempvals4).fill(maxF);
      Zarray.subcube(0,yr+1, 0, maxage, yr+1, nareas-1) = Marray.subcube(0,yr+1, 0, maxage, yr+1, nareas-1) + FMarray.subcube(0,yr+1, 0, maxage, yr+1, nareas-1);
    }
    
    // calculate spawning biomass 
    // account for spawn_time_frac for spawning biomass
    arma::mat Nnext_sp(n_age, nareas, arma::fill::zeros);
    for (int age=0; age<n_age; age++) {
      for (int A=0; A<nareas; A++) {
        Nnext_sp(age, A) = Narray(age, yr+1, A) * exp(-Zarray(age, yr+1, A)*spawn_time_frac);
      }
    }
 
    // recruitment
    double PerrYr = Prec(yr+maxage+1); // rec dev
    double SBtot = 0; // Total spawning biomass this year
    arma::vec rec(1); // Total recruitment this year
    
    // Spawning biomass before recruitment (age-0 doesn't contribute to SB)
    for (int A=0; A<nareas; A++) {
      SBarray.subcube(0, yr+1, A, maxage, yr+1, A) = Nnext_sp.col(A) % FecAge.col(yr+1);
      SB(A) = accu(SBarray.subcube(1, yr+1, A, maxage, yr+1, A)); // total spawning biomass
      SBtot += SB(A);
    }
    if (SRrelc == 1) {
      // BH SRR
      rec(0) =  PerrYr * (4*R0 * hc * SBtot)/(SSBpRc(0) * R0 * (1-hc) + (5*hc-1) * SBtot); // global recruitment
    }
    double bR = log(5 * hc)/(0.8*SSB0c);
    if (SRrelc == 2) {
      // most transparent form of the Ricker uses alpha and beta params
      rec(0) = PerrYr * aRc2(0) * SBtot * exp(-bR *SBtot);
    }
    
    if (SRrelc == 3) {
      double rec_val = as<double>(SRRfun(SBtot, SRRpars));
      rec(0) = PerrYr * rec_val;
    }
    
    // Distribute recruitment across areas
    arma::vec recdist = R0c2/sum(R0c2); // distribution of R0
    for (int A=0; A<nareas; A++) {
      Narray(0, yr+1, A) = rec(0) * recdist(A);
      Barray(0,yr+1,A) = Narray(0, yr+1, A) *  WtAge(0,yr+1); 
    }
  }

  List out(8);
  out(0) = Narray;
  out(1) = Barray;
  out(2) = SSNarray;
  out(3) = SBarray;
  out(4) = VBarray;
  out(5) = FMarray;
  out(6) = FMretarray;
  out(7) = Zarray;


  return out;
}



