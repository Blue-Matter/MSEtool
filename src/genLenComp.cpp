#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;



// https://stackoverflow.com/questions/30175104/how-to-effectively-combine-a-list-of-numericvectors-into-one-large-numericvector
// [[Rcpp::export]]
NumericVector combine(const List& list)
{
  std::size_t n = list.size();

  // Figure out the length of the output vector
  std::size_t total_length = 0;
  for (std::size_t i = 0; i < n; ++i)
    total_length += Rf_length(list[i]);

  // Allocate the vector
  NumericVector output = no_init(total_length);

  // Loop and fill
  std::size_t index = 0;
  for (std::size_t i = 0; i < n; ++i)
  {
    NumericVector el = list[i];
    std::copy(el.begin(), el.end(), output.begin() + index);
    // Update the index
    index += el.size();
  }
  return output;
}


// https://stackoverflow.com/questions/13661065/superimpose-histogram-fits-in-one-plot-ggplot

// [[Rcpp::export]]
NumericVector get_freq(NumericVector x, double width, double origin = 0,
                       int outlen=0) {
  int bin= 0;
  int nmissing = 0;
  std::vector<int> out(outlen);
  for(NumericVector::iterator x_it = x.begin(); x_it != x.end(); ++x_it) {
    double val = *x_it;
    if (ISNAN(val)) {
      ++nmissing;
    } else {
      bin = (val - origin) / width;
      ++out[bin];
    }
  }
  return wrap(out);
}

// [[Rcpp::export]]
NumericVector get_freq2(NumericVector x, NumericVector CAL_bins, int outlen=0) {
  std::vector<int> out(outlen);
  for(int i=0;i<x.size();i++) {
    double val = x(i);
    int bin = 0;
    if (!ISNAN(val)) {
      for(int j=1;j<=outlen;j++) {
        if(val > CAL_bins(j)) {
          bin += 1;
        } else {
          break;
        }
      }
      out[bin] += 1;
    }
  }
  return wrap(out);
}


//// [[Rcpp::export]]
//int which_maxC(NumericVector x){
//  //int out;
//  //out = std::distance(x.begin(),std::max_element(x.begin(),x.end()));
//  //out++;
//  return out;
//}



// https://stackoverflow.com/questions/14034200/efficient-random-number-generation-from-a-truncated-normal-distribution

// [[Rcpp::export]]
NumericVector rnormSelect2(int N, int mi, int ma) {
  int N2 = N * 1.25;
  NumericVector X = rnorm(N2, 0, 1);
  LogicalVector ind = (X >= mi) & (X <= ma);
  NumericVector Y(N);
  int k=0;
  for (int i=0; (i<N2) & (k<N); i++) {
    if (ind[i]) Y(k++) = X(i);
  }
  return Y;
}

// [[Rcpp::export]]
NumericVector tdnorm(NumericVector x, double mi, double ma) {
  NumericVector dist = dnorm(x, 0.0, 1.0);
  NumericVector cdist = pnorm(x, 0.0, 1.0);
  LogicalVector ind = (cdist < R::pnorm(mi, 0.0, 1.0,1,0)) | (cdist >R::pnorm(ma, 0.0, 1.0,1,0));
  double sz = dist.size();
  int maxind = which_max(dist);
  NumericVector Y = dist;
  for (int i=0; (i<sz); i++) { // truncate
    if (ind[i]) Y(i) = 0;
  }
  if (sum(Y)==0) Y[maxind] = 1;
  Y = Y/sum(Y);
  return Y;
}



// [[Rcpp::export]]
NumericMatrix genSizeComp(NumericMatrix VulnN, 
                          NumericVector CAL_binsmid, 
                          NumericVector CAL_bins, 
                          NumericMatrix selCurve,
                          double CAL_ESS, 
                          double CAL_nsamp,
                          NumericVector Linfs, 
                          NumericVector Ks, 
                          NumericVector t0s,
                          double LenCV, 
                          double truncSD) {

  int nyears = VulnN.nrow();
  int k = VulnN.ncol(); // number of age classes
  int nbins = CAL_binsmid.size();
  NumericMatrix CAL(nyears, nbins);

  NumericVector varAges = NumericVector::create(0.00000000, 0.08333333, 0.16666667, 
                                                0.25000000, 0.33333333, 0.41666667,
                                                0.50000000, 0.58333333, 0.66666667,
                                                0.75000000, 0.83333333, 0.91666667); // monthly ages
  
  for (int yr=0; yr < nyears; yr++) {
    NumericVector Nage = (VulnN.row(yr)); // numbers of catch-at-age this year
    double Ncatch = sum(Nage); // total catch this year
   
    if (Ncatch>0) {
      
      List Lens(k*12); // list for each age class in each month
      int count = 0;
      for (int age=0; age < k; age++) { // loop over annual ages
        
        // Calculate probability of sub-year age-classes
        int n_age = 12; 
        NumericMatrix ALK(n_age, nbins);
        NumericMatrix ALKs(n_age, nbins);
        NumericVector page(n_age);
        
        // calc select-at-subage
        for (int subage=0; subage<n_age; subage++) { // loop over 12 months
          double sage = varAges(subage) + age;
          double thislength = Linfs(yr) * (1-exp(-Ks(yr)* (sage - t0s(yr))));
          if (thislength<0.001) {
            thislength = 0.001;
          }
          ALK(subage, _) = Rcpp::dnorm(CAL_binsmid, thislength, thislength*LenCV, false);
          page(subage) = sum(ALK(subage,_));
        }
        
        LogicalVector  b1 = page>0;
        if (is_false(all(b1))) {
          ALK(0,0) = 1;
        }
    
        NumericVector sela(n_age);
        for (int subage=0; subage<n_age; subage++) {
          if (page(subage)>0) {
            ALKs(subage,_) = ALK(subage,_)/page(subage);
            }
          for (int len=0; len<nbins; len++) {
            sela(subage) += ALKs(subage,len)*selCurve(len,yr); // selectivity-at-age in each monthly time-step
          }
        }
        

        // distribute annual effective sample size proportional to relative monthly catch
        double relnumber = Nage(age)/sum(Nage);
        NumericVector probsubage = sela/sum(sela) * relnumber;
        NumericVector subAgeVec = CAL_ESS * probsubage;
        
        // NumericVector subAgeVec = Nage3/NumericVector::create(1,2,3,4,5,6,7,8,9,10,11,12); // distribute n across months
        for (int subage=0; subage<=11; subage++) { // loop over 12 months
          if (subAgeVec(subage) > 0) {
            double sage = varAges(subage) + age;
            double mean = Linfs(yr) * (1-exp(-Ks(yr)* (sage - t0s(yr)))); // calculate mean length at sub-age;
            if (mean < 0) mean = 0.01;
            NumericVector dist = tdnorm((CAL_binsmid-mean)/(LenCV*mean), -truncSD, truncSD); // prob density of lengths for this age
            NumericVector newdist = dist * selCurve(_,yr); // probability = dist * size-selection curve
            if (sum(newdist)!=0) {
              // newdist = newdist/sum(newdist);
              Lens(count) = RcppArmadillo::sample(CAL_binsmid, subAgeVec(subage), TRUE, newdist); // sample lengths for this sub-age class
            } else {
              Lens(count) = NA_INTEGER;
            }
          } else {
            Lens(count) = NA_INTEGER;
          }
          count += 1;
        }
      }
      // out(0) = Lens;
      NumericVector LenVals = combine(Lens); // unlist
      
      NumericVector templens = get_freq2(LenVals, CAL_bins, nbins); // calculate frequencies
      double rat = CAL_nsamp/sum(templens);
      templens =  templens * rat; // scale to CAL_nsamp
      CAL(yr,_) = templens;
    } else {
      NumericVector zeros(nbins);
      CAL(yr,_) = zeros;
    }
  }
  
  return(CAL);
}

// [[Rcpp::export]]
NumericMatrix  genSizeComp2(NumericMatrix VulnN, NumericVector CAL_binsmid, NumericVector CAL_bins, NumericMatrix selCurve,
                           double CAL_ESS, double CAL_nsamp,
                           NumericVector Linfs, NumericVector Ks, NumericVector t0s,
                           double LenCV, double truncSD) {
  int nyears = VulnN.nrow();
  int k = VulnN.ncol();
  int nbins = CAL_binsmid.size();
  NumericMatrix CAL(nyears, nbins);
  //double width = CAL_binsmid(1) - CAL_binsmid(0);
  //double origin = CAL_binsmid(0) - 0.5 * width;
  for (int yr=0; yr < nyears; yr++) {
    NumericVector Nage = (VulnN.row(yr)); // numbers of catch-at-age this year
    List Lens(Nage);
    double Ncatch = sum(Nage); // total catch this year
    if (Ncatch>0) {
      NumericVector Nage2 = (Nage/Ncatch) * CAL_ESS; // number-at-age effective sample
      for (int age=0; age < k; age++) { // loop over 1:maxage
        double mean = Linfs(yr) * (1-exp(-Ks(yr)* (age - t0s(yr)))); // calculate mean length at age;
        if (mean < 0) mean = 0.01;
        NumericVector dist = tdnorm((CAL_binsmid-mean)/(LenCV*mean), -truncSD, truncSD); // prob density of lengths for this age
        NumericVector newdist = dist * selCurve(_,yr); // probability = dist * size-selection curve
        if (sum(newdist)!=0) {
          newdist = newdist/sum(newdist);
          Lens(age) = RcppArmadillo::sample(CAL_binsmid, Nage2(age), TRUE, newdist); // sample lengths for this age class
        } else {
          Lens(age) = NA_INTEGER;
        }
      }

      NumericVector LenVals = combine(Lens); // unlist
      NumericVector templens = get_freq2(LenVals, CAL_bins, nbins); // calculate frequencies
      double rat = CAL_nsamp/sum(templens);
      templens =  templens * rat; // scale to CAL_nsamp
      CAL(yr,_) = templens;
    } else {
      NumericVector zeros(nbins);
      CAL(yr,_) = zeros;
    }
  }
  return(CAL);
}


