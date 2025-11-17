#include <Rcpp.h>
using namespace Rcpp;

int CalcRecruitment_TimeStep_(S4 Ages, double TSperYear) {
  NumericVector AgeClasses = Ages.slot("Classes");
  double MinAge = min(AgeClasses);
  double temp = 0;
  int RecruitTS = 0;
  for (int i=0; i<AgeClasses.size(); i++) {
    if (temp<=MinAge) {
      temp +=TSperYear;
      RecruitTS = i+1;
    }
  }
  return(RecruitTS);
}