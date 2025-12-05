#include <Rcpp.h>
using namespace Rcpp;

int CalcRecruitment_TimeStep_(S4 Ages, double Seasons) {
  NumericVector AgeClasses = Ages.slot("Classes");
  double MinAge = min(AgeClasses);
  double j = 0;
  int k = 0;
  for (int i=0; i<AgeClasses.size(); i++) {
    if (j<MinAge) {
      j +=Seasons;
      k +=1;
    }
  }
  return(k);
}