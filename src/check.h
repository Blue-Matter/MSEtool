#ifndef check_h
#define check_h

#include <Rcpp.h>
using namespace Rcpp;

inline void CheckClass(RObject object,
                Rcpp::CharacterVector reqClass,
                Rcpp::CharacterVector argName) {
  
  bool b = object.hasAttribute("class");
  int counter = 0;
  int nClass = reqClass.size();
  if (b) {
    CharacterVector actualClass = object.attr("class");
    for(int i = 0; i < nClass; i++) {
      if (actualClass[0] == reqClass[i])
        counter++;
    }
  }
  
  if (!b || counter ==0)
    stop("argument %s must be class %s", argName, reqClass);
  
}

inline void CheckDimensions(int req, 
                     int act,
                     Rcpp::CharacterVector object,
                     Rcpp::CharacterVector name,
                     int type =0) {
  if (act != req) {
    Rcpp::Rcout << "Required: " << req << std::endl;
    Rcpp::Rcout << "Provided: " << act << std::endl;
    if (type == 0)
      stop("%s must have %s rows", object, name);
    if (type == 1)
      stop("%s must have %s columns", object, name);
    if (type == 3)
      stop("%s must have %s slices (3rd dimension)", object, name);
  }
}

inline void CheckLength(int req, 
                 int act,
                 Rcpp::CharacterVector object,
                 Rcpp::CharacterVector name) {
  if (act != req) {
    Rcpp::Rcout << "Required: " << req << std::endl;
    Rcpp::Rcout << "Provided: " << act << std::endl;
    stop("%s must be length %s ", object, name);
  }
}

// TODO - doesn't handle MP Lists
inline int CheckMPClass(RObject MP) {
  int hasMP = 0;
  if(is<Function>(MP)){
    // check the class
    CharacterVector req = {"mp", "mmp", "MP", "MMP"};
    CheckClass(MP, req, CharacterVector("MP"));
    hasMP = 1;
  } else if(!MP.isNULL()){
    stop("Argument `MP` must be either NULL or a function class `mp`, `mmp`, `MP` or `MMP`");
  }
  return(hasMP);
}

inline void msg(int counter) {
  Rcpp::Rcout << "Here " << counter << std::endl;
}



#endif // check_h