#ifndef __JAM_RTYPES_HPP__
#define __JAM_RTYPES_HPP__

#include "jamtypes.hpp"
#include <cstdint>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

SEXPTYPE Jam2SexpType (JamElType::type jtype);

JamElType::type Sexp2JamElType (SEXPTYPE stype);

JamElType::type best_int_type(SEXP x);


// default implementation for numeric types
template <class inT>
SEXP toSEXP(const std::vector<inT>& vec, SEXPTYPE stype) {

  size_t n = vec.size();
  SEXP out = PROTECT(Rf_allocVector(stype, n));

  const inT* pv = vec.data();

  switch(stype) {
   case LGLSXP:
     std::copy(vec.begin(), vec.end(), LOGICAL(out)); 
     break;
   case INTSXP:
     std::copy(vec.begin(), vec.end(), INTEGER(out)); 
     break;
   case REALSXP:
     std::copy(vec.begin(), vec.end(), REAL(out)); 
     break;
   default:
     stop("Conversion to SEXP of this type is not implemented");
  }
  
  UNPROTECT(1);
  return out;
}

// Specialization for strings (WAF? Cannot be implemented here as that leads to
// multiple declarations during linking)
template <>
SEXP toSEXP(const std::vector<std::string>& vec, SEXPTYPE stype);

#endif
