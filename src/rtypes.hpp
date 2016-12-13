#ifndef __JAM_RTYPES_HPP__
#define __JAM_RTYPES_HPP__

#include "jamtypes.hpp"
#include <cstdint>
#include <string>
#include <fstream>
#include <iostream>
#include <Rcpp.h>
using namespace Rcpp;

enum RType {
  R_LGL,
  R_INT,
  R_DBL,
  R_CHR,
  R_RAW,
  R_FACTOR,
  R_DATE,
  R_DATETIME,
  R_TIME
};

RType Jam2RType (JamElType::type jtype) {
  switch(jtype) {
   case JamElType::BOOL:
     return R_LGL;
   case JamElType::BYTE:
   case JamElType::SHORT:
   case JamElType::INT:
   case JamElType::UBYTE:
   case JamElType::USHORT:
   case JamElType::UINT:
     return R_INT;
   case JamElType::LONG:
   case JamElType::ULONG:
   case JamElType::FLOAT:
   case JamElType::DOUBLE:
     return R_DBL;
     // case JamElType::UTF8:
     //   return R_CHR;
     // case JamElType::BINARY:
     //   return R_RAW;
  }
  throw std::runtime_error("Invalid JamElType");
}

JamElType::type Sexp2JamType (SEXPTYPE stype) {
  switch(stype) {
    // NILSXP	  /* nil = NULL */
   case LGLSXP:  return JamElType::UBYTE;
   case INTSXP:  return JamElType::INT;	 
   case REALSXP: return JamElType::DOUBLE;
   case STRSXP:  return JamElType::UTF8;
   case VECSXP:  return JamElType::UNDEFINED;
  }
  stop("Serialization of SEXPTYPE %d is not supported", stype);
}

SEXPTYPE RType2SexpType(RType rtype) {
  switch(rtype) {
   case R_LGL:      return LGLSXP;
   case R_INT:      return INTSXP;
   case R_DBL:      return REALSXP;
   case R_CHR:      return STRSXP;
   case R_RAW:      return VECSXP;
   case R_FACTOR:   return INTSXP;
   case R_DATE:     return INTSXP;
   case R_DATETIME: return REALSXP;
   case R_TIME:     return REALSXP;
  }
  throw std::runtime_error("Invalid RType");
}

template <class SrcType, class DestType>
void copyRecast(const SrcType* src, DestType* dest, size_t n) {
  auto recast = reinterpret_cast<const SrcType*>(src);
  std::copy(&recast[0], &recast[0] + n, dest);
}

// default implementation for numeric types
template <class T>
SEXP toSEXP(const std::vector<T>& vec, RType rtype) {

  size_t n = vec.size();
  SEXP out = PROTECT(Rf_allocVector(RType2SexpType(rtype), n));

  const T* pv = vec.data();

  switch(rtype) {
   case R_LGL:
     break;
   case R_INT:
     copyRecast<T, int>(pv, INTEGER(out), n);
     break;
   case R_DBL:
     copyRecast<T, double>(pv, REAL(out), n);
     break;
   default:
     stop("Not implemented");
  }
  
  UNPROTECT(1);
  return out;
}


// specialization for strings
template <>
SEXP toSEXP(const std::vector<std::string>& vec, RType rtype) {

  size_t n = vec.size();
  SEXP out = PROTECT(Rf_allocVector(RType2SexpType(rtype), n));

  if (rtype != R_CHR) stop("Jammer strings can be only converted to R character vector.");

  for (int i = 0; i < n; ++i) {
    std::string istr = vec[i];
    SEXP ostr = Rf_mkCharLenCE(istr.c_str(), istr.size(), CE_UTF8);
    SET_STRING_ELT(out, i, ostr);
  }
  
  UNPROTECT(1);
  return out;
}

#endif
