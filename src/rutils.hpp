#ifndef __JAM_RTYPES_HPP__
#define __JAM_RTYPES_HPP__

#include <cstdint>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>

#include <Rcpp.h>
using namespace Rcpp;

#include "jam.hpp"
using namespace jam;

#define GET_NAMES(x) Rf_getAttrib(x, R_NamesSymbol)
#define GET_DIM(x)   Rf_getAttrib(x, R_DimSymbol)

SEXPTYPE Jam2SexpType (jam::Type jtype);

jam::Type Sexp2JamElType (SEXPTYPE stype);

jam::Type best_int_type(SEXP x);

Head get_head(SEXP x);

// Didn't find in R, so roll my own.
SEXP get_list_elt(SEXP x, const char* name);

inline int common_el_type(SEXP x, bool size1 = false) {
  SEXPTYPE common_type = TYPEOF(VECTOR_ELT(x, 0));
  size_t N = XLENGTH(x);
  bool same = true;
  for (size_t i = 1; i < N; i++) {
    SEXP el = VECTOR_ELT(x, i);
    SEXPTYPE type = TYPEOF(el);
    if (type != common_type || type == VECSXP || (size1 && (XLENGTH(el) > 1))) {
      same = false;
      break;
    }
  }
  if (same) return (int) common_type;
  else return -1;
}


inline void stop_on_invalid_type(SEXP x, const Type& jtype) {
  stop("Cannot serialize object of R type %s into Jar Type %s",
       Rf_type2char(TYPEOF(x)), Type2String(jtype));
}

inline void jam_names(cereal::BinaryOutputArchive& bout, SEXP x) {
  std::vector<std::string> names = as<std::vector<std::string>>(GET_NAMES(x));
  bout(names);
}

// meta is a VECSEXP
inline SEXP set_meta(SEXP obj, SEXP meta) {
  if (meta != R_NilValue) {
    SEXP meta_names = Rf_getAttrib(meta, R_NamesSymbol);
    int metaN = LENGTH(meta_names);
    for (int i = 0; i < metaN; i++) {
      SEXP nm = STRING_ELT(meta_names, i);
      Rf_setAttrib(obj, Rf_installChar(nm), VECTOR_ELT(meta, i));
    }
  }
  return obj;
}

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


// Specialization for strings (WAF? Even inline specialization cannot be
// implemented here as that leads to multiple declarations during linking)
template <>
SEXP toSEXP(const std::vector<std::string>& vec, SEXPTYPE stype);


#endif
