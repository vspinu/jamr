#ifndef __JAM_COMMON_HPP__
#define __JAM_COMMON_HPP__

#include "jam_types.hpp"
#include "r_types.hpp"
#include <cstring>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/archives/binary.hpp>
#include "Rcpp.h"
using namespace Rcpp;

// #define DEBUG

#ifdef DEBUG
#define PRINT(...) printf(__VA_ARGS__)
#else
#define PRINT(...)
#endif


// UTILS

inline void stop_on_invalid_type(SEXP x, const JamElType::type& jtype) {
  stop("Cannot serialize object of R type %s into JamElType %s",
       Rf_type2char(TYPEOF(x)), JamElType::toString(jtype));
}

inline int meta_length(const SEXP x) {
  SEXP attr = ATTRIB(x);
  int len = 0;
  while (attr != R_NilValue) {
    len++;
    attr = CDR(attr);
  }
  return len;
}

inline int common_el_type(SEXP x) {
  SEXPTYPE common_type = TYPEOF(VECTOR_ELT(x, 0));
  size_t N = XLENGTH(x);
  bool same = true;
  for (size_t i = 1; i < N; i++) {
    SEXPTYPE type = TYPEOF(VECTOR_ELT(x, i));
    if (type != common_type || type == VECSXP) {
      same = false;
      break;
    }
  }
  if (same) return (int) common_type;
  else return -1;
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

// JAM.CPP
void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head = true);
void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, JamType& head);
void jam_meta(cereal::BinaryOutputArchive& bout, SEXP x);

// UNJAM.CPP
SEXP unjam_sexp(cereal::BinaryInputArchive& bin);
SEXP unjam_sexp(cereal::BinaryInputArchive& bin, const JamType& head);
SEXP unjam_meta(cereal::BinaryInputArchive& bin);
  
#endif
