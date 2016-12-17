#ifndef __JAM_COMMON_HPP__
#define __JAM_COMMON_HPP__

#include "jam_types.hpp"
#include "r_types.hpp"
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/archives/binary.hpp>
#include "Rcpp.h"

using namespace Rcpp;


// UTILS

inline void stop_on_invalid_type(SEXP x, const JamElType::type& jtype) {
  stop("Cannot serialize object of R type %s into JamElType %s",
       Rf_type2char(TYPEOF(x)), JamElType::toString(jtype));
}

inline int meta_length(const SEXP x) {
  SEXP attr = ATTRIB(x);
  int len = 0;
  while (attr != R_NilValue) {
    if (TAG(attr) != R_NamesSymbol) len++;
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

// JAM.CPP
void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head = true);
void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, JamType& head);
void jam_meta(cereal::BinaryOutputArchive& bout, SEXP x, SEXP dim = R_NilValue);

// UNJAM.CPP
SEXP unjam_sexp(cereal::BinaryInputArchive& bin);
SEXP unjam_sexp(cereal::BinaryInputArchive& bin, const JamType& head);

#endif
