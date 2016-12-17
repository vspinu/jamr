
#include "r_types.hpp"
#include "common.hpp"
#include <cstring>

SEXPTYPE Jam2SexpType (JamElType::type jtype) {
  switch(jtype) {
   case JamElType::BOOL:
     return LGLSXP;
   case JamElType::BYTE:
   case JamElType::SHORT:
   case JamElType::INT:
   case JamElType::UBYTE:
   case JamElType::USHORT:
   case JamElType::UINT:
     return INTSXP;
   case JamElType::LONG:
   case JamElType::ULONG:
   case JamElType::FLOAT:
   case JamElType::DOUBLE:
     return REALSXP;
     // case JamElType::UTF8:
     //   return R_CHR;
     // case JamElType::BINARY:
     //   return R_RAW;
  }
  throw std::runtime_error("Cannot convert JamElType " + std::to_string((int) jtype) + " to SEXPTYPE.");
}

JamElType::type Sexp2JamElType (SEXPTYPE stype) {
  switch(stype) {
   case NILSXP:	 return JamElType::NIL;
   case LGLSXP:  return JamElType::BOOL;
   case INTSXP:  return JamElType::INT;
   case REALSXP: return JamElType::DOUBLE;
   case STRSXP:  return JamElType::UTF8;
   case VECSXP:  return JamElType::UNDEFINED;
   default:      return JamElType::UNSUPORTED;
  }
}

JamElType::type best_int_type(SEXP x){
  if (TYPEOF(x) != INTSXP) stop("x must be of INTSXP type");
  int M = std::numeric_limits<int>::min();
  int m = std::numeric_limits<int>::max();
  if (Rf_inherits(x, "factor")) {
    M = Rf_nlevels(x);
    m = 0;
  } else {
    int* pt = INTEGER(x);
    size_t N = XLENGTH(x);
    for (int i = 0; i < N; i++) {
      int v = pt[i];
      if (v != NA_INTEGER){
        M = std::max(M, pt[i]);
        m = std::min(m, pt[i]);
      }
    }
  }
  if (M >= MAX_USHORT || m <= MIN_SHORT) return JamElType::INT;
  if (M >= MAX_UBYTE && m >= 0) return JamElType::USHORT;
  if (M >= MAX_SHORT) return JamElType::INT; // m <= MIN_SHORT
  if (M >= MAX_UBYTE || m <= MIN_BYTE) return JamElType::SHORT;
  if (M >= MAX_BYTE && m >= 0) return JamElType::UBYTE;
  if (M >= MAX_BYTE) return JamElType::SHORT;
  return JamElType::BYTE;
}

JamType get_head(SEXP x) {
  bool has_names = GET_NAMES(x) != R_NilValue;
  bool has_meta = meta_length(x) > 0;

  switch (TYPEOF(x)) {
   case NILSXP:
     return JAM_NIL_HEAD;
   case VECSXP:
     if (XLENGTH(x) == 0) {
       return JamType(JamCollType::LIST, JamElType::UNDEFINED);
     } else {
       if (common_el_type(x) >= 0) {
         return JamType(JamCollType::LIST, JamElType::VECTOR, has_names, has_meta);
       } else {
         return JamType(JamCollType::LIST, JamElType::MIXED, has_names, has_meta);
       }
     }
   default:
     JamElType::type el_type = (XLENGTH(x) == 0) ? JamElType::UNDEFINED : Sexp2JamElType(TYPEOF(x));
     return JamType(JamCollType::VECTOR, el_type, has_names, has_meta);     
  }
}

SEXP get_list_elt(SEXP x, const char* name) {
  SEXP names = GET_NAMES(x);
  if (names == R_NilValue) return R_NilValue;
  size_t i;
  bool found = false;
  for (i = 0; i < XLENGTH(names); i++) {
    if (std::strcmp(CHAR(STRING_ELT(names, i)), name) == 0){
      found = true;
      break;
    }
  }
  if (found) return VECTOR_ELT(x, i);
  else return R_NilValue;
}

// specialization for strings
template <>
SEXP toSEXP(const std::vector<std::string>& vec, SEXPTYPE stype) {

  size_t n = vec.size();
  SEXP out = PROTECT(Rf_allocVector(stype, n));

  if (stype != STRSXP) stop("Jammer strings can be only converted to R character vector.");

  for (int i = 0; i < n; ++i) {
    std::string istr = vec[i];
    SEXP ostr = Rf_mkCharLenCE(istr.c_str(), istr.size(), CE_UTF8);
    SET_STRING_ELT(out, i, ostr);
  }
  
  UNPROTECT(1);
  return out;
}
