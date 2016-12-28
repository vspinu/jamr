#include "rutils.hpp"
#include <cstring>

SEXPTYPE Jam2SexpType (Type jtype) {
  switch(jtype) {
   case BOOL:
     return LGLSXP;
   case BYTE:
   case SHORT:
   case INT:
   case UBYTE:
   case USHORT:
   case UINT:
     return INTSXP;
   case LONG:
   case ULONG:
   case FLOAT:
   case DOUBLE:
     return REALSXP;
   case UTF8:
   case STRING:
     return STRSXP;
     // case BINARY:
     //   return R_RAW;
  }
  throw std::runtime_error("Cannot convert jam " + Type2String(jtype) + " to SEXPTYPE.");
}

Type Sexp2JamElType (SEXPTYPE stype) {
  switch(stype) {
   case NILSXP:	 return NIL;
   case LGLSXP:  return BOOL;
   case INTSXP:  return INT;
   case REALSXP: return DOUBLE;
   case STRSXP:  return UTF8;
   case VECSXP:  return UNDEFINED;
   default:      return UNSUPORTED;
  }
}

Type best_int_type(SEXP x){
  if (TYPEOF(x) != INTSXP) stop("x must be of INTSXP type");
  int M = std::numeric_limits<int>::min();
  int m = std::numeric_limits<int>::max();
  if (Rf_inherits(x, "factor")) {
    M = Rf_nlevels(x);
    m = 0;
  } else {
    int* pt = INTEGER(x);
    R_len_t N = XLENGTH(x);
    for (R_len_t i = 0; i < N; i++) {
      int v = pt[i];
      if (v != NA_INTEGER){
        M = std::max(M, v);
        m = std::min(m, v);
      }
    }
  }
  if (M >= MAX_USHORT || m <= MIN_SHORT) return INT;
  if (M >= MAX_UBYTE && m >= 0) return USHORT;
  if (M >= MAX_SHORT) return INT; // m <= MIN_SHORT
  if (M >= MAX_UBYTE || m <= MIN_BYTE) return SHORT;
  if (M >= MAX_BYTE && m >= 0) return UBYTE;
  if (M >= MAX_BYTE) return SHORT;
  return BYTE;
}


inline int attr_length(const SEXP x) {
  SEXP attr = ATTRIB(x);
  int len = 0;
  while (attr != R_NilValue) {
    len++;
    attr = CDR(attr);
  }
  return len;
}

Head get_head(SEXP x) {

  bool has_meta = ATTRIB(x) != R_NilValue;

  switch (TYPEOF(x)) {
   case NILSXP:
     return JAM_NIL_HEAD;
   case VECSXP:
     if (XLENGTH(x) == 0) {
       return Head(LIST, UNDEFINED);
     } else {
       if (common_el_type(x) >= 0) {
         return Head(LIST, VECTOR, has_meta);
       } else {
         return Head(LIST, MIXED, has_meta);
       }
     }
   default:
     Type el_type = Sexp2JamElType(TYPEOF(x));
     return Head(VECTOR, el_type, has_meta);     
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
