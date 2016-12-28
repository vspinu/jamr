#include "rutils.hpp"


/* ------------------------------------------------------ */
/* Conversions from SEXP to CPP                           */
/* ------------------------------------------------------ */

#define DEFSEXP2CPP(NAME, TYPE, EXTRACTOR)          \
  static VarColl NAME(SEXP x){                      \
    SEXP rnames = GET_NAMES(x);                     \
    R_len_t N = LENGTH(x);                          \
    strmap<TYPE> m;                                 \
    for (int i = 0; i < N; i++) {                   \
      m[string(CHAR(STRING_ELT(rnames, i)))] =      \
        EXTRACTOR;                                  \
    }                                               \
    return VarColl(std::move(m));                   \
  }                                                 \

DEFSEXP2CPP(intvec2map, int, INTEGER(x)[i])
DEFSEXP2CPP(realvec2map, double, REAL(x)[i])
DEFSEXP2CPP(strvec2map, string, string(CHAR(STRING_ELT(x, i))))
DEFSEXP2CPP(intlist2map, int, INTEGER(VECTOR_ELT(x, i))[0])
DEFSEXP2CPP(reallist2map, double, REAL(VECTOR_ELT(x, i))[0])
DEFSEXP2CPP(strlist2map, string, string(CHAR(STRING_ELT(VECTOR_ELT(x, i), 0))))

static VarColl SEXP2VarColl(SEXP x) {
  SEXP rnames = Rf_getAttrib(x, R_NamesSymbol);
  if (rnames != R_NilValue) {
    switch (TYPEOF(x)) {
     case LGLSXP:
     case INTSXP:  return intvec2map(x);
     case REALSXP: return realvec2map(x);
     case STRSXP:  return strvec2map(x);
     case VECSXP: {
       int ct = common_el_type(x, true);
       if (LENGTH(x) > 0 && ct > 0) {
         switch (TYPEOF(VECTOR_ELT(x, 0))) {
          case LGLSXP:
          case INTSXP:  return intlist2map(x);
          case REALSXP: return reallist2map(x);
          case STRSXP:  return strlist2map(x);
         }
       }
     }
    } 
  } else {
    switch (TYPEOF(x)) {
     case LGLSXP:
     case INTSXP:  return as<int_vec>(x);
     case REALSXP: return as<dbl_vec>(x);
     case STRSXP:  return as<str_vec>(x);
    }
  }
}

static Head get_sexp_head (SEXP x) {
  Type coll_type = UNSUPORTED, el_type = UNSUPORTED;
  switch (TYPEOF(x)) {
   case NILSXP:  break;
   case LGLSXP:  coll_type = VECTOR, el_type = INT;    break;
   case INTSXP:  coll_type = VECTOR, el_type = INT;    break;
   case REALSXP: coll_type = VECTOR, el_type = DOUBLE; break;
   case STRSXP:  coll_type = VECTOR, el_type = STRING; break;
   case VECSXP:  {
     // only named lists are supported
     if (XLENGTH(x) > 0) {
       if (GET_NAMES(x) != R_NilValue) {
         int ct = common_el_type(x, true);
         if (ct >= 0) {
           Head out = get_sexp_head(VECTOR_ELT(x, 0));
           if (GET_NAMES(x) != R_NilValue) {
             out.coll_type = MAP;
           } else {
             out.coll_type = VECTOR;
           };
           return out;
         }
       }
     }
   }
  }
  bool has_meta = ATTRIB(x) != R_NilValue;
  return Head(coll_type, el_type, has_meta);
}

static strmap<VarColl> get_meta(SEXP x) {
  strmap<VarColl> out;
  SEXP attr = ATTRIB(x);
  
  while (attr != R_NilValue) {
    Head h = get_sexp_head(CAR(attr));
    if (h.coll_type != UNSUPORTED) {
      string nm(CHAR(PRINTNAME(TAG(attr))));
      PRINT("nm:%s\n", nm.c_str());
      out[nm] = SEXP2VarColl(CAR(attr));
    };
    attr = CDR(attr);
  }
  return out;
}



/* ------------------------------------------------------ */
/* SERIALIZATION                                          */
/* ------------------------------------------------------ */

template<class Tout> inline
void jar_int_primitive(cereal::BinaryOutputArchive& bout, const int& el, const Tout& na_val) {
  if (el == NA_INTEGER) bout(na_val);
  else bout(static_cast<Tout>(el));
}

// [[Rcpp::export]]
void c_jar(SEXP x, const std::string& path, bool append, int rows_per_chunk) {
 
  if (!(TYPEOF(x) == VECSXP && Rf_inherits(x, "data.frame"))) {

    stop("Jarring is for data.frames only.");

  } else {


    uint ncols = XLENGTH(x);
    strmap<VarColl> meta = get_meta(x);
    vector<strmap<VarColl>> col_metas(ncols);

    for (size_t c = 0; c < ncols; c++){
      col_metas[c] = get_meta(VECTOR_ELT(x, c));
    }

    PRINT("-- init writer --\n");
    Writer writer(path, meta, col_metas);

    PRINT("-- writing header --\n");
    writer.write_header();

    size_t nrows = XLENGTH(VECTOR_ELT(x, 0)); // no straightforward way to get nrows?

    vector<VarColl> cols;
    for (size_t c = 0; c < ncols; c++) {
      cols.push_back(SEXP2VarColl(VECTOR_ELT(x, c)));
    }
    PRINT("-- cols writing --\n");
    writer.write_columns(cols, rows_per_chunk);
  }
}
