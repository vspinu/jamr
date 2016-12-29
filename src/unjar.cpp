#include "rutils.hpp"
#include <cstring>

#include "jam.hpp"
using namespace jam;

#include "Rcpp.h"
using namespace Rcpp;


template<class Tin> inline
int unjar_int_primitive(cereal::BinaryInputArchive& bin, const Tin& na_val) {
  PRINT("unjaring int primitive\n");
  Tin el;
  bin(el);
  if (el == na_val) return NA_INTEGER;
  else return static_cast<int>(el);
}

SEXP VarColl2SEXP (const VarColl& vc) {
  PRINT("ct:%s  et:%s\n", Type2String(vc.coll_type).c_str(), Type2String(vc.el_type).c_str());
  switch(vc.coll_type) {
   case VECTOR:
     switch (vc.el_type) {
      case INT:    return wrap(vc.int_vec_val);
      case DOUBLE: return wrap(vc.dbl_vec_val);
      case STRING: return wrap(vc.str_vec_val);
      default:
        stop("Unsuported el type %s in VECTOR.", Type2String(vc.el_type));
     }
     break;
   case MAP:
     switch(vc.el_type) {
      case INT:    return wrap(vc.int_map_val);
      case DOUBLE: return wrap(vc.dbl_map_val);
      case STRING: return wrap(vc.str_map_val);
      default:
        stop("Unsuported el type %s in MAP.", Type2String(vc.el_type));
     }
     break;
   case NIL: return R_NilValue;
   default:
     stop("Unsuported coll type %s", Type2String(vc.coll_type));
  }
}

SEXP unjar_sexp(Reader& reader, int chunks) {

  PRINT("-- fetch columns --\n");
  vector<VarColl>& cols = reader.read_columns(chunks);
  PRINT("-- done --\n");

  if (cols.size() == 0)
    return R_NilValue;

  size_t ncols = reader.ncols();
  size_t nrows = reader.nrows();

  // BUILD OUTPUT LIST
  List out(ncols);

  for (size_t c = 0; c < ncols; c++) {
    PRINT("assigning column %ld\n", c);
    SEXP col = PROTECT(VarColl2SEXP(cols[c]));
    strmap<VarColl> attr = reader.col_metas[c];
    if (attr.size() > 0) {
      PRINT("setting attributes\n");
      for (const auto& kv : attr) {
        SEXP nm = Rf_installChar(Rf_mkChar(kv.first.c_str()));
        Rf_setAttrib(col, nm, VarColl2SEXP(kv.second));
      }
    }
    out[c] = col;
    UNPROTECT(1);
  }

  // SET ATTRIBUTES
  for (const auto& kv : reader.meta) {
    if (kv.first != "row.names")
      out.attr(kv.first) = VarColl2SEXP(kv.second);
  }
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrows);
  
  return out;
}

// [[Rcpp::export]]
SEXP c_unjar_bind(const std::string& path, int chunks) {
  Reader reader(path);
  return unjar_sexp(reader, chunks);
}

// [[Rcpp::export]]
SEXP c_unjar_nobind(const std::string& path, int chunks) {
  Reader reader(path);

  if (chunks <= 0)
    chunks = MAX_INT;
      
  List out;

  SEXP df = unjar_sexp(reader, 1);
  size_t nchunk = 1;
  while (df != R_NilValue && nchunk < chunks) {
    out.push_back(df);
    df = unjar_sexp(reader, 1);
    nchunk++;
  }

  return out;  
}
