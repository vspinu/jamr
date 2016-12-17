#include "common.hpp"

template<class Tout> inline
void ham_int_primitive(cereal::BinaryOutputArchive& bout, const int& el, const Tout& na_val) {
  if (el == NA_INTEGER) bout(na_val);
  else bout(static_cast<Tout>(el));
}


void ham_sexp(cereal::BinaryOutputArchive& bout, SEXP x) {
 
  if (!(TYPEOF(x) == VECSXP && Rf_inherits(x, "data.frame"))) {
    stop("Hamming is for data.frames only.");
  } else {


    size_t ncols = XLENGTH(x);
    if (ncols == 0) stop("cannot serialize empty data.frame"); // fixme:
    size_t nrows = XLENGTH(VECTOR_ELT(x, 0)); // no straightforward way to get nrows?

    std::vector<JamType> heads(ncols);

    for (size_t c = 0; c < ncols; c++) {
      JamType col_head = get_head(VECTOR_ELT(x, c));
      if (col_head.coll_type != JamCollType::VECTOR)
        stop("Only atomic columns can be serialized. Not true for column %d", c + 1);
      heads[c] = col_head;
    }

    JamType head = get_head(x);
    head.coll_type = JamCollType::HDF;
    head.print("head:");
    bout(head);

    SEXP dim = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nrows;
    INTEGER(dim)[1] = ncols;
    jam_meta(bout, x, dim);
    UNPROTECT(1);

    bout(heads);

    // hamming rows 
    for (size_t r = 0; r < nrows; r++) {
      for (size_t c = 0; c < ncols; c++) {
        SEXP col = VECTOR_ELT(x, c);
        switch (TYPEOF(col)) {
    
         case LGLSXP:
           switch (heads[c].el_type) {
            case JamElType::BOOL:
            case JamElType::BYTE:
              ham_int_primitive(bout, INTEGER(col)[r], NA_BYTE); break;
            case JamElType::UBYTE:
              ham_int_primitive(bout, INTEGER(col)[r], NA_UBYTE); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           };
           break;
     
         case INTSXP:

           switch (heads[c].el_type) {
            case JamElType::BYTE:
              ham_int_primitive(bout, INTEGER(col)[r], NA_BYTE); break;
            case JamElType::UBYTE:
              ham_int_primitive(bout, INTEGER(col)[r], NA_UBYTE); break;
            case JamElType::SHORT:
              ham_int_primitive(bout, INTEGER(col)[r], NA_SHORT); break;
            case JamElType::USHORT:
              ham_int_primitive(bout, INTEGER(col)[r], NA_USHORT); break;
            case JamElType::INT:
              bout(INTEGER(col)[r]); break;
            case JamElType::UINT:
              ham_int_primitive(bout, INTEGER(col)[r], NA_UINT); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           };
           break;
     
         case REALSXP:
           switch(heads[c].el_type) {
            case JamElType::FLOAT:
              bout(static_cast<float>(REAL(col)[r])); break;
            case JamElType::DOUBLE:
              bout(REAL(col)[r]); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           }
           break;
     
         case STRSXP:
           switch(heads[c].el_type) {
            case JamElType::STRING:
              bout(std::string(CHAR(STRING_ELT(col, r)))); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           }
           break;

         default:
           stop("Cannot column object of type %s", Rf_type2char(TYPEOF(col)));
        }
      }
    }
  }
}

// [[Rcpp::export]]
void c_ham(SEXP x, const std::string path) {
  std::ofstream fout(path, std::ios::binary);
  cereal::BinaryOutputArchive bout(fout);
  ham_sexp(bout, x);
}
