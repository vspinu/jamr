#include "common.hpp"

template<class Tout> inline
void jar_int_primitive(cereal::BinaryOutputArchive& bout, const int& el, const Tout& na_val) {
  if (el == NA_INTEGER) bout(na_val);
  else bout(static_cast<Tout>(el));
}


// [[Rcpp::export]]
void c_jar(SEXP x, const std::string path, const bool& append) {

  size_t orig_nrows = 0, orig_ncols = 0;

  if (append) {
    // read current nrows first
    std::ifstream fin(path, std::ios::binary);
    cereal::BinaryInputArchive bin(fin);
    JamType head;
    bin(head);
    if (head.coll_type != JamCollType::HDF)
      stop("Output file contain an object of type '%s', not HDF.", JamCollType::toString(head.coll_type));
    bin(orig_nrows);
    bin(orig_ncols);
    fin.close();
  }

  auto bmode = append ? (std::ios::binary | std::ios::out | std::ios::in) : (std::ios::binary | std::ios::out);
  std::fstream fout(path, bmode);
  // std::ofstream fout(path, append ? (std::ios::binary) : std::ios::binary);
  cereal::BinaryOutputArchive bout(fout);
 
  if (!(TYPEOF(x) == VECSXP && Rf_inherits(x, "data.frame"))) {

    stop("Jarring is for data.frames only.");

  } else {

    size_t ncols = XLENGTH(x);
    if (ncols == 0) stop("cannot serialize empty data.frame"); // fixme:
    size_t nrows = XLENGTH(VECTOR_ELT(x, 0)); // no straightforward way to get nrows?

    // HEAD[META]
    JamType head = get_head(x);
    head.coll_type = JamCollType::HDF;

    // COLLHEADS
    std::vector<JamType> heads(ncols);
    for (size_t c = 0; c < ncols; c++) {
      JamType col_head = get_head(VECTOR_ELT(x, c));
      if (col_head.coll_type != JamCollType::VECTOR)
        stop("Only atomic columns can be serialized. Not true for column %d", c + 1);
      // fixme: hack till customizable output types are inplace
      if (col_head.el_type == JamElType::UTF8)
        col_head.el_type = JamElType::STRING;
      heads[c] = col_head;
    }

    if (append) {

      if (orig_ncols != ncols)
        stop("Number of columns of the input and archived object differ (%ld vs %ld).", ncols, orig_ncols);

      // fixme: drop DIM metadata and implement reading with re-sizable vectors?
      fout.seekp(4, fout.beg);
      bout(static_cast<ulong>(nrows + orig_nrows));
      fout.seekp(0, fout.end);

    } else {

      // HEAD
      bout(head);

      // DIM
      bout(static_cast<ulong>(nrows));
      bout(static_cast<ulong>(ncols));

      // [META]
      if (head.hasMeta()) jam_meta(bout, x);

      bout(heads);

      // [COLLMETA]
      for (size_t c = 0; c < ncols; c++) {
        if (heads[c].hasMeta()) {
          jam_meta(bout, VECTOR_ELT(x, c));
        }
      }
    }

    // ROWS 
    for (size_t r = 0; r < nrows; r++) {
      for (size_t c = 0; c < ncols; c++) {
        SEXP col = VECTOR_ELT(x, c);
        switch (TYPEOF(col)) {
    
         case LGLSXP:
           switch (heads[c].el_type) {
            case JamElType::BOOL:
            case JamElType::BYTE:
              jar_int_primitive(bout, INTEGER(col)[r], NA_BYTE); break;
            case JamElType::UBYTE:
              jar_int_primitive(bout, INTEGER(col)[r], NA_UBYTE); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           };
           break;
     
         case INTSXP:

           switch (heads[c].el_type) {
            case JamElType::BYTE:
              jar_int_primitive(bout, INTEGER(col)[r], NA_BYTE); break;
            case JamElType::UBYTE:
              jar_int_primitive(bout, INTEGER(col)[r], NA_UBYTE); break;
            case JamElType::SHORT:
              jar_int_primitive(bout, INTEGER(col)[r], NA_SHORT); break;
            case JamElType::USHORT:
              jar_int_primitive(bout, INTEGER(col)[r], NA_USHORT); break;
            case JamElType::INT:
              bout(INTEGER(col)[r]); break;
            case JamElType::UINT:
              jar_int_primitive(bout, INTEGER(col)[r], NA_UINT); break;
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
            case JamElType::UTF8:
            case JamElType::STRING:
              bout(std::string(CHAR(STRING_ELT(col, r)))); break;
            default:
              stop_on_invalid_type(col, heads[c].el_type);
           }
           break;

         default:
           stop("Cannot serialize column object of type %s", Rf_type2char(TYPEOF(col)));
        }
      }
    }
  }
}
