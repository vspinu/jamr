#include "common.hpp"
#include <cstring>

template<class Tin> inline
int unham_int_primitive(cereal::BinaryInputArchive& bin, const Tin& na_val) {
  Tin el;
  bin(el);
  if (el == na_val) return NA_INTEGER;
  else return static_cast<int>(el);
}

SEXP unham_sexp(cereal::BinaryInputArchive& bin, JamType& head) {
 
  if (head.coll_type != JamCollType::HDF) {
    stop("Can unham only objects of HDF. Got %s", JamCollType::toString(head.coll_type));
  } else {

    if (!head.hasMeta()) stop("Invalid serialized HDF object. Must have meta.");

    SEXP meta = PROTECT(unjam_sexp(bin, JAM_META_HEAD));

    std::vector<JamType> heads;
    bin(heads);

    std::vector<JamElType::type> types(heads.size());

    for (size_t i = 0; i < heads.size(); i++) {
      if (heads[i].coll_type != JamCollType::VECTOR)
        stop("Only primitive columns currently supported. Not true for column %ld (%s).",
             i, JamCollType::toString(heads[i].coll_type));
      types[i] = heads[i].el_type;
    }
      
    SEXP dim = get_list_elt(meta, "dim");

    if (dim == R_NilValue) stop("Stored HDF doesn't have a dim attribute.");

    size_t nrows = INTEGER(dim)[0];
    size_t ncols = INTEGER(dim)[1];

    if (ncols != types.size())
      stop("Dim ncols metadata (%d) and size of column types vector (%d) don't match.", ncols, types.size());

    SEXP out = PROTECT(Rf_allocVector(VECSXP, ncols));
    for (size_t c = 0; c < ncols; c++){
      // do I need to protect elements of protected list?
      SET_VECTOR_ELT(out, c, PROTECT(Rf_allocVector(Jam2SexpType(types[c]), nrows)));
    }
    for (size_t r = 0; r < nrows; r++) {
      for (size_t c = 0; c < ncols; c++) {
        switch (types[c]) {
         case JamElType::BOOL:  
         case JamElType::BYTE:   INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_BYTE);  break;
         case JamElType::UBYTE:  INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_UBYTE); break;
         case JamElType::SHORT:  INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_SHORT); break;
         case JamElType::USHORT: INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_USHORT);break;
         case JamElType::INT:    INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_INT);   break;
         case JamElType::UINT:   INTEGER(VECTOR_ELT(out, c))[r] = unham_int_primitive(bin, NA_UINT);   break;

         case JamElType::FLOAT:
           {
             float el; bin(el);  REAL(VECTOR_ELT(out, c))[r] = static_cast<double>(el);
           }
           break;
         case JamElType::DOUBLE:
           {
             double el; bin(el); REAL(VECTOR_ELT(out, c))[r] = el;
           }
           break;
         case JamElType::STRING:
           {
             std::string el; bin(el); SET_STRING_ELT(VECTOR_ELT(out, c), r, Rf_mkChar(el.c_str()));
           }
           break;
         default:
           stop("Unsupported JamElType (%s) in column %ld type spec.", JamElType::toString(types[c]), c);
        }
      }
    }

    SEXP meta_names = Rf_getAttrib(meta, R_NamesSymbol);
    int metaN = LENGTH(meta_names);
    for (int i = 0; i < metaN; i++) {
      SEXP nm = STRING_ELT(meta_names, i);
      if (std::strcmp(CHAR(nm), "dim") != 0) {
        Rf_setAttrib(out, Rf_installChar(nm), VECTOR_ELT(meta, i));
      }
    }

    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = -nrows;
    Rf_setAttrib(out, R_RowNamesSymbol, rn);

    UNPROTECT(ncols + 3);
    return out;
  }
}

// [[Rcpp::export]]
SEXP c_unham(const std::string& path) {
  std::ifstream fin(path, std::ios::binary);
  cereal::BinaryInputArchive bin(fin);
  JamType head;
  bin(head);
  return unham_sexp(bin, head);
}

