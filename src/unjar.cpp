#include "common.hpp"
#include <cstring>

template<class Tin> inline
int unjar_int_primitive(cereal::BinaryInputArchive& bin, const Tin& na_val) {
  Tin el;
  bin(el);
  if (el == na_val) return NA_INTEGER;
  else return static_cast<int>(el);
}

SEXP c_unjar(const std::string& path) {
  
  std::ifstream fin(path, std::ios::binary);
  cereal::BinaryInputArchive bin(fin);

  // std::iostream::pos_type fbeg = fin.tellg();
  // fin.seekg(0, fin.end);
  // std::iostream::pos_type fend = fin.tellg();
  // fin.seekg(fbeg);

  JamType head;
  bin(head);
 
  if (head.coll_type != JamCollType::HDF) {

    stop("Can unjar only objects of HDF. Got %s", JamCollType::toString(head.coll_type));

  } else {

    uint nprot = 0;

    // DIM
    ulong nrows, ncols;
    bin(nrows, ncols);
    
    // [META]
    SEXP meta = R_NilValue;
    if (head.hasMeta()) {
      meta = PROTECT(unjam_meta(bin));
      nprot++;
    }

    // COLLHEADS
    std::vector<JamType> heads;
    bin(heads);

    // [COLLMETA]
    SEXP collmetas = PROTECT(Rf_allocVector(VECSXP, heads.size()));
    nprot++;
    for (size_t i = 0; i < heads.size(); i++) {
      if (heads[i].hasMeta()) {
          SET_VECTOR_ELT(collmetas, i, PROTECT(unjam_meta(bin)));
          nprot++;
      } else {
          SET_VECTOR_ELT(collmetas, i, R_NilValue);
      }
    }

    std::vector<JamElType::type> types(heads.size());
    for (size_t i = 0; i < heads.size(); i++) {
      if (heads[i].coll_type != JamCollType::VECTOR)
        stop("Only primitive columns currently supported. Not true for column %ld (%s).",
             i, JamCollType::toString(heads[i].coll_type));
      types[i] = heads[i].el_type;
    }

    if (ncols != types.size())
      stop("Dim ncols metadata (%d) and size of column types vector (%d) don't match.", ncols, types.size());

    SEXP out = PROTECT(Rf_allocVector(VECSXP, ncols));
    nprot++;
    for (size_t c = 0; c < ncols; c++) {
      // do we really need to protect elements of protected list?
      SET_VECTOR_ELT(out, c, PROTECT(Rf_allocVector(Jam2SexpType(types[c]), nrows)));
      nprot++;
    }

    // size_t r = 0;
    // while (fin.tellg() < fend) {
    for (size_t r = 0; r < nrows; r++) {
      for (size_t c = 0; c < ncols; c++) {
        switch (types[c]) {
         case JamElType::BOOL:  
         case JamElType::BYTE:   INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_BYTE);  break;
         case JamElType::UBYTE:  INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_UBYTE); break;
         case JamElType::SHORT:  INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_SHORT); break;
         case JamElType::USHORT: INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_USHORT);break;
         case JamElType::INT:    INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_INT);   break;
         case JamElType::UINT:   INTEGER(VECTOR_ELT(out, c))[r] = unjar_int_primitive(bin, NA_UINT);  break;

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
      // r++;
    }

    set_meta(out, meta);

    for (size_t c = 0; c < ncols; c++) {
      if (VECTOR_ELT(collmetas, c) != R_NilValue) {
        set_meta(VECTOR_ELT(out, c), VECTOR_ELT(collmetas, c));
      }
    }

    // we need to overwrite rownames metadata if rjar was written through append
    SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
    nprot++;
    INTEGER(rn)[0] = NA_INTEGER;
    INTEGER(rn)[1] = - static_cast<int>(nrows);
    Rf_setAttrib(out, R_RowNamesSymbol, rn);

    UNPROTECT(nprot);
    return out;
  }
}

