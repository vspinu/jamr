// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(Rcereal)]]

#include "jamtypes.hpp"
#include "rtypes.hpp"

template <class BinType>
SEXP unjam_vector0(cereal::BinaryInputArchive& bin, RType out_rtype){
  std::vector<BinType> vec;
  bin(vec);
  return toSEXP<BinType>(vec, out_rtype);
}

SEXP unjam_vector(cereal::BinaryInputArchive&bin, const JamType& jtype) {
  switch (jtype.element_type) {
    // case JamType::BOOL: {
    //   std::vector<bool> vec;
    //   bin(vec);
    //   return toSEXP<bool>(vec, R_INT);
    // }
   case JamElType::BYTE:       return unjam_vector0<byte>(bin, R_INT);
   case JamElType::UBYTE:      return unjam_vector0<ubyte>(bin, R_INT);
   case JamElType::SHORT:      return unjam_vector0<short>(bin, R_INT);
   case JamElType::USHORT:     return unjam_vector0<ushort>(bin, R_INT);
   case JamElType::INT:        return unjam_vector0<int>(bin, R_INT);
   case JamElType::UINT:       return unjam_vector0<uint>(bin, R_INT);
     // case JamElType::LONG:  return "LONG";
     // case JamElType::ULONG: return "ULONG";
   case JamElType::FLOAT:      return unjam_vector0<float>(bin, R_DBL);
   case JamElType::DOUBLE:     return unjam_vector0<double>(bin, R_DBL);
   case JamElType::UTF8:       return unjam_vector0<std::string>(bin, R_CHR);
     // case JamElType::BITSET: return "BITSET";
   default:
     // stop("Invalid JamElType in file header: %s",
     //      jamTypeToString(head.element_type));
     stop("Invalid JamElType in file header.");
  }  
}

SEXP unjam_list(cereal::BinaryInputArchive& bin, const JamType& jtype) {
  uint N;
  bin(N);
  
  if (((uint) jtype.element_type) > 100) {
    bool is_mixed = jtype.element_type == JamElType::MIXED) {

    JamType ijtype;
    std::vector<JamType> ijtypes;
    vector<std::string> names;
    SEXP meta = R_NilValue;
    
    if (is_mixed) bin(ijtypes) else bin(ijtype);
    if (jtype.hasNames()) bin(names);
    if (jtype.hasMeta()) meta = unjam_list(bin, JAM_META_HEAD);

    PROTECT(meta);
      
    SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
      
    switch (ijtype.collection_type) {
     case JamType::VECTOR:
       for (int i = 0; i < N; ++i)
         SET_VECTOR_ELT(out, i, unjam_vector(bin, is_mixed ? ijtypes[i] : ijtype));
       break;
     case: JamType::LIST:
       for (int i = 0; i < N; ++i)
         SET_VECTOR_ELT(out, i, unjam_list(bin, is_mixed ? ijtypes[i] : ijtype));
       break;
     default:
       stop("'%s' is not supported as LIST elements.",
            JamCollType::toString(ijtype.collection_type));
    }

    if (names.size() > 0) {
      out = Rf_setAttrib(out, R_NamesSymbol, toSEXP(names));
    }
    if (meta != R_NilValue) {
      SEXP meta_names = Rf_getAttrib(meta, R_NamesSymbol);
      for (R_len_t i = 0; i < XLENGTH(meta_names); i++) {
        out = setAttrib(out, STRING_ELT(meta_names, i), VECTOR_ELT(meta, i));
      }
    }
    UNPROTECT(2);
    return out;
  }
    
  } else {
    // primitive types
    stop("Lists of primitive types not supported.")
  }
  
}

SEXP unjam(const std::string& path) {
  
  std::ifstream fin(path, std::ios::binary);
  cereal::BinaryInputArchive bin(fin);
  JamType head;
  bin(head);

  switch (head.collection_type) {
   case JamCollType::VECTOR: return unjam_vector(bin, head);
   case JamCollType::MAP:    return unjam_named_list(bin, head);
   default:
     stop("Not implemented");
  }
}
