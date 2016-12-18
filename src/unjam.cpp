#include "common.hpp"

SEXP unjam_bool_vec_tail(cereal::BinaryInputArchive& bin) {
  std::vector<ubyte> bytes;
  bin(bytes);
  size_t n = bytes.size();
  size_t N = ((bytes[n-1] & 12) == 12) ? n*2 - 1 : n*2; // last 2 bits = 11, means no value
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, N));
  int* pt = LOGICAL(out);
  for (size_t i = 0; i < N; i++) {
    std::div_t div = std::div(i, 2);
    if (div.rem) {
      // lower bits
      if (bytes[div.quot] & 8) pt[i] = NA_INTEGER;
      else pt[i] = (bytes[div.quot] & 12) != 0;
    } else {
      // upper bits
      if (bytes[div.quot] & 2) pt[i] = NA_INTEGER;
      else pt[i] = (bytes[div.quot] & 3) != 0;
    }
  }
  UNPROTECT(1);
  return out;
}

template <class inT>
SEXP unjam_vec_tail(cereal::BinaryInputArchive& bin, SEXPTYPE stype){
  std::vector<inT> vec;
  bin(vec);
  return toSEXP<inT>(vec, stype);
}

template <class inT>
SEXP unjam_int_vec_tail(cereal::BinaryInputArchive& bin, SEXPTYPE stype, const inT& na_val){
  std::vector<inT> vec;
  bin(vec);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, vec.size()));
  int* px = INTEGER(out);
  for (size_t i = 0; i < vec.size(); i++) {
    if (vec[i] == na_val)
      px[i] = NA_INTEGER;
    else
      px[i] = vec[i];
  }
  UNPROTECT(1);
  return out;
}

template<class lenT>
SEXP unjam_char_utf8_tail(cereal::BinaryInputArchive& bin) {
  
  std::vector<lenT> nchars;
  bin(nchars);
  std::vector<uint8_t> data;
  bin(data);
  const char* dpt = reinterpret_cast<const char*>(data.data());
  size_t N = nchars.size();
  
  SEXP out = PROTECT(Rf_allocVector(STRSXP, N));
  for (int i = 0; i < N; i++){
    lenT n = nchars[i];
    if (n == 0)
      SET_STRING_ELT(out, i, R_BlankString);
    else if (n == -1)
      SET_STRING_ELT(out, i, R_NaString);
    else {
      SET_STRING_ELT(out, i, Rf_mkCharLenCE(dpt, n, CE_UTF8));
      dpt += n;
    }
  }
  UNPROTECT(1);
  return out;
}

SEXP unjam_list_tail(cereal::BinaryInputArchive& bin, const JamType& head) {
  // head.print("unjam_list_tail");
  uint N;
  bin(N);
  
  SEXP out = PROTECT(Rf_allocVector(VECSXP, N));

  if (N > 0) {
    switch (head.el_type) {
     case JamElType::MIXED:
       {
         for (int i = 0; i < N; ++i)
           SET_VECTOR_ELT(out, i, unjam_sexp(bin));
       }
       break;
     case JamElType::VECTOR:
       {
         JamType common_head;
         bin(common_head);
         for (int i = 0; i < N; ++i)
           SET_VECTOR_ELT(out, i, unjam_sexp(bin, common_head));
       }
       break;
     default:
       stop("Element type of LISTs can only be VECTOR or MIXED.");
    }
  }
    
  UNPROTECT(1);
  return out;
}

SEXP unjam_meta(cereal::BinaryInputArchive& bin) {
  PRINT(">META\n");
  std::vector<std::string> names;
  bin(names);
  SEXP out = PROTECT(unjam_sexp(bin, JAM_META_HEAD));
  if (names.size() > 0) {
    Rf_setAttrib(out, R_NamesSymbol, toSEXP(names, STRSXP));
  }
  UNPROTECT(1);
  PRINT("<META\n");
  return out;
}

SEXP unjam_sexp(cereal::BinaryInputArchive& bin, const JamType& head) {
#ifdef DEBUG
  head.print("unjam_sexp:");
#endif
  
  // [META]
  SEXP meta = R_NilValue;
  int nprot = 0;
  if (head.hasMeta()){
    meta = PROTECT(unjam_meta(bin));
    nprot++;
  }
  
  SEXP out; 

  switch (head.coll_type) {

   case JamCollType::NIL:
     out = R_NilValue;
     break;

   case JamCollType::VECTOR:
     switch (head.el_type) {
      case JamElType::NIL:        stop("Invalid VECTOR specification. Elements of a vector cannot be nil.");
      case JamElType::BOOL:       out = unjam_bool_vec_tail(bin); break;
      case JamElType::BYTE:       out = unjam_int_vec_tail<byte>(bin, INTSXP, NA_BYTE); break;
      case JamElType::UBYTE:      out = unjam_int_vec_tail<ubyte>(bin, INTSXP, NA_UBYTE); break;
      case JamElType::SHORT:      out = unjam_int_vec_tail<short>(bin, INTSXP, NA_SHORT); break;
      case JamElType::USHORT:     out = unjam_int_vec_tail<ushort>(bin, INTSXP, NA_USHORT); break;
      case JamElType::INT:        out = unjam_vec_tail<int>(bin, INTSXP); break;
      case JamElType::UINT:       out = unjam_int_vec_tail<uint>(bin, INTSXP, NA_UINT); break;

      case JamElType::FLOAT:      out = unjam_vec_tail<float>(bin, REALSXP); break;
      case JamElType::DOUBLE:     out = unjam_vec_tail<double>(bin, REALSXP); break;

      case JamElType::STRING:     out = unjam_vec_tail<std::string>(bin, STRSXP); break;

      case JamElType::UTF8:
        {
          JamType nchar_head;
          bin(nchar_head);
          switch (nchar_head.el_type) {
           case JamElType::BYTE:  out = unjam_char_utf8_tail<byte>(bin); break;
           case JamElType::SHORT: out = unjam_char_utf8_tail<short>(bin); break;
           case JamElType::INT:   out = unjam_char_utf8_tail<int>(bin); break;
           default:
             stop("Invalid JamElType (%s) for nchar specification.",
                  JamElType::toString(nchar_head.el_type));
          }
        };
        break;
      default:
        stop("Unsupported JamElType in the header (%s).", JamElType::toString(head.el_type));
     }  
     break;

   case JamCollType::META:
   case JamCollType::LIST:
     out = unjam_list_tail(bin, head);
     break;

   default:
     stop("Unsupported JamCollType in the header (%s).", JamCollType::toString(head.coll_type));
  }

  PROTECT(out);
  nprot++;

  if (meta != R_NilValue) {
    SEXP meta_names = Rf_getAttrib(meta, R_NamesSymbol);
    int metaN = LENGTH(meta_names);
    for (int i = 0; i < metaN; i++) {
      Rf_setAttrib(out, Rf_installChar(STRING_ELT(meta_names, i)), VECTOR_ELT(meta, i));
    }
  }
  
  UNPROTECT(nprot);
  return out;
}

SEXP unjam_sexp(cereal::BinaryInputArchive& bin) {
  JamType head;
  bin(head);
  return unjam_sexp(bin, head);
}

// [[Rcpp::export]]
SEXP c_unjam(const std::string& path) {
  std::ifstream fin(path, std::ios::binary);
  cereal::BinaryInputArchive bin(fin);
  return unjam_sexp(bin);
}
