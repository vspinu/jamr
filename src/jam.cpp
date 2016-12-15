// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(Rcereal)]]


// // for test
// {
//   std::ifstream fin(path, std::ios::binary);
//   cereal::BinaryInputArchive bout(fin);
//   bout(JamType());
//   std::vector<TOUT> ivec;
//   bout(ivec);
//   cereal::JSONOutputArchive ojson(std::cout);
//   ojson(CEREAL_NVP(ivec));
//   fin.close();
// }

// void c_jam_map(SEXP x, const std::string& path) {
//   if (TYPEOF(x) != VECSXP) {
//     stop("Invalid SEXP type.");
//   } else {
//     std::map<std::string,JamType> jtypemap;
//     for (s = ATTRIB(x); s != R_NilValue; s = CDR(s))
//       if (TAG(s) == name) {
// 	    if (name == R_DimNamesSymbol && TYPEOF(CAR(s)) == LISTSXP)
//           error("old list is no longer allowed for dimnames attribute");
// 	    SET_NAMED(CAR(s), 2);
// 	    return CAR(s);
//       }
//   }
// }

#include "jamtypes.hpp"
#include "rtypes.hpp"
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/archives/binary.hpp>
// #include <cereal/archives/json.hpp>
// #include <cereal/types/map.hpp>
// #include <cereal/types/unordered_map.hpp>

void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head = true);
void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, JamType& head);

#define GET_NAMES(x) Rf_getAttrib(x, R_NamesSymbol)

int meta_length(const SEXP x) {
  SEXP attr = ATTRIB(x);
  int len = 0;
  while (attr != R_NilValue) {
    if (TAG(attr) != R_NamesSymbol) len++;
    attr = CDR(attr);
  }
  return len;
}

int common_el_type(SEXP x) {
  SEXPTYPE common_type = TYPEOF(VECTOR_ELT(x, 0));
  size_t N = XLENGTH(x);
  bool same = true;
  for (size_t i = 1; i < N; i++) {
    SEXPTYPE type = TYPEOF(VECTOR_ELT(x, i));
    if (type != common_type || type == VECSXP) {
      same = false;
      break;
    }
  }
  if (same) return (int) common_type;
  else return -1;
}

JamType get_head(SEXP x) {
  bool has_names = GET_NAMES(x) != R_NilValue;
  bool has_meta = meta_length(x) > 0;

  if (TYPEOF(x) != VECSXP) {
    JamElType::type el_type = (XLENGTH(x) == 0) ? JamElType::UNDEFINED : Sexp2JamElType(TYPEOF(x));
    return JamType(JamCollType::VECTOR, el_type, has_names, has_meta);
  } else {
    if (XLENGTH(x) == 0) {
      return JamType(JamCollType::LIST, JamElType::UNDEFINED);
    } else {
      if (common_el_type(x) >= 0) {
        return JamType(JamCollType::LIST, JamElType::VECTOR, has_names, has_meta);
      } else {
        return JamType(JamCollType::LIST, JamElType::MIXED, has_names, has_meta);
      }
    }
  }
}

void jam_names(cereal::BinaryOutputArchive& bout, SEXP x) {
  std::vector<std::string> names = as<std::vector<std::string>>(GET_NAMES(x));
  bout(names);
}

void jam_meta(cereal::BinaryOutputArchive& bout, SEXP x) {
  std::vector<std::string> names;
  std::vector<int> ixs;
  uint j = 0;
  SEXP attr = ATTRIB(x);
  while (attr != R_NilValue) {
    bool can_jam = Sexp2JamElType(TYPEOF(CAR(attr))) != JamElType::UNSUPORTED;
    if (can_jam && (TAG(attr) != R_NamesSymbol)) {
      ixs.push_back(j);
      if (TAG(attr) == R_NilValue)
        names.push_back("");
      else
        names.push_back(std::string(CHAR(PRINTNAME(TAG(attr)))));
    }
    j++;
    attr = CDR(attr);
  }

  // std::cout << "meta names: ";
  //   for (const auto& n : names)
  //     std::cout << n << " ";
  // std::cout << std::endl;

  bout(names);
  bout((uint) names.size());

  if (names.size() > 0) {
    uint j = 0;
    attr = ATTRIB(x);
    for (uint i = 0; i < names.size(); i++) {
      while (j < ixs[i]){
        j++;
        attr = CDR(attr);
      }
      jam_sexp(bout, CAR(attr), true);
    }
  }
}

template<typename TOUT, typename TIN>
void jam_vector_tail (cereal::BinaryOutputArchive& bout, TIN* x, const size_t& N) {
  std::vector<TOUT> ovec(x, x + N);
  bout(ovec);
}

template<typename TOUT>
void jam_int_vector_tail (cereal::BinaryOutputArchive& bout, int* x, const size_t& N, const TOUT& na_val) {
  std::vector<TOUT> ovec(N);
  for (size_t i = 0; i < N; i++) {
    int xi = x[i];
    if (xi == NA_INTEGER) ovec[i] = na_val;
    else ovec[i] = xi;
  }
  bout(ovec);
}

void jam_bool_vector_tail (cereal::BinaryOutputArchive& bout, int* x, const size_t& N) {
  size_t n = (N + 1)/2;
  std::vector<byte> bytes(n);
  size_t i;
  for (i = 1; i < N; i += 2) {
    byte b1 = (x[i-1] == NA_INTEGER) ? 2 : (x[i-1] ? 1 : 0); // 0010, 0001 or 0000
    byte b2 = (x[i] == NA_INTEGER) ? 8 : (x[i] ? 4 : 0); // 1000, 0100 or 0000      
    bytes[i/2] = b1 | b2;
  }
  if (N % 2) {
    // set odd last element separately
    bytes[n] = (x[N] == NA_INTEGER) ? 14 : (x[N] ? 13 : 12); // 1110, 1101 or 1100
  }
  bout(bytes);
}

void jam_utf8_vector_tail (cereal::BinaryOutputArchive& bout, SEXP x) {
  uint N = LENGTH(x);

  bout(N);

  for (uint i = 0; i < N; i++) {
    SEXP str = STRING_ELT(x, i);
    if (str == R_NaString) {
      bout(static_cast<uint>(-1));
    } else  if (str == R_BlankString) {
      bout(static_cast<uint>(0));
    } else {
      const char* ch = Rf_translateCharUTF8(str);
      int len = strlen(ch);
      bout(len);
      bout(cereal::BinaryData<const void*>(ch, len));
    }
  }
}

void jam_string_vector_tail(cereal::BinaryOutputArchive& bout, SEXP x) {
  bout(as<std::vector<std::string>>(x));
}

void jam_list_tail(cereal::BinaryOutputArchive& bout, SEXP x, JamType& head) {
  uint N = LENGTH(x); // max list size is uint max element
  bout(N);
  if (N != 0) {
    switch (head.el_type) {
     case JamElType::VECTOR:
       {
         JamType common_head = get_head(VECTOR_ELT(x, 0));
         bout(common_head);
         for (uint i = 0; i < N; i++) {
           jam_sexp(bout, VECTOR_ELT(x, i), false, common_head);
         }
       }
       break;
     case JamElType::MIXED:
       for (size_t i = 0; i < N; i++) {
         jam_sexp(bout, VECTOR_ELT(x, i), true);
       }
       break;
     default: stop("Should not happen. Please report.");
    }
  }
}

void stop_on_invalid_type(SEXP x, const JamElType::type& jtype) {
  stop("Cannot jam object of R type %s into JamElType %s",
       Rf_type2char(TYPEOF(x)), JamElType::toString(jtype));
}

void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head) {
  JamType head = get_head(x);
  jam_sexp(bout, x, with_head, head);
}

void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, JamType& head) {
  // head.print("jam_sexp:");

  if (TYPEOF(x) == INTSXP) {
    head.el_type = best_int_type(x);
    // std::cout << "best int type:" << JamElType::toString(head.el_type) << std::endl;
  }

  size_t N = XLENGTH(x);
  JamElType::type jtype = head.el_type;

  if (with_head) bout(head);
  if (head.hasNames()) jam_names(bout, x);
  if (head.hasMeta()) jam_meta(bout, x);
 
  switch (TYPEOF(x)) {

   case NILSXP:
     bout(JAM_NIL_HEAD);
     break;
    
   case LGLSXP:
     switch (jtype) {
      case JamElType::BOOL:
        jam_bool_vector_tail(bout, LOGICAL(x), N);
        break;
      case JamElType::BYTE:
        jam_int_vector_tail<byte>(bout, LOGICAL(x), N, NA_BYTE);
        break;
      case JamElType::UBYTE:
        jam_int_vector_tail<ubyte>(bout, LOGICAL(x), N, NA_UBYTE);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
     
   case INTSXP:

     switch (jtype) {
      case JamElType::BYTE:
        jam_int_vector_tail<byte>(bout, INTEGER(x), N, NA_BYTE);
        break;
      case JamElType::UBYTE:
        jam_int_vector_tail<ubyte>(bout, INTEGER(x), N, NA_UBYTE);
        break;
      case JamElType::SHORT:
        jam_int_vector_tail<short>(bout, INTEGER(x), N, NA_SHORT);
        break;
      case JamElType::USHORT:
        jam_int_vector_tail<ushort>(bout, INTEGER(x), N, NA_USHORT);
        break;
      case JamElType::INT:
        jam_vector_tail<int>(bout, INTEGER(x), N);
        break;
      case JamElType::UINT:
        jam_int_vector_tail<uint>(bout, INTEGER(x), N, NA_UINT);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
     
   case REALSXP:
     switch(jtype) {
      case JamElType::FLOAT:
        jam_vector_tail<float>(bout, REAL(x), N);
        break;
      case JamElType::DOUBLE:
        jam_vector_tail<double>(bout, REAL(x), N);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     }
     break;
     
   case STRSXP:
     switch(jtype) {
      case JamElType::UTF8:
        jam_utf8_vector_tail(bout, x);
        break;
      case JamElType::STRING:
        jam_string_vector_tail(bout, x);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     }
     break;
     
   case VECSXP:
     jam_list_tail(bout, x, head);
     break;

   default:
     stop("Cannot jam object of type %s", Rf_type2char(TYPEOF(x)));
  }
}

// [[Rcpp::export]]
void c_jam(SEXP x, const std::string path) {
  std::ofstream fout(path, std::ios::binary);
  cereal::BinaryOutputArchive bout(fout);
  jam_sexp(bout, x, true);
}
