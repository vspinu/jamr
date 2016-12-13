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

#include "jam.hpp"

#define GET_NAMES(x) Rf_getAttrib(x, R_NamesSymbol)

int common_el_type(SEXP x) {
  SEXPTYPE common_type = TYPEOF(VECTOR_ELT(x, 0));
  R_len_t N = XLENGTH(x);
  bool same = true;
  for (R_len_t i = 1; i < N; i++) {
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
  if (TYPEOF(x) != VECSXP) {
    JamElType::type el_type = (XLENGTH(x) == 0) ? JamElType::UNDEFINED : Sexp2JamType(TYPEOF(x));
    JamType(JamCollType::VECTOR, el_type, GET_NAMES(x) != R_NilValue, ATTRIB(x) != R_NilValue);
  } else {
    if (XLENGTH(x) == 0) {
      return JamType(JamCollType::ULIST, JamElType::UNDEFINED);
    } else {
      bool has_names = GET_NAMES(x) != R_NilValue;
      bool has_meta = ATTRIB(x) != R_NilValue;
      int common_type = common_el_type(x);
      if (common_el_type >= 0) {
        JamType(JamCollType::ULIST, Sexp2JamType(common_type), has_names, has_meta);
      } else {
        JamType(JamCollType::MLIST, JamElType::MIXED, has_names, has_meta);
      }
    }
  }
}

void jam_names(cereal::BinaryOutputArchive& bout, SEXP x) {
  std::vector<std::string> names = as<std::vector<std::string>>(GET_NAMES(x));
  bout(names);
}

void jam_meta(const cereal::BinaryOutputArchive& bout, SEXP x) {
  
}

template<typename TOUT, typename TIN>
void jam_primitive_vector_tail (cereal::BinaryOutputArchive& bout, TIN* x, const size_t& N) {
  std::vector<TOUT> ovec(x, x + N);
  bout(ovec);
}

void jam_character_vector_tail (cereal::BinaryOutputArchive& bout, SEXP x) {
  bout(as<std::vector<std::string>>(x));
}

void jam_list_tail(cereal::BinaryOutputArchive& bout, SEXP x, const JamType& head) {
  uint N = LENGTH(x); // max list size is uint max element
  bout(N);
  if (N != 0) {
    switch (head.collection_type) {
     case JamCollType::ULIST:
       {
         JamType common_head = get_head(VECTOR_ELT(x, 0));
         bout(common_head);
         for (uint i = 0; i < N; i++) {
           jam_vector(bout, VECTOR_ELT(x, i), false, common_head);
         }
       }
       break;
     case JamCollType::MLIST:
       for (R_len_t i = 0; i < N; i++) {
         jam_vector(bout, VECTOR_ELT(x, i), true);
       }
     default: stop("should not end here");
    }
  }
}

void stop_on_invalid_type(SEXP x, const JamElType::type& jtype) {
  stop("Cannot jam object of R type %s into JamElType %s",
       Rf_type2char(TYPEOF(x)), JamElType::toString(jtype));
}

void jam_vector(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, const JamType& head) {

  R_len_t N = XLENGTH(x);
  JamElType::type jtype = head.element_type;

  if (with_head) bout(head);
  if (head.hasNames()) jam_names(bout, x);
  if (head.hasMeta()) jam_meta(bout, x);
 
  switch (TYPEOF(x)) {
    
   case LGLSXP:
     switch (jtype) {
      case JamElType::BYTE:
        jam_primitive_vector_tail<byte>(bout, INTEGER(x), N);
        break;
      case JamElType::UBYTE:
        jam_primitive_vector_tail<ubyte>(bout, INTEGER(x), N);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
     
   case INTSXP:
     switch (jtype) {
      case JamElType::BYTE:
        jam_primitive_vector_tail<byte>(bout, INTEGER(x), N);
        break;
      case JamElType::UBYTE:
        jam_primitive_vector_tail<ubyte>(bout, INTEGER(x), N);
        break;
      case JamElType::SHORT:
        jam_primitive_vector_tail<short>(bout, INTEGER(x), N);
        break;
      case JamElType::USHORT:
        jam_primitive_vector_tail<ushort>(bout, INTEGER(x), N);
        break;
      case JamElType::INT:
        jam_primitive_vector_tail<int>(bout, INTEGER(x), N);
        break;
      case JamElType::UINT:
        jam_primitive_vector_tail<uint>(bout, INTEGER(x), N);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
     
   case REALSXP:
     switch(jtype) {
      case JamElType::FLOAT:
        jam_primitive_vector_tail<float>(bout, REAL(x), N);
        break;
      case JamElType::DOUBLE:
        jam_primitive_vector_tail<double>(bout, REAL(x), N);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     }
     break;
     
   case STRSXP:
     if (jtype != JamElType::UTF8) stop_on_invalid_type(x, jtype);
     jam_character_vector_tail(bout, x);
     break;
     
   case VECSXP:
     jam_list_tail(bout, x, head);
     break;

   default:
     stop("Cannot jam object of type %s", Rf_type2char(TYPEOF(x)));
  }
}

void jam_vector(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head) {
  jam_vector(bout, x, with_head, get_head(x));
}
