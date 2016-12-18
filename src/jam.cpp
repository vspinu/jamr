#include "common.hpp"

void jam_meta(cereal::BinaryOutputArchive& bout, SEXP x) {
  PRINT(">META\n");
  std::vector<std::string> names;
  std::vector<int> ixs;
  uint j = 0;
  SEXP attr = ATTRIB(x);
  while (attr != R_NilValue) {
    bool can_jam = Sexp2JamElType(TYPEOF(CAR(attr))) != JamElType::UNSUPORTED;
    if (can_jam) {
      ixs.push_back(j);
      if (TAG(attr) == R_NilValue)
        names.push_back("");
      else
        names.push_back(std::string(CHAR(PRINTNAME(TAG(attr)))));
    }
    j++;
    attr = CDR(attr);
  }
  bout(names);
  bout(static_cast<uint>(names.size()));

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
  PRINT("<META\n");
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
  std::vector<ubyte> bytes(n);
  size_t i;
  for (i = 0; i < N; i += 2) {
    ubyte b1 = (x[i] == NA_INTEGER) ? 2 : (x[i] ? 1 : 0); // 0010, 0001 or 0000
    ubyte b2 = (x[i+1] == NA_INTEGER) ? 8 : (x[i+1] ? 4 : 0); // 1000, 0100 or 0000      
    bytes[i/2] = (b1 | b2);
  }
  if (N % 2) {
    // set last odd element separately
    bytes[n-1] = (x[N-1] == NA_INTEGER) ? 14 : (x[N-1] ? 13 : 12); // 1110, 1101 or 1100
  }
  bout(bytes);
}

void jam_utf8_vector_tail (cereal::BinaryOutputArchive& bout, SEXP x) {
  uint N = LENGTH(x);

  std::vector<uint8_t> data;
  size_t data_len = 0;

  std::vector<int> nchars(N);
  int max_nchars = 0;
  
  for (int i = 0; i < N; i++) {
    SEXP str = STRING_ELT(x, i);
    if (str == R_NaString) {
      nchars[i] = -1;
    } else  if (str == R_BlankString) {
      nchars[i] = 0;
    } else {
      const char* ch = Rf_translateCharUTF8(str);
      int len = strlen(ch);
      nchars[i] = len;
      data_len += len;
      max_nchars = std::max(len, max_nchars);
      std::copy(&ch[0], &ch[len], std::inserter(data, data.end()));
    }
  }

  JamType head = JamType(JamCollType::VECTOR, JamElType::INT, false, false);
  
  if (max_nchars >= MAX_SHORT) {
    bout(head);
    bout(nchars);
  } else if (max_nchars >= MAX_BYTE) {
    std::vector<short> tnchars(nchars.begin(), nchars.end());
    head.el_type = JamElType::SHORT;
    bout(head);
    bout(tnchars);
  } else {
    std::vector<byte> tnchars(nchars.begin(), nchars.end());
    head.el_type = JamElType::BYTE;
    bout(head);
    bout(tnchars);
  }

  bout(data);
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

void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head) {
  JamType head = get_head(x);
  if (with_head && TYPEOF(x) == INTSXP) {
    // FIXME: ULISTs of int vectors don't use this optimization
    head.el_type = best_int_type(x);
  }
  jam_sexp(bout, x, with_head, head);
}

void jam_sexp(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, JamType& head) {
#ifdef DEBUG
  head.print("jam_sexp:");
#endif
  
  size_t N = XLENGTH(x);
  JamElType::type jtype = head.el_type;

  if (with_head) bout(head);
  if (head.hasMeta()) jam_meta(bout, x);
 
  switch (TYPEOF(x)) {

   case NILSXP:
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
