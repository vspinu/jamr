// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(Rcereal)]]

#include "types.hpp"

template <class BinType>
SEXP unjam_sexp(cereal::BinaryInputArchive& bin, RType out_rtype){
  std::vector<BinType> vec;
  bin(vec);
  return toSEXP<BinType>(vec, out_rtype);
}

// [[Rcpp::export]]
SEXP c_unjam_vector(const std::string& path) {

  std::ifstream fin(path, std::ios::binary);
  cereal::BinaryInputArchive bin(fin);
  JamHead head;
  bin(head);

  switch(head.element_type) {
   // case JamType::BOOL: {
   //   std::vector<bool> vec;
   //   bin(vec);
   //   return toSEXP<bool>(vec, R_INT);
   // }

   case JamType::BYTE: return unjam_sexp<byte>(bin, R_INT);
   case JamType::UBYTE: return unjam_sexp<ubyte>(bin, R_INT);
   case JamType::SHORT: return unjam_sexp<short>(bin, R_INT);
   case JamType::USHORT: return unjam_sexp<ushort>(bin, R_INT);
   case JamType::INT: return unjam_sexp<int>(bin, R_INT);
   case JamType::UINT: return unjam_sexp<uint>(bin, R_INT);
     // case JamType::LONG:   return "LONG";
     // case JamType::ULONG:  return "ULONG";
   case JamType::FLOAT: return unjam_sexp<float>(bin, R_DBL);
   case JamType::DOUBLE: return unjam_sexp<double>(bin, R_DBL);
     // case JamType::UTF8:   return "UTF8";
     // case JamType::BITSET: return "BITSET";
   default:
     // stop("Invalid JamType in file header: %s",
     //      jamTypeToString(head.element_type));
     stop("Invalid JamType in file header.");
  }
  
}
