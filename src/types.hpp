#ifndef __JAM_HPP__
#define __JAM_HPP__

#include <cstdint>
#include <string>
#include <fstream>
#include <iostream>
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/map.hpp>
#include <cereal/types/unordered_map.hpp>
#include <cereal/archives/binary.hpp>
#include <cereal/archives/json.hpp>
#include <Rcpp.h>
using namespace Rcpp;

typedef int8_t byte;
typedef uint8_t ubyte;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;

struct JamCollType {
  enum type : ubyte {
    STRUCT = 0,
    VECTOR = 1,
    MAP    = 2,
    UMAP   = 3
  };
};

struct JamType {
  enum type : ubyte {
    BOOL   = 0,
    BYTE   = 1,
    UBYTE  = 2,
    SHORT  = 3,
    USHORT = 4,
    INT    = 5,
    UINT   = 6,
    LONG   = 7,
    ULONG  = 8,
    FLOAT  = 9,
    DOUBLE = 10,
    UTF8   = 11,
    BITSET = 12, 
    UNDEFINED = 255
  };
};

std::string jamTypeToString(JamType::type t) {
  switch(t) {
   case JamType::BOOL:   return "BOOL";
   case JamType::BYTE:   return "BYTE";
   case JamType::UBYTE:  return "UBYTE";
   case JamType::SHORT:  return "SHORT";
   case JamType::USHORT: return "USHORT";
   case JamType::INT:    return "INT";
   case JamType::UINT:   return "UINT";
   case JamType::LONG:   return "LONG";
   case JamType::ULONG:  return "ULONG";
   case JamType::FLOAT:  return "FLOAT";
   case JamType::DOUBLE: return "DOUBLE";
   case JamType::UTF8:   return "UTF8";
   case JamType::BITSET: return "BITSET";
  }
  throw std::runtime_error("Invalid JamType");
}

struct JamHead {
  JamCollType::type collection_type = JamCollType::STRUCT;
  JamType::type element_type = JamType::UNDEFINED;

  JamHead(){};
  JamHead(JamCollType::type collection_type, JamType::type element_type) :
    element_type(element_type), collection_type(collection_type) {};
  
  template<class Archive>
  void serialize(Archive & archive)
  {
    archive(collection_type, element_type, version, meta);
  }
  
 private:
  ubyte version = 0;
  ubyte meta = 0;
};

struct JamListHead {
  uint length;
  std::vector<std::string> attr_names;
  std::vector<JamHead> attr_types;
}

enum RType {
  R_LGL,
  R_INT,
  R_DBL,
  R_CHR,
  R_RAW,
  R_FACTOR,
  R_DATE,
  R_DATETIME,
  R_TIME
};

RType Jam2RType (JamType::type jtype) {
  switch(jtype) {
   case JamType::BOOL:
     return R_LGL;
   case JamType::BYTE:
   case JamType::SHORT:
   case JamType::INT:
   case JamType::UBYTE:
   case JamType::USHORT:
   case JamType::UINT:
     return R_INT;
   case JamType::LONG:
   case JamType::ULONG:
   case JamType::FLOAT:
   case JamType::DOUBLE:
     return R_DBL;
     // case JamType::UTF8:
     //   return R_CHR;
     // case JamType::BINARY:
     //   return R_RAW;
  }
  throw std::runtime_error("Invalid JamType");
}

SEXPTYPE RType2SexpType(RType rtype) {
  switch(rtype) {
   case R_LGL:      return LGLSXP;
   case R_INT:      return INTSXP;
   case R_DBL:      return REALSXP;
   case R_CHR:      return STRSXP;
   case R_RAW:      return VECSXP;
   case R_FACTOR:   return INTSXP;
   case R_DATE:     return INTSXP;
   case R_DATETIME: return REALSXP;
   case R_TIME:     return REALSXP;
  }
  throw std::runtime_error("Invalid RType");
}

template <class SrcType, class DestType>
void copyRecast(const SrcType* src, DestType* dest, size_t n) {
  auto recast = reinterpret_cast<const SrcType*>(src);
  std::copy(&recast[0], &recast[0] + n, dest);
}

template <class T>
SEXP toSEXP(const std::vector<T>& vec, RType rtype) {
  const T* pv = vec.data();
  size_t n = vec.size();
  SEXP out = PROTECT(Rf_allocVector(RType2SexpType(rtype), n));

  switch(rtype) {
   case R_LGL:
   case R_INT:
     copyRecast<T>(pv, INTEGER(out), n);
     break;
   case R_DBL:
     copyRecast<T>(pv, REAL(out), n);
     break;
   default:
     stop("Not implemented");
  }
  
  UNPROTECT(1);
  return out;
}

#endif
