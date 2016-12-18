#ifndef __JAM_TYPES_HPP__
#define __JAM_TYPES_HPP__

#include <limits>
#include <cstdint>
#include <string>
#include <stdexcept>
#include <iostream>
#include <cereal/cereal.hpp>

typedef int8_t   byte;
typedef uint8_t  ubyte;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;

const byte NA_BYTE = std::numeric_limits<byte>::min();
const ubyte NA_UBYTE = std::numeric_limits<ubyte>::max();
const short NA_SHORT = std::numeric_limits<short>::min();
const ushort NA_USHORT = std::numeric_limits<ushort>::max();
const int NA_INT = std::numeric_limits<int>::min();
const uint NA_UINT = std::numeric_limits<uint>::max();
const long NA_LONG = std::numeric_limits<long>::min();
const ulong NA_ULONG = std::numeric_limits<ulong>::max();

const byte MAX_BYTE = std::numeric_limits<byte>::max();
const ubyte MAX_UBYTE = std::numeric_limits<ubyte>::max();
const short MAX_SHORT = std::numeric_limits<short>::max();
const ushort MAX_USHORT = std::numeric_limits<ushort>::max();
const int MAX_INT = std::numeric_limits<int>::max();
const uint MAX_UINT = std::numeric_limits<uint>::max();
const long MAX_LONG = std::numeric_limits<long>::max();
const ulong MAX_ULONG = std::numeric_limits<ulong>::max();

const byte MIN_BYTE = std::numeric_limits<byte>::min();
const ubyte MIN_UBYTE = std::numeric_limits<ubyte>::min();
const short MIN_SHORT = std::numeric_limits<short>::min();
const ushort MIN_USHORT = std::numeric_limits<ushort>::min();
const int MIN_INT = std::numeric_limits<int>::min();
const uint MIN_UINT = std::numeric_limits<uint>::min();
const long MIN_LONG = std::numeric_limits<long>::min();
const ulong MIN_ULONG = std::numeric_limits<ulong>::min();

struct JamCollType {
  enum type : ubyte {
    NIL       = 0,
    VECTOR    = 1,
    LIST      = 2,
    HDF       = 3,
    META      = 4, 

    UNSUPORTED = 254, 
    UNDEFINED  = 255
  };

  static std::string toString(type t) {
    switch(t) {
     case NIL:        return "NIL";
     case VECTOR:     return "VECTOR";
     case LIST:       return "LIST";
     case HDF:        return "HDF";
     case META:       return "META";
     case UNSUPORTED: return "UNSUPORTED";
     case UNDEFINED:  return "UNDEFINED";
    }
    return "INVALID(" + std::to_string((int) t) + ")";
  }
};

class JamElType {

 public:
  
  enum type : ubyte {
    NIL    = 0, 
    BOOL   = 1,
    BYTE   = 2,
    UBYTE  = 3,
    SHORT  = 4,
    USHORT = 5,
    INT    = 6,
    UINT   = 7,
    LONG   = 8,
    ULONG  = 9,
    FLOAT  = 10,
    DOUBLE = 11,
    UTF8   = 12,
    STRING = 13,

    MIXED  = 99, 

    VECTOR = 101,
    LIST   = 102,
    DF     = 103,

    UNSUPORTED = 254, 
    UNDEFINED  = 255
  };
  
  static std::string toString(type t) {
    switch(t) {
     case NIL:       return "NIL";
     case BOOL:      return "BOOL";
     case BYTE:      return "BYTE";
     case UBYTE:     return "UBYTE";
     case SHORT:     return "SHORT";
     case USHORT:    return "USHORT";
     case INT:       return "INT";
     case UINT:      return "UINT";
     case LONG:      return "LONG";
     case ULONG:     return "ULONG";
     case FLOAT:     return "FLOAT";
     case DOUBLE:    return "DOUBLE";
     case UTF8:      return "UTF8";
     case STRING:    return "STRING";

     case MIXED:     return "MIXED";

     case VECTOR:    return "VECTOR";
     case LIST:      return "MLIST";
     case DF:        return "DF";

     case UNSUPORTED: return "UNSUPORTED";
     case UNDEFINED:  return "UNDEFINED";
    }
    return "INVALID(" + std::to_string((int) t) + ")";
  }
};

class JamType {

 public:

  JamType(){};
  JamType(JamCollType::type coll_type, JamElType::type el_type,
          bool has_names = false, bool has_meta = false) :
    el_type(el_type),
    coll_type(coll_type),
    extra((has_names ? 2 : 0) | (has_meta ? 1 : 0))
  {};

  bool hasMeta () const {
    return extra & 1;
  }

  void setMeta (const bool& has_meta) {
    if (has_meta) extra |= 1;
    else extra &= ~(1);
  }

  void print () const {
    print("");
  }
  void print (std::string name) const {
    std::cout << name
              << " coll_type:" << JamCollType::toString(coll_type)
              << " el_type:" << JamElType::toString(el_type)
              << " meta:" << (hasMeta() ? "true" : "false") << std::endl;
  }
  
  template<class Archive>
  void serialize(Archive & archive)
  {
    archive(coll_type, el_type, version, extra);
  }

 public:
  JamCollType::type coll_type = JamCollType::UNDEFINED;
  JamElType::type el_type = JamElType::UNDEFINED;

 private:
  ubyte version = 0;
  ubyte extra = 0;

};

const JamType JAM_META_HEAD = JamType(JamCollType::META, JamElType::MIXED, true, false);
const JamType JAM_NIL_HEAD = JamType(JamCollType::NIL, JamElType::UNDEFINED, false, false);

#endif
