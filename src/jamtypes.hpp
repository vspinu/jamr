#ifndef __JAM_TYPES_HPP__
#define __JAM_TYPES_HPP__

#include <cstdint>
#include <string>
#include <stdexcept>

typedef int8_t   byte;
typedef uint8_t  ubyte;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;

struct JamCollType {
  enum type : ubyte {
    UNDEFINED = 0,
    VECTOR    = 1,
    ULIST     = 2,
    MLIST     = 3,
    DF        = 4,
    ARRAY     = 5
  };

  static std::string toString(type t) {
    switch(t) {
     case UNDEFINED: return "UNDEFINED";
     case VECTOR:    return "VECTOR";
     case ULIST:     return "ULIST";
     case MLIST:     return "MLIST";
     case DF:        return "DF";
     case ARRAY:     return "ARRAY";
    }
    throw std::runtime_error("Invalid JamCollType");
  }

};

class JamElType {

 public:
  
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

    MIXED = 99, 

    VECTOR = 101,
    ULIST  = 102,
    MLIST  = 103,
    DF     = 104,
    ARRAY  = 105, 

    UNDEFINED = 255
  };
  
  static std::string toString(type t) {
    switch(t) {
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
     case BITSET:    return "BITSET";

     case MIXED:     return "MIXED";

     case VECTOR:    return "VECTOR";
     case ULIST:     return "ULIST";
     case MLIST:     return "MLIST";
     case DF:        return "DF";
     case ARRAY:     return "ARRAY";

     case UNDEFINED: return "UNDEFINED";
    }
    throw std::runtime_error("Invalid JamElType");
  }
  
};

class JamType {

 public:

  JamType(){};
  JamType(JamCollType::type collection_type, JamElType::type element_type,
          bool has_names = false, bool has_meta = false) :
    element_type(element_type),
    collection_type(collection_type),
    extra((has_names ? 2 : 0) | (has_meta ? 1 : 0))
  {};

  bool hasMeta () const {
    return extra & 1;
  }

  bool hasNames () const {
    return extra & 2;
  }

  template<class Archive>
  void serialize(Archive & archive)
  {
    archive(collection_type, element_type, version, extra);
  }

 public:
  JamCollType::type collection_type = JamCollType::UNDEFINED;
  JamElType::type element_type = JamElType::UNDEFINED;

 private:
  ubyte version = 0;
  ubyte extra = 0;

};

const JamType JAM_META_HEAD = JamType(JamCollType::MLIST, JamElType::MIXED, true, true);


#endif
