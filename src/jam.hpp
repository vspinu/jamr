#ifndef __JAR_HPP__
#define __JAR_HPP__

/* #define DEBUG */

#ifdef DEBUG
#define PRINT(...) printf(__VA_ARGS__)
#else
#define PRINT(...)
#endif

// for tracing move & copy allocations

/* #define DEBUGALOC */

#ifdef DEBUGALOC
#define PRINTALOC(...) printf(__VA_ARGS__)
#else
#define PRINTALOC(...)
#endif

#include <iostream>
#include <fstream>

#include <string>
#include <vector>
#include <map>
#include <cstdint>
#include <limits>
#include <new> // for placement-new
#include <algorithm>

#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/types/map.hpp>
#include <cereal/archives/binary.hpp>

namespace jam {

// using namespace std;
typedef int8_t   byte;
typedef uint8_t  ubyte;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;

using std::string;
using std::vector;

template<class T>
using strmap = std::map<std::string, T>;

typedef vector<int> int_vec;
typedef vector<long> long_vec;
typedef vector<double> dbl_vec;
typedef vector<string> str_vec;

typedef strmap<int> int_map;
typedef strmap<long> long_map;
typedef strmap<double> dbl_map;
typedef strmap<string> str_map;

class JamException : public std::exception {
 public:
  JamException(std::string msg = "Invalid operation"): message(msg) {}
  virtual ~JamException() throw() {}
  const virtual char * what () const throw() { return message.c_str(); }
 private:
  std::string message;
};

const byte   NA_BYTE = std::numeric_limits<byte>::min();
const ubyte  NA_UBYTE = std::numeric_limits<ubyte>::max();
const short  NA_SHORT = std::numeric_limits<short>::min();
const ushort NA_USHORT = std::numeric_limits<ushort>::max();
const int    NA_INT = std::numeric_limits<int>::min();
const uint   NA_UINT = std::numeric_limits<uint>::max();
const long   NA_LONG = std::numeric_limits<long>::min();
const ulong  NA_ULONG = std::numeric_limits<ulong>::max();

const byte   MAX_BYTE = std::numeric_limits<byte>::max();
const ubyte  MAX_UBYTE = std::numeric_limits<ubyte>::max();
const short  MAX_SHORT = std::numeric_limits<short>::max();
const ushort MAX_USHORT = std::numeric_limits<ushort>::max();
const int    MAX_INT = std::numeric_limits<int>::max();
const uint   MAX_UINT = std::numeric_limits<uint>::max();
const long   MAX_LONG = std::numeric_limits<long>::max();
const ulong  MAX_ULONG = std::numeric_limits<ulong>::max();

const byte   MIN_BYTE = std::numeric_limits<byte>::min();
const ubyte  MIN_UBYTE = std::numeric_limits<ubyte>::min();
const short  MIN_SHORT = std::numeric_limits<short>::min();
const ushort MIN_USHORT = std::numeric_limits<ushort>::min();
const int    MIN_INT = std::numeric_limits<int>::min();
const uint   MIN_UINT = std::numeric_limits<uint>::min();
const long   MIN_LONG = std::numeric_limits<long>::min();
const ulong  MIN_ULONG = std::numeric_limits<ulong>::min();

const ulong  MAX_SIZE = std::numeric_limits<size_t>::max();

enum Type : ubyte {
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

  META   = 98, // deprecated
  MIXED  = 99, 

  VECTOR = 101,
  LIST   = 102,
  MAP    = 103,
  DF     = 104,

  UNSUPORTED = 254, 
  UNDEFINED  = 255
};

const byte NILVAL = 0; 

inline std::string Type2String(Type t) {
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
   case LIST:      return "LIST";
   case MAP:       return "MAP";
   case DF:        return "DF";

   case UNSUPORTED: return "UNSUPORTED";
   case UNDEFINED:  return "UNDEFINED";
   default:
     return "INVALID(" + std::to_string((int) t) + ")";
  }
}

static inline void throw_on_invalid_type (const Type& type) {
  throw JamException("Invalid use of type " + Type2String(type));
}


/* ------------------------------------------------------ */
/* VARIADIC ELEMENT TYPE                                  */
/* ------------------------------------------------------ */

struct VarEl {

  Type type;

  union {
    byte nil_val;
    byte bool_val;
    byte byte_val;
    ubyte ubyte_val;
    short short_val;
    ushort ushort_val;
    int int_val;
    uint uint_val;
    long long_val;
    ulong ulong_val;
    float float_val;
    double double_val;
    string string_val;
  };

  // DEFAULT CONSTRUCTORS

  VarEl() : type(NIL), nil_val(NILVAL) {}

  // COPY CONSTRUCTORS
  
  VarEl(const byte& val)   : type(BYTE), byte_val(val) { }
  VarEl(const ubyte& val)  : type(UBYTE), ubyte_val(val) { }
  VarEl(const short& val)  : type(SHORT), short_val(val) { }
  VarEl(const ushort& val) : type(USHORT), ushort_val(val) { }
  VarEl(const int& val)    : type(INT), int_val(val) { }
  VarEl(const uint& val)   : type(UINT), uint_val(val) { }
  VarEl(const long& val)   : type(LONG), long_val(val) { }
  VarEl(const ulong& val)  : type(ULONG), ulong_val(val) { }
  VarEl(const float& val)  : type(FLOAT), float_val(val) { }
  VarEl(const double& val) : type(DOUBLE), double_val(val) { }
  VarEl(const string& val) : type(STRING) { new (&string_val) string(val); }

  // MOVE CONSTRUCTOR FOR STRINGS

  VarEl(string&& val) : type(STRING) { new (&string_val) string(std::move(val)); }

  // OPERATOR=
  // copy
  VarEl& operator=(const VarEl& rhs) {
    if (this != &rhs) {
      clear();
      copy(rhs);
    }
    return *this;
  }
  // move
  VarEl& operator=(VarEl&& rhs) {
    if (this != &rhs) {
      clear();
      move(rhs);
    }
    return *this;
  }

  // DESTRUCTOR
  ~VarEl() { if (type == STRING) string_val.~string(); }

  template<class T> T get() const;

  // conversions
  explicit operator byte()   const { return coerce_numeric<byte>(); }
  explicit operator ubyte()  const { return coerce_numeric<ubyte>(); }
  explicit operator short()  const { return coerce_numeric<short>(); };
  explicit operator ushort() const { return coerce_numeric<ushort>(); }
  explicit operator int()    const { return coerce_numeric<int>(); }
  explicit operator uint()   const { return coerce_numeric<uint>(); }
  explicit operator long()   const { return coerce_numeric<long>(); }
  explicit operator ulong()  const { return coerce_numeric<ulong>(); }
  explicit operator float()  const { return coerce_numeric<float>(); }
  explicit operator double() const { return coerce_numeric<double>(); }
  explicit operator string () const {
    switch (type) {
     case STRING: return string_val.c_str();
     case FLOAT:  return std::to_string(float_val);
     case DOUBLE: return std::to_string(double_val);
     case ULONG:  return std::to_string(long_val);
     default: return std::to_string(coerce_numeric<long>());
    };
  }
  
 private:

  template<class T>
  T coerce_numeric() const {
    switch (type) {
     case STRING: throw JamException("Type is string; cannot convert to a numeric type");
     case NIL:    throw JamException("Type is NIL; cannot convert to a numeric type");
     case DOUBLE: return static_cast<T>(double_val);
     case INT:    return static_cast<T>(int_val);
     case BYTE:   return static_cast<T>(byte_val);
     case BOOL:   return static_cast<T>(bool_val);
     case UBYTE:  return static_cast<T>(ubyte_val);
     case SHORT:  return static_cast<T>(short_val);
     case USHORT: return static_cast<T>(ushort_val);
     case UINT:   return static_cast<T>(uint_val);
     case LONG:   return static_cast<T>(long_val);
     case ULONG:  return static_cast<T>(ulong_val);
     case FLOAT:  return static_cast<T>(float_val);
    }
  };

  void clear() {
    if (type == STRING) string_val.~string();
  }
  
  void copy(const VarEl& rhs) {
    type = rhs.type;
    switch (type) {
     case DOUBLE: double_val = rhs.double_val;
     case INT:    int_val    = rhs.int_val;
     case BYTE:   byte_val   = rhs.byte_val;
     case BOOL:   byte_val   = rhs.bool_val;
     case UBYTE:  ubyte_val  = rhs.ubyte_val;
     case SHORT:  short_val  = rhs.short_val;
     case USHORT: ushort_val = rhs.ushort_val;
     case UINT:   uint_val   = rhs.uint_val;
     case LONG:   long_val   = rhs.long_val;
     case ULONG:  ulong_val  = rhs.ulong_val;
     case FLOAT:  float_val  = rhs.float_val;
     case STRING: new (&string_val) string(rhs.string_val); break;
     case NIL:    nil_val    = NILVAL; break;
     default:
       throw JamException("Invalid VarEl type " + Type2String(type));
    }
  }

  void move(VarEl& rhs) {
    type = rhs.type;
    if (type == STRING)
      new (&string_val) string(std::move(rhs.string_val));
    else
      copy(rhs);
  }
  
};

static inline void throw_on_invalid_type (const VarEl* vel) {
  throw JamException("Invalid get request; variant element is of type " + Type2String(vel->type));
}

template<class T>
static inline T ve_get(const VarEl* ve) {
  throw JamException("Invalid type passed to get function");
}

#define VE_GET(T, VT, V) template<>             \
  inline T VarEl::get() const {                 \
    if (type == VT) return(V);                  \
    else throw_on_invalid_type(this);           \
  }                                             \

VE_GET(bool, BOOL, bool_val)
VE_GET(byte, BYTE, byte_val)
VE_GET(ubyte, UBYTE, ubyte_val)
VE_GET(short, SHORT, short_val)
VE_GET(ushort, USHORT, ushort_val)
VE_GET(int, INT, int_val)
VE_GET(uint, UINT, uint_val)
VE_GET(long, LONG, long_val)
VE_GET(ulong, ULONG, ulong_val)
VE_GET(float, FLOAT, float_val)
VE_GET(double, DOUBLE, double_val)
VE_GET(string, STRING, string_val)

/* template<class T> */
/* T VarEl::get() const { */
/*   return ve_get<T>(this); */
/* } */


/* ------------------------------------------------------ */
/* VARIADIC COLLECTION TYPE                               */
/* ------------------------------------------------------ */

struct VarColl {

  Type coll_type;
  Type el_type;

  union {
    byte nil_val;
    int_vec int_vec_val;
    long_vec long_vec_val;
    dbl_vec dbl_vec_val;
    str_vec str_vec_val;
    int_map int_map_val;
    long_map long_map_val;
    dbl_map dbl_map_val;
    str_map str_map_val;
  };

  // DEFAULT CONSTRUCTOR

  VarColl() : coll_type(NIL), el_type(NIL), nil_val(NILVAL) {
    PRINTALOC("constr (empty)\n");
  }

  VarColl(const Type& coll_type, const Type& el_type) : coll_type(coll_type), el_type(el_type) {
    PRINTALOC("constr (col,el)\n");
    init();
  }
  
  // COPY CONSTRUCTORS

  VarColl(const int_vec& val) : coll_type(VECTOR), el_type(INT), int_vec_val(val) {
    PRINTALOC("copy (int)\n");
  }
  
  VarColl(const int_map& val)  : coll_type(MAP),    el_type(INT), int_map_val(val) {}
  VarColl(const long_vec& val) : coll_type(VECTOR), el_type(LONG), long_vec_val(val) {}
  VarColl(const long_map& val) : coll_type(MAP),    el_type(LONG), long_map_val(val) {}
  VarColl(const dbl_vec& val)  : coll_type(VECTOR), el_type(DOUBLE), dbl_vec_val(val) {}
  VarColl(const dbl_map& val)  : coll_type(MAP),    el_type(DOUBLE), dbl_map_val(val) {}
  VarColl(const str_vec& val)  : coll_type(VECTOR), el_type(STRING), str_vec_val(val) {};
  VarColl(const str_map& val)  : coll_type(MAP),    el_type(STRING), str_map_val(val) {};

  VarColl(const VarColl& rhs) {
    PRINTALOC("copy (VarColl)\n");
    copy(rhs);
  }
  
  // MOVE CONSTRUCTORS

  VarColl(int_vec&& val)  : coll_type(VECTOR), el_type(INT),    int_vec_val(std::move(val)) {
    PRINTALOC("move (int)\n");
  }
  VarColl(int_map&& val)  : coll_type(MAP),    el_type(INT),    int_map_val(std::move(val)) {}
  VarColl(long_vec&& val) : coll_type(VECTOR), el_type(LONG),   long_vec_val(std::move(val)) {}
  VarColl(long_map&& val) : coll_type(MAP),    el_type(LONG),   long_map_val(std::move(val)) {}
  VarColl(dbl_vec&& val)  : coll_type(VECTOR), el_type(DOUBLE), dbl_vec_val(std::move(val)) {}
  VarColl(dbl_map&& val)  : coll_type(MAP),    el_type(DOUBLE), dbl_map_val(std::move(val)) {}
  VarColl(str_vec&& val)  : coll_type(VECTOR), el_type(STRING), str_vec_val(std::move(val)) {};
  VarColl(str_map&& val)  : coll_type(MAP),    el_type(STRING), str_map_val(std::move(val)) {};

  VarColl(VarColl&& rhs) {
    PRINTALOC("move (VarColl)\n");
    move(rhs);
  }

  // OPERATOR=
  // copy
  VarColl& operator=(const VarColl& rhs) {
    if (this != &rhs) {
      PRINTALOC("op=(copy)\n");
      clear();
      copy(rhs);
    }
    return *this;
  }
  // move
  VarColl& operator=(VarColl&& rhs) {
    if (this != &rhs) {
      PRINTALOC("op=(move)\n");
      clear();
      move(rhs);
    }
    return *this;
  }

  // DESTRUCTOR
  ~VarColl() {
    PRINTALOC("clear\n");
    clear();
  }

  // UTILS

  VarColl subset(size_t first, size_t last) const {
    switch (coll_type) {
     case VECTOR:
       switch (el_type) {
        case INT    : return int_vec(int_vec_val.begin() + first, int_vec_val.begin() + last);
        case DOUBLE : return dbl_vec(dbl_vec_val.begin() + first, dbl_vec_val.begin() + last);
        case STRING : return str_vec(str_vec_val.begin() + first, str_vec_val.begin() + last);
        default:
          throw JamException("Unsupported el type for subsetting: " + Type2String(el_type));
       }
       break;
     default:
       throw JamException("Subset of variadic map is not implemented yet");
    }    
  }

  // TEMPLATED UTILS
  template<class T> T& get();
  template<class T> void push_back(const T& val);

  // ARCHIVE

  template<class Archive>
  void serialize_common(Archive & archive) {

  }

  void serialize_init(cereal::BinaryOutputArchive& archive) {
    archive(coll_type, el_type);
  }

  void serialize_init(cereal::BinaryInputArchive& archive) {
    clear();
    archive(coll_type, el_type);
    init();
  }

  template<class Archive>
  void serialize(Archive & archive) {
    
    serialize_init(archive);
    
    switch (coll_type) {
     case VECTOR:
       switch (el_type) {
        case INT    : archive(int_vec_val); break;
        case DOUBLE : archive(dbl_vec_val); break;
        case STRING : archive(str_vec_val); break;
        default:
          throw JamException("Unsupported el type in writing VECTOR: " + Type2String(el_type));
       }
       break;
     case MAP:
       switch (el_type) {
        case INT    : archive(int_map_val); break;
        case DOUBLE : archive(dbl_map_val); break;
        case STRING : archive(str_map_val); break;
        default:
          throw JamException("Unsupported el type in writing MAP: " + Type2String(el_type));
       }
       break;
     case NIL: break;
     default:
       throw JamException("Invalid coll type during reading: " + Type2String(coll_type));
    }
  }

  size_t size() const {
    switch (coll_type) {
     case MAP:
       switch (el_type) {
        case STRING: return str_map_val.size();
        case INT:    return int_map_val.size();
        case LONG:   return long_map_val.size();
        case DOUBLE: return dbl_map_val.size();
       }
       break;
     case VECTOR:
       switch (el_type) {
        case STRING: return str_vec_val.size();
        case INT:    return int_vec_val.size();
        case LONG:   return long_vec_val.size();
        case DOUBLE: return dbl_vec_val.size();
       }
       break;
     case NIL: return 0;
    }    
  }
  
 private:
  
  void init() {
    switch (coll_type) {
     case MAP:
       switch (el_type) {
        case STRING: new (&str_map_val)  str_map(); break;
        case INT:    new (&int_map_val)  int_map(); break;
        case LONG:   new (&long_map_val) long_map(); break;
        case DOUBLE: new (&dbl_map_val)  dbl_map(); break;
       }
       break;
     case VECTOR:
       switch (el_type) {
        case STRING: new (&str_vec_val)  str_vec(); break;
        case INT:    new (&int_vec_val)  int_vec(); break;
        case LONG:   new (&long_vec_val) long_vec(); break;
        case DOUBLE: new (&dbl_vec_val)  dbl_vec(); break;
       }
       break;
     case NIL: nil_val = NILVAL; break;
    }
  }
  
  void clear() {
    switch (coll_type) {
     case MAP:
       switch (el_type) {
        case STRING: str_map_val.~str_map(); break;
        case INT:    int_map_val.~int_map(); break;
        case LONG:   long_map_val.~long_map(); break;
        case DOUBLE: dbl_map_val.~dbl_map(); break;
       }
       break;
     case VECTOR:
       switch (el_type) {
        case STRING: str_vec_val.~str_vec(); break;
        case INT:    int_vec_val.~int_vec(); break;
        case LONG:   long_vec_val.~long_vec(); break;
        case DOUBLE: dbl_vec_val.~dbl_vec(); break;
       }
       break;
    }
  }

  void copy(const VarColl& rhs) {
    el_type = rhs.el_type;
    coll_type = rhs.coll_type;
    
    switch (coll_type) {
     case MAP:
       switch (el_type) {
        case STRING: new (&str_map_val) str_map(rhs.str_map_val); break;
        case INT: new (&int_map_val)    int_map(rhs.int_map_val); break;
        case LONG: new (&long_map_val)  long_map(rhs.long_map_val); break;
        case DOUBLE: new (&dbl_map_val) dbl_map(rhs.dbl_map_val); break;
       }
       break;
     case VECTOR:
       switch (el_type) {
        case STRING: new (&str_vec_val) str_vec(rhs.str_vec_val); break;
        case INT: new (&int_vec_val)    int_vec(rhs.int_vec_val); break;
        case LONG: new (&long_vec_val)  long_vec(rhs.long_vec_val); break;
        case DOUBLE: new (&dbl_vec_val) dbl_vec(rhs.dbl_vec_val); break;
       }
       break;
     case NIL: nil_val = NILVAL; break;
    }
  }

  void move(VarColl& rhs) {
    PRINTALOC("move helper (VarColl)\n");
    el_type = rhs.el_type;
    coll_type = rhs.coll_type;
    
    switch (coll_type) {
     case MAP:
       switch (el_type) {
        case STRING: new (&str_map_val)  str_map(std::move(rhs.str_map_val)); break;
        case INT:    new (&int_map_val)  int_map(std::move(rhs.int_map_val)); break;
        case LONG:   new (&long_map_val) long_map(std::move(rhs.long_map_val)); break;
        case DOUBLE: new (&dbl_map_val)  dbl_map(std::move(rhs.dbl_map_val)); break;
       }
       break;
     case VECTOR:
       switch (el_type) {
        case STRING: new (&str_vec_val)  str_vec(std::move(rhs.str_vec_val)); break;
        case INT:    new (&int_vec_val)  int_vec(std::move(rhs.int_vec_val)); break;
        case LONG:   new (&long_vec_val) long_vec(std::move(rhs.long_vec_val)); break;
        case DOUBLE: new (&dbl_vec_val)  dbl_vec(std::move(rhs.dbl_vec_val)); break;
       }
       break;
     case NIL: nil_val = NILVAL; break;
    }
  }
  
};


static inline void throw_on_invalid_type (const VarColl* vc, const string& type = "get") {
  throw JamException("Invalid " + type + " request; variant element is " +
                     Type2String(vc->coll_type) + " of type " + Type2String(vc->el_type));
}

//// GET

template<class T>
static inline T vc_get(VarColl* vc) {
  throw JamException("Invalid type passed to get function");
}

#define VC_GET(T, CT, ET, V) template<>             \
  inline T& VarColl::get() {                        \
    if (el_type == ET && coll_type == CT)           \
      return(V);                                    \
    else throw_on_invalid_type(this, "get");        \
  }                                                 \
 
VC_GET(int_vec,  VECTOR, INT,     int_vec_val)
VC_GET(long_vec, VECTOR, LONG,    long_vec_val)
VC_GET(dbl_vec,  VECTOR, DOUBLE,  dbl_vec_val)
VC_GET(str_vec,  VECTOR, STRING,  str_vec_val)
VC_GET(int_map,  MAP,    INT,     int_map_val)
VC_GET(long_map, MAP,    LONG,    long_map_val)
VC_GET(dbl_map,  MAP,    DOUBLE,  dbl_map_val)
VC_GET(str_map,  MAP,    STRING,  str_map_val)

/* template<class T> */
/* T& VarColl::get() { */
/*   return vc_get<T>(this); */
/* } */


//// PUSH BACK

template<class T>
static inline void vc_push_back(VarColl* vc, const T& val) {
  throw JamException("Unsupported type passed to push_back function");
}

#define VC_PUSH_BACK(T, ET, V) template<>               \
  inline void VarColl::push_back(const T& val) {        \
    if (el_type == ET && coll_type == VECTOR)           \
      V.push_back(val);                                 \
    else                                                \
      throw_on_invalid_type(this, "push_back");         \
  }                                                     \

VC_PUSH_BACK(int,     INT,     int_vec_val)
VC_PUSH_BACK(long,    LONG,    long_vec_val)
VC_PUSH_BACK(double,  DOUBLE,  dbl_vec_val)
VC_PUSH_BACK(string,  STRING,  str_vec_val)

template<class T>
inline void VarColl::push_back(const T& val) {
  return vc_push_back(this, val);
}


/* ------------------------------------------------------ */
/* READER HEAD                                            */
/* ------------------------------------------------------ */

class Head {

  ubyte version = 0;
  ubyte extra = 0;

 public:

  Type coll_type = UNDEFINED;
  Type el_type = UNDEFINED;

  Head(){};
  Head(Type coll_type, Type el_type, bool has_meta = false) :
    el_type(el_type),
    coll_type(coll_type),
    extra((has_meta ? 1 : 0))
  {};

  bool metabit () const {
    return extra & 1;
  }

  void metabit (const bool bit) {
    if (bit) extra |= 1;
    else extra &= ~(1);
  }

  bool contbit () const {
    return extra & (1 << 4);
  }

  void contbit (const bool bit) {
    if (bit) extra |= (1 << 4);
    else extra &= ~(1 << 4);
  }

  void print () const {
    print("");
  }
  
  void print (std::string name) const {
    std::cout << name
              << " coll_type:" << Type2String(coll_type)
              << " el_type:" << Type2String(el_type)
              << " meta:" << (metabit() ? "true" : "false") << std::endl;
  }
  
  template<class Archive>
  void serialize(Archive & archive)
  {
    archive(coll_type, el_type, version, extra);
  }
};


const Head JAM_META_HEAD = Head(jam::META, jam::MIXED, true);
const Head JAM_NIL_HEAD = Head(jam::NIL, jam::UNDEFINED, false);


/* ------------------------------------------------------ */
/* UTILITIES                                              */
/* ------------------------------------------------------ */

inline vector<Head> heads_from_columns(vector<VarColl> cols) {
  vector<Head> out;
  for (const auto& col : cols) {
    out.push_back(Head(col.coll_type, col.el_type));
  }
  return out;
}

inline vector<Type> types_from_columns(vector<VarColl> cols) {
  vector<Type> out(cols.size());
  for (size_t i = 0; i < cols.size(); i++)
    out[i] = cols[i].el_type;
  return out;
}

inline vector<Type> types_from_heads(vector<Head> heads) {
  vector<Type> out(heads.size());
  for (size_t i = 0; i < heads.size(); i++)
    out[i] = heads[i].el_type;
  return out;
}


/* ------------------------------------------------------ */
/* READER                                                 */
/* ------------------------------------------------------ */

typedef cereal::BinaryInputArchive BIN;

class Reader {

  bool fetched_header_ = false;
  bool fetched_base_header_ = false;
  size_t nrows_;
  
  std::ifstream istream;
  BIN bin_;

 public:

  const string path;
  Head head; 
  strmap<VarColl> meta;
  vector<strmap<VarColl>> col_metas;
  vector<VarColl> columns;

  Reader(const string& path) : path(path), istream(std::ifstream(path, std::ios::binary)), bin_(istream) {
    // waf? this doesn't work.
    istream.exceptions(std::ifstream::failbit|std::ifstream::badbit|std::ifstream::eofbit);
  };
  /* Reader(std::istream& istream) : path(""), bin_(istream) {}; */

  str_vec names() {
    if (!fetched_base_header_)
      throw JamException("Header hasn't been fetched yet");
    return meta["names"].get<str_vec>();
  }
  
  size_t ncols() const {
    return col_metas.size();
  }

  size_t nrows() const {
    return nrows_;
  }

  vector<Type> coll_types() {
    vector<Type> out;
    for (const auto& c : columns) {
      out.push_back(c.coll_type);
    }
    return out;
  }

  vector<Type> el_types() {
    vector<Type> out;
    for (const auto& c : columns) {
      out.push_back(c.el_type);
    }
    return out;
  }
  
  Reader& fetch_header () {
    bin_(head);
    if (head.coll_type != DF)
      throw JamException("Can read only objects of type DF. Found " + Type2String(head.coll_type));
    if (head.contbit()) {
      if (!fetched_base_header_)
        throw JamException("Continuation header encountered, but no base header has been fetched yet");
    } else {
      bin_(meta, col_metas);
      fetched_base_header_ = true;
    }
    fetched_header_ = true;
    return *this;
  }

  // return empty vector if cannot read;
  vector<VarColl>& read_columns(size_t nchunks = MAX_SIZE) {

    if (nchunks <= 0)
      nchunks = MAX_SIZE;

    columns = vector<VarColl>();

    try {

      if (!fetched_header_)
        fetch_header();
      fetched_header_ = false;
    
      PRINT("started reading (nchunks %ld)\n", nchunks);
      bin_(columns);
      
    } catch (std::exception&) {
      columns = vector<VarColl>();
      nrows_ = 0;
      return columns;
    }
    size_t chunks = 1; 
    
    // cereal throws on end of input; need a double catch here
    bool keep_reading = true;
    while (keep_reading && chunks < nchunks) {
      try {
        fetch_header();
        if (!head.contbit()) {
          // fixme: compatible chunks should be handled
          throw JamException("Heterogeneous chunks cannot be bound at the moment. Try non binding option instead.");
        }
        vector<VarColl> next; bin_(next);
        for (size_t c = 0; c < next.size(); c++) {
          check_col_type(columns[c], next[c], c);
          switch (next[c].el_type) {
           case INT:    columns[c].int_vec_val.insert(columns[c].int_vec_val.end(), next[c].int_vec_val.begin(), next[c].int_vec_val.end()); break;
           case DOUBLE: columns[c].dbl_vec_val.insert(columns[c].dbl_vec_val.end(), next[c].dbl_vec_val.begin(), next[c].dbl_vec_val.end()); break;
           case STRING: columns[c].str_vec_val.insert(columns[c].str_vec_val.end(), next[c].str_vec_val.begin(), next[c].str_vec_val.end()); break;
           default:
             throw JamException("Should never end up here; please report");
          }
        }
        chunks++;
        // fixme: throwing on eof doesn't work for unclear reason: http://stackoverflow.com/a/11808139/453735
        // } catch (std::ios_base::failure fail) { keep_reading = false; };
      } catch (JamException& e) {
        throw e;
      } catch (std::exception& e) {
        keep_reading = false;
      };
    }

    PRINT("done reading %ld chunks\n", chunks);
    nrows_ = columns[0].size();
    return columns;
  }

  vector<vector<VarColl>> read_columns_nobind(size_t nchunks = MAX_SIZE) {

    vector<vector<VarColl>> out;
    vector<VarColl> v;

    size_t chunks = 0;
    bool keep_reading = true;
    
    while (keep_reading && chunks < nchunks){
      v = read_columns(1);
      chunks++;
      if (v.size() == 0)
        keep_reading = false;
      else
        out.push_back(v);
    }

    return out;
  }

  vector<VarEl> read_line() {
    if (next_row_ >= nrows()) {
      read_columns(1);
      next_row_ = 0;
      if (nrows()==0) throw JamException("0 rows fetched; aborting");
    }
    
    size_t nc = ncols();
    vector<VarEl> out(nc);
    
    for (size_t i = 0; i < nc; i++) {
      switch(columns[i].el_type) {
       case INT:    out[i] = VarEl(columns[i].int_vec_val[next_row_]); break;
       case DOUBLE: out[i] = VarEl(columns[i].dbl_vec_val[next_row_]); break;
       case STRING: out[i] = VarEl(columns[i].str_vec_val[next_row_]); break;
       default:
         throw JamException("Unsupported type (should never end up here, please report)");
      }
    }
        
  }

 private:

  size_t next_row_ = 0;
  
  void check_col_type(const VarColl& old_col, const VarColl& new_col, size_t c) {
    if (old_col.el_type != new_col.el_type) {
      throw JamException("Column " + std::to_string(c) + " type (" + Type2String(new_col.el_type) + ") doesn't match old type (" + Type2String(old_col.el_type) + ")");
    }
  }
  /* // from http://stackoverflow.com/a/24868211/453735 */
  /* template <typename T> constexpr T& as_lvalue(T&& t) { return t; }; */
  
};



/* ------------------------------------------------------ */
/* WRITER                                                 */
/* ------------------------------------------------------ */

typedef cereal::BinaryOutputArchive BOUT;

class Writer {
  
  std::ofstream ostream_;
  BOUT bout_;

 public:

  string path;
  const Head head = Head(DF, MIXED, true);
  strmap<VarColl> meta;
  vector<strmap<VarColl>> col_metas;

  // CONSTRUCTORS
  Writer(const string& path, bool append = false) :
    Writer(path, strmap<VarColl>(), vector<strmap<VarColl>>(), append) {}
  
  Writer(const string& path, strmap<VarColl> meta, size_t ncols) :
    Writer(path, meta, vector<strmap<VarColl>>(ncols)) {}

  Writer(const Reader& reader) :
    Writer(reader.path, reader.meta, reader.col_metas) {}

  Writer(const string& path, strmap<VarColl> meta, vector<strmap<VarColl>> col_metas, bool append = false) :
    path(path),
    ostream_(std::ofstream(path, append ? (std::ios::binary | std::ios::app) : std::ios::binary)),
    bout_(ostream_),
    meta(meta),
    col_metas(col_metas){};

  // UTILS
  
  size_t ncols() {
    return col_metas.size();
  }

  Writer& fetch_header() {
    if (path == "")
      throw JamException("Path wasn't initialized");
    return fetch_header(path);
  }
  
  Writer& fetch_header(string path) {
    Reader reader(path);
    PRINT("fetching header from '%s' ...\n", path.c_str());
    reader.fetch_header();
    PRINT("done ...\n");
    meta = reader.meta;
    col_metas = reader.col_metas;
    PRINT("meta size: %ld", meta.size());
    return *this;
  }
  
  // WRITERS
    
  Writer& write_header (bool continuation = false) {
    if (ncols() == 0)
      throw JamException("Attempting to write a table with 0 columns");
    if (continuation) {
      Head cont_head(head);
      cont_head.contbit(true);
      bout_(cont_head);
    } else {
      bout_(head);
      bout_(meta, col_metas);
    }
    return *this;
  }

  Writer& write_columns(const vector<VarColl>& cols, size_t rows_per_chunk = MAX_SIZE, bool continuation = false) {

    if (meta.find("names") == meta.end()) {
      throw std::invalid_argument("Meta must contain a vector of column names named 'names'");
    }
    
    if (cols.size() != ncols())
      throw JamException("Writer's number of columns (" + std::to_string(ncols()) + ") not equals number of supplied columns (" + std::to_string(cols.size()) + ")");
    
    size_t nrows =  cols[0].size();
    for (const auto& c : cols) {
      if (c.size() != nrows)
        throw JamException("All columns must have same length");
    }

    size_t chunks = 0;

    write_header(continuation);

    if (rows_per_chunk >= nrows) {
      bout_(cols);
      chunks++;
    } else {
      size_t first = 0, last = rows_per_chunk;
      do {
        if (chunks > 0)
          write_header(true);
        vector<VarColl> subcols;
        for (const auto& c : cols) {
          subcols.push_back(c.subset(first, last));
        }
        bout_(subcols);
        first = last;
        last = std::min(last + rows_per_chunk, nrows);
        chunks++;
      } while (first < nrows);
    }
    
    PRINT("wrote %ld chunks\n", chunks);
    return *this;
  }
  
};
}

#endif
