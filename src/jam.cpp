// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(Rcereal)]]

#include "types.hpp"
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

void stop_on_invalid_type(SEXP x, const JamType::type& jtype) {
  stop("Cannot jam object of R type %s into JamType %s",
       Rf_type2char(TYPEOF(x)), jamTypeToString(jtype));
}

template<typename TIN, typename TOUT>
void jam_vector (TIN* x, const size_t& N, const std::string& path, JamType::type& jtype) {
  {
    std::ofstream fout(path, std::ios::binary);
    cereal::BinaryOutputArchive bout(fout);
    bout(JamHead(JamCollType::VECTOR, jtype));
    const std::vector<TOUT> ovec(x, x + N);
    bout(ovec);
    fout.close();
  }
  // // for test
  // {
  //   std::ifstream fin(path, std::ios::binary);
  //   cereal::BinaryInputArchive bin(fin);
  //   bin(JamHead());
  //   std::vector<TOUT> ivec;
  //   bin(ivec);
  //   cereal::JSONOutputArchive ojson(std::cout);
  //   ojson(CEREAL_NVP(ivec));
  //   fin.close();
  // }
}

// [[Rcpp::export]]
void c_jam_vector(SEXP x, const int& jam_type, const std::string& path) {

  size_t N = XLENGTH(x);
  JamType::type jtype = (JamType::type) jam_type;
  
  switch(TYPEOF(x)) {
   case LGLSXP:
     switch(jtype) {
      case JamType::BYTE:
        jam_vector<int,byte>(INTEGER(x), N, path, jtype);
        break;
      case JamType::UBYTE:
        jam_vector<int,ubyte>(INTEGER(x), N, path, jtype);
        break;
        // case JamType::BITSET:
        //   jam_lgl_to_bitset(INTEGER(x), N, path);
        //   break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
   case INTSXP:
     switch(jtype) {
      case JamType::BYTE:
        jam_vector<int,byte>(INTEGER(x), N, path, jtype);
        break;
      case JamType::UBYTE:
        jam_vector<int,ubyte>(INTEGER(x), N, path, jtype);
        break;
      case JamType::SHORT:
        jam_vector<int,short>(INTEGER(x), N, path, jtype);
        break;
      case JamType::USHORT:
        jam_vector<int,ushort>(INTEGER(x), N, path, jtype);
        break;
      case JamType::INT:
        jam_vector<int,int>(INTEGER(x), N, path, jtype);
        break;
      case JamType::UINT:
        jam_vector<int,uint>(INTEGER(x), N, path, jtype);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     };
     break;
   case REALSXP:
     switch(jtype) {
      case JamType::FLOAT:
        jam_vector<double,float>(REAL(x), N, path, jtype);
        break;
      case JamType::DOUBLE:
        jam_vector<double,double>(REAL(x), N, path, jtype);
        break;
      default:
        stop_on_invalid_type(x, jtype);
     }
     // jam_vector<double,double>(REAL(x), N, path);
     break;
   case STRSXP:
   default:
     stop("Cannot jam object of type %s", Rf_type2char(TYPEOF(x)));
  }
}

