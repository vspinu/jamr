#ifndef __JAM_HPP__
#define __JAM_HPP__

#include "jamtypes.hpp"
#include "rtypes.hpp"
#include <cereal/cereal.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/vector.hpp>
#include <cereal/archives/binary.hpp>
// #include <cereal/archives/json.hpp>
// #include <cereal/types/map.hpp>
// #include <cereal/types/unordered_map.hpp>

void jam_vector(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head = true);
void jam_vector(cereal::BinaryOutputArchive& bout, SEXP x, bool with_head, const JamType& head);



#endif
