#include "json.hpp"
#include <string>
#include <regex>

// json validation: nlohmann::json::accept(std::string x)
// check 4 truthiness: std::string::empty(std::string s)

// Is json a (non-empty) array(-like)?
bool isArray(const std::string json) {
  return std::regex_match(json, std::regex("\\[.+\\]"));
}

// Is json a (non-empty) object(-like)?
bool isObject(const std::string json) {
  return std::regex_match(json, std::regex("\\{.+\\}"));
}

// Is json a (non-empty) array(-like) or object(-like)?
bool isStruct(const std::string json) {
  return isArray(json) || isObject(json);
}
