#include "json.hpp"
#include <string>
#include <regex>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <algorithm>
#include <vector>

// json validation: nlohmann::json::accept(std::string x)
// check 4 truthiness: std::string::empty

// Is given file accessible?
bool fileExists(const std::string filename) {
  std::ifstream file(filename.c_str());
  if (file.is_open()) {
    file.close();
    return true;
  } else {
    return false;
  }
}

// Read in an ASCII file into a std::string
std::string readInFile(const std::string filename) {
  std::ifstream instream(filename);
  std::stringstream buffer;
  buffer << instream.rdbuf();
  return buffer.str();
}

// Is x a (non-empty) array(-like)?
bool isArray(const std::string x) {
  return std::regex_match(x, std::regex("\\[.+\\]"));
}

// Is x a (non-empty) object(-like)?
bool isObject(const std::string x) {
  return std::regex_match(x, std::regex("\\{.+\\}"));
}

// Is x a (non-empty) array(-like) or object(-like)?
bool isStruct(const std::string x) {
  return isArray(x) || isObject(x);
}

// Strips an array(-like)
std::string stripArray(const std::string x) {
  return isArray(x) ? 
    std::regex_replace(x, std::regex("^\\[|\\]$"), std::string("")) : x;
}

// Strips an object(-like)
std::string stripObject(const std::string x) {
  return isObject(x) ?
    std::regex_replace(x, std::regex("^\\{|\\}$"), std::string("")) : x;
}

// Replaces whitespace not enclosed in double quotes
std::string mutateInputJSON(const std::string json) {
  std::string input, mutant;
  input = fileExists(json) ? readInFile(json) : json;
  mutant = regex_replace(input, 
                         std::regex("\\s+(?=(?:(?:[^\"]*\"){2})*[^\"]*$)"), 
                         std::string(""));
  return mutant;
}

bool allMapValuesEven(std::unordered_map<char, int> imap) {
  std::vector<int> vals;
  vals.reserve(imap.size());
  for (auto& kv : imap) vals.push_back(kv.second);
  return std::all_of(vals.begin(), vals.end(), 
                     [](int i){ return i % 2 == 0; });
}

// Checks whether search contains character neither enclosed in brackets nor
// double quotes
// unreliable if counts of brackets and double quotes are not even !!!
bool hasUnclosedChar(const std::string search, const char character) {
  int index = 0, opbr = 0, opqt = 2;
  std::unordered_map<char, int> nsqt;
  nsqt['z'] = 2;
  const std::string OPEN = "[{", CLOSE = "]}", ESCS = "\\";
  const char QUOTE = '"';
  std::string::const_iterator it = search.begin();
  for (it; it < search.end(); it++, index++) {
    if (std::binary_search(OPEN.begin(), OPEN.end(), *it)) opbr++;
    if (std::binary_search(CLOSE.begin(), CLOSE.end(), *it)) opbr--;
    if (*it == QUOTE) opqt++;
    if (std::binary_search(ESCS.begin(), ESCS.end(), *it) && 
        search[index + 1] == QUOTE) {
      std::vector<char> keys;
      keys.reserve(nsqt.size());
      for (auto& kv : nsqt) keys.push_back(kv.first);
      if (!std::binary_search(keys.begin(), keys.end(), *it)) {
        nsqt[*it] = 2 + 1;  // std::string(*it)
      } else {
        nsqt[*it]++;
      }
    }
    // false negative if total bracket/double quote occurrences are not even !
    if (*it == character && 
        (opbr == 0 && opqt % 2 == 0 && allMapValuesEven(nsqt))) {
      return true;
    }
  }
  return false;
}
