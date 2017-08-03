/* utils.cpp */

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
bool fileExists(const std::string& filename) {
  std::ifstream file(filename.c_str());
  if (file.is_open()) {
    file.close();
    return true;
  } else {
    return false;
  }
}

// Read in an ASCII file into a std::string
std::string readInFile(const std::string& filename) {
  if (!fileExists(filename)) exit(1);
  std::ifstream instream(filename);
  std::stringstream buffer;
  buffer << instream.rdbuf();
  return buffer.str();
}

// Is x a (non-empty) array(-like)?
inline bool isArray(const std::string& x) {
  return std::regex_match(x, std::regex("\\[.+\\]"));
}

// Is x a (non-empty) object(-like)?
inline bool isObject(const std::string& x) {
  return std::regex_match(x, std::regex("\\{.+\\}"));
}

// Is x a (non-empty) array(-like) or object(-like)?
inline bool isStruct(const std::string& x) {
  return isArray(x) || isObject(x);
}

// Strips an array(-like)
std::string stripArray(const std::string& x) {
  return isArray(x) ? 
    std::regex_replace(x, std::regex("^\\[|\\]$"), std::string("")) : x;
}

// Strips an object(-like)
std::string stripObject(const std::string& x) {
  return isObject(x) ?
    std::regex_replace(x, std::regex("^\\{|\\}$"), std::string("")) : x;
}

// Replaces whitespace not enclosed in double quotes
std::string mutateInputJSON(const std::string& json) {
  std::string input, mutant;
  input = fileExists(json) ? readInFile(json) : json;
  mutant = regex_replace(input, 
                         std::regex("\\s+(?=(?:(?:[^\"]*\"){2})*[^\"]*$)"), 
                         std::string(""));
  return mutant;
}

// OBSOLETE !!!!!
// Are all values of hmap even?
bool allMapValuesEven(std::unordered_map<char, int>& hmap) {
  std::vector<int> vals;
  vals.reserve(hmap.size());
  for (auto& kv : hmap) vals.push_back(kv.second);
  return std::all_of(vals.begin(), vals.end(), 
                     [](int i){ return i % 2 == 0; });
}

bool charIndexIsFollowedByUnevenNumberOfOuterDoubleQuotes
  (const std::string& search, const int index) {
  const char QUOTE = '"';
  const char* ESCS = "\\\\+";
  int countr = 0, indexr = 0;
  std::string::const_iterator it;
  for (it = search.begin() + index + 1; it < search.end(); it++, indexr++) {
    if (*it == QUOTE && 
        !std::regex_match(&search[indexr - 1], std::regex(ESCS))) {
        countr++;
    }
  }
  return countr % 2 != 0;
}
 
/* Checks whether search contains character neither enclosed in brackets nor
 * double quotes
 * unreliable if counts of brackets and double quotes are not even or ...
 * search has heavily nested double quotes (depth  > 2)
 */
bool hasUnclosedChar(const std::string& search, const char character) {
  int index = 0, opbr = 0, opqt = 2;
  std::unordered_map<char, int> nsqt;
  nsqt['z'] = 2;
  const std::string OPEN = "[{", CLOSE = "]}", ESCS = "\\";
  const char QUOTE = '"';
  std::string::const_iterator it;
  for (it = search.begin(); it < search.end(); it++, index++) {
    if (std::binary_search(OPEN.begin(), OPEN.end(), *it)) opbr++;
    if (std::binary_search(CLOSE.begin(), CLOSE.end(), *it)) opbr--;
    if (*it == QUOTE) opqt++;
    if (std::binary_search(ESCS.begin(), ESCS.end(), *it) && 
        search[index + 1] == QUOTE) {
      std::vector<char> keys;
      keys.reserve(nsqt.size());
      for (auto& kv : nsqt) keys.push_back(kv.first);
      if (!std::binary_search(keys.begin(), keys.end(), *it)) {
        nsqt[*it] = 2 + 1;
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

/* Splits search on character neither enclosed in brackets nor
 * double quotes
 * unreliable if counts of brackets and double quotes are not even or ...
 * search has heavily nested double quotes (depth  > 2)
 */
std::vector<std::string> splitOnUnclosedChar(const std::string& search, 
                                             const char character, 
                                             const bool keep=false) {
  // setup ...
  std::vector<std::string> pieces;
  if (!std::regex_search(search, std::regex(&character))) {
    pieces.push_back(search);
    return pieces;  // early xit
  }
  std::unordered_map<char, int> nsqt;  // nested quotes
  nsqt['z'] = 2;
  int index = 0, opbr = 0, lastcut = -1, opqt = 2;
  const std::string OPEN = "[{", CLOSE = "]}", ESCS = "\\";
  const char QUOTE = '"';
  // peep thru
  std::string::const_iterator it;
  for (it = search.begin(); it < search.end(); it++, index++) {
    if (std::binary_search(OPEN.begin(), OPEN.end(), *it)) opbr++;
    if (std::binary_search(CLOSE.begin(), CLOSE.end(), *it)) opbr--;
    if (*it == QUOTE) opqt++;
    if (std::binary_search(ESCS.begin(), ESCS.end(), *it) && 
        search[index + 1] == QUOTE) {  // deeply nested " probly break tihs
      std::vector<char> keys;
      keys.reserve(nsqt.size());
      for (auto& kv : nsqt) keys.push_back(kv.first);
      if (!std::binary_search(keys.begin(), keys.end(), *it)) {
        nsqt[*it] = 2 + 1;
      } else {
        nsqt[*it]++;
      }
    }
    // false negative if total bracket/double quote occurrences are not even !
    if (*it == character && 
        (opbr == 0 && opqt % 2 == 0 && allMapValuesEven(nsqt))) {
      pieces.push_back(search.substr(lastcut + 1, index - lastcut - 1));
      if (keep) pieces.push_back(search.substr(index, 1));
      lastcut = index;
    }
  }
  // consume remainder !!!
  if (lastcut < search.size() - 1) {
      pieces.push_back(search.substr(lastcut + 1, search.size() - lastcut)); // -1?
  }
  return pieces;
}
