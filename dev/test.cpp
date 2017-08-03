#include <iostream>
#include <string>
#include <regex>
//#include "boxjson.cpp"
//#include "utils_dev.cpp"
/*
#include <string>
#include <vector>
#include <algorithm>
#include "utils.cpp"
*/

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

int main(const int argc, const char* argv[]) {
  const std::string input = "hello,world\"";
  
  /*
//const char delimiter(argv[2][0]);
  const char delimiter = ',';
//std::cout << hasUnclosedChar(input, delimiter) << std::endl;
  std::vector<std::string> pieces{splitOnUnclosedChar(input, delimiter)};
  std::for_each(pieces.begin(), pieces.end(), [](const std::string& s) {
    std::cout << s << std::endl;
  });
  */
  
//std::cout << hasUnboxedAtom(input) << std::endl;
  const int index = 1;
  std::cout << charIndexIsFollowedByUnevenNumberOfOuterDoubleQuotes(input, index) << std::endl;
  
  return 1;
}
