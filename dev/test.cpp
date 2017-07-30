#include <iostream>
#include "boxjson.cpp"
//#include "utils.cpp"
/*
#include <string>
#include <vector>
#include <algorithm>
#include "utils.cpp"
*/
 
int main(const int argc, const char* argv[]) {
  const std::string input(argv[1]);
  
  /*
//const char delimiter(argv[2][0]);
  const char delimiter = ',';
//std::cout << hasUnclosedChar(input, delimiter) << std::endl;
  std::vector<std::string> pieces{splitOnUnclosedChar(input, delimiter)};
  std::for_each(pieces.begin(), pieces.end(), [](const std::string& s) {
    std::cout << s << std::endl;
  });
  */
  
  std::cout << hasUnboxedAtom(input) << std::endl;
  
  return 1;
}
