#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include "utils.cpp"

//std::vector<std::string> pieces = splitOnUnclosedChar(std::string("h,w"), ',');

int main(const int argc, const char* argv[]) {
  const std::string input(argv[1]);
//const char delimiter(argv[2][0]);
  const char delimiter = ',';
  
  //std::cout << hasUnclosedChar(input, delimiter) << std::endl;
  std::vector<std::string> pieces{splitOnUnclosedChar(input, delimiter)};
//std::cout << pieces[0] << '\n' << pieces[1] << std::endl;
  std::for_each(pieces.begin(), pieces.end(), [](const std::string& s) {
    std::cout << s << std::endl;
  });
  
  return 1;
}
