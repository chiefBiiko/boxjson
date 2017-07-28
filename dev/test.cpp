#include <iostream>
#include <string>
#include "utils.cpp"

int main(const int argc, const char* argv[]) {
  std::string input(argv[1]);
  std::cout << hasUnclosedChar(input, ',') << std::endl;
  return 0;
}
