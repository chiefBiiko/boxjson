#include <iostream>
#include "json.hpp"

// 4 cli usage
int main(const int argc, const char* argv[]) {
  std::cout << nlohmann::json::accept(&argv[1][0]);
  return 0;
}

// 4 src
bool yali(const char* input) {
  return nlohmann::json::accept(&input[0]);
}