/* boxjson.cpp */

#include "json.hpp"
#include <string>
#include <regex>
#include <vector>
#include <algorithm>
#include "utils.cpp"
#include <iostream>

// Checks if JSON contains unboxed atoms
bool hasUnboxedAtom(const std::string& json, const bool strict=true) {
  if (json.empty()) exit(1);
  const std::string mutant = mutateInputJSON(json);
  if (strict && !nlohmann::json::accept(json)) exit(1);
  if (isArray(json)) {
    return false;
  } else if (isObject(json)) {
    std::vector<std::string> cpl = splitOnUnclosedChar(stripObject(json), ',');
    std::cout << cpl.size() << std::endl;
    /*
     cpl <- splitOnUnclosedChar(stripObject(json), ',')
     spl <- unlist(lapply(cpl, splitOnUnclosedChar, char=':', keep=TRUE))
     pre <- NA
     for (chunk in spl) {
       if (identical(pre, ':') && (!isStruct(chunk) || hasUnboxedAtom(chunk))) {
         return(TRUE)
       }
       pre <- chunk
     }
    return(FALSE)
    */
  } else {
    return true;
  }
}