
#ifndef COLLECTIONS_CASEINSENSITIVE_MAP
#define COLLECTIONS_CASEINSENSITIVE_MAP

#include <cctype>
#include <map>

class ignorecase_comparator
    : public std::binary_function<const std::string &, const std::string &,
                                  bool> {
public:
  bool operator()(const std::string &str1, const std::string &str2) const {
    std::string::size_type max =
        str1.length() < str2.length() ? str1.length() : str2.length();
    for (unsigned int i = 0; i < max; i++) {
      int lower1 = tolower(str1[i]);
      int lower2 = tolower(str2[i]);
      if (lower1 != lower2) {
        return lower1 > lower2;
      }
    }
    return str1.length() > str2.length();
  }
};

/**
 * A map where the keys (which are string) are compared case-insensitively
 */
template <typename V>
class caseinsensitive_map
    : public std::map<std::string, V, ignorecase_comparator> {
public:
  caseinsensitive_map(){};
};

#endif
