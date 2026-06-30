
#include "filenames.h"
#include <cstdlib>
#include <vector>

std::string SchemeFilenames::toFilename(std::wstring str) {
  size_t length = str.size();
  std::vector<char> cstring(length * 5 + 1);
  const wchar_t *s = str.c_str();
  size_t size = ::wcsrtombs(cstring.data(), &s, 5 * length + 1, NULL);
  cstring[size] = 0;
  return std::string(cstring.data());
}

std::wstring SchemeFilenames::toString(std::string s) {
  size_t length = s.size();
  std::vector<wchar_t> wcstring(length + 1);
  ::mbstowcs(wcstring.data(), s.c_str(), length + 1);
  return std::wstring(wcstring.data());
}
