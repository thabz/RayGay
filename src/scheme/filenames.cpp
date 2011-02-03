
#include "filenames.h"
#include <cstdlib>

std::string SchemeFilenames::toFilename(std::wstring str) {
    size_t length = str.size();        
    char cstring[length*5+1];
    const wchar_t* s = str.c_str(); 
    size_t size = ::wcsrtombs(cstring, &s, 5*length+1, NULL);
    cstring[size] = 0;
    return std::string(cstring);
}

std::wstring SchemeFilenames::toString(std::string s) {
    size_t length = s.size();        
    wchar_t wcstring[length+1];
    ::mbstowcs(wcstring, s.c_str(), length+1);
    return std::wstring(wcstring);        
}
