
#ifndef SCHEME_FILENAMES_H
#define SCHEME_FILENAMES_H

#include <string>
#include <wchar.h>

/**
 * Conversion from wstrings to and from filenames on the local system.
 */
struct SchemeFilenames {
    static std::string toFilename(std::wstring str);
    static std::wstring toString(std::string filename);
};


#endif
