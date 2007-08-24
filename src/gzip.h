
#ifndef RAYGAY_GZIP
#define RAYGAY_GZIP

#include <string>
#include <fstream>

/**
 * RFC 1952 describes the simple fileformat.
 * RFC 1951 describes the deflate algorithm.
 */
class GZIP {
    public:
        static void dump_info(std::string filename);
	static void deflate(std::ifstream& is);
};

#endif
