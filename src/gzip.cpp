
#include "gzip.h"
#include "exception.h"

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>

using namespace std;

uint8_t read_uint8(ifstream& is) {
    uint8_t result;
    is.read((char*)&result, 1);
    return result;            
}

#define FHTXT     0x01
#define FHCRC     0x02
#define FHEXTRA   0x04
#define FHNAME    0x08
#define FHCOMMENT 0x10

void GZIP::dump_info(std::string filename) {
    cout << filename << endl;
    ifstream is(filename.c_str(), ios::in|ios::binary);
    if (is.bad()) {
        throw_exception("Can't open " + filename);    
    }
    
    uint8_t b1 = read_uint8(is);
    uint8_t b2 = read_uint8(is);
    if (b1 != 0x1f || b2 != 0x8b) {
        throw_exception("Not a gzip-file: " + filename);    
    }

    uint8_t compression_method = read_uint8(is);

    if (compression_method != 8) {
        throw_exception("Unknown compression: " + filename);    
    }

    uint8_t flags = read_uint8(is);

    // Skip MTIME, XFL, OS fields
    is.ignore(4+1+1);

    if (flags & FHEXTRA) {
        throw_exception("Implement me: skip extras");
    }

    // If FNAME is set, an original file name is present,
    // terminated by a zero byte.  The name must consist of ISO
    // 8859-1 (LATIN-1) characters
    if (flags & FHNAME) {
	cout << "Skipping fhname" << endl;
        while(read_uint8(is)) {};
    }

    // If FCOMMENT is set, a zero-terminated file comment is
    // present.  This comment is not interpreted; it is only
    // intended for human consumption.  The comment must consist of
    // ISO 8859-1 (LATIN-1) characters.
    if (flags & FHCOMMENT) {
	cout << "Skipping fhcomment" << endl;
        while(read_uint8(is)) {};
    }

    // If FHCRC is set, a CRC16 for the gzip header is present,
    // immediately before the compressed data. The CRC16 consists
    // of the two least significant bytes of the CRC32 for all
    // bytes of the gzip header up to and not including the CRC16.
    if (flags & FHCRC) {
	cout << "Skipping fhcrc" << endl;
	is.ignore(2);
    }	

    deflate(is);

    // Note, the file is ended by CRC32 AND ISIZE two 32 bit numbers.
    // ISIZE is the uncompressed filesize modulo 2^32
}

void GZIP::deflate(ifstream& is) {

}
