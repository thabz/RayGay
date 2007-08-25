
#include "gzip.h"
#include "exception.h"

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <algorithm>

using namespace std;

GZIP::GZIP(std::string filename) {
    this->filename = filename;
    this->is = new std::ifstream(filename.c_str(), ios::in|ios::binary);
    if (is->bad()) {
        error("Can't open file");
    }
    global_filepos.bits_left_in_cur_byte = 0;
    
    // Create the fixed Hoffman codes. See RFC 1951, sec. 3.2.6.
    for(uint32_t i = 000; i <= 143; i++) fixed_huffman_codes[i].len = 8;
    for(uint32_t i = 144; i <= 255; i++) fixed_huffman_codes[i].len = 9;
    for(uint32_t i = 256; i <= 279; i++) fixed_huffman_codes[i].len = 7;
    for(uint32_t i = 280; i <= 287; i++) fixed_huffman_codes[i].len = 8;
    expand_tree(fixed_huffman_codes, 287);    
    cout << "Fixed" << endl;
    dump_codes(fixed_huffman_codes, 287);
    skip_header();
    data_pos = is->tellg();
}

void GZIP::error(std::string problem) {
    throw_exception(filename + ": " + problem);    
}


#define FHTXT     0x01
#define FHCRC     0x02
#define FHEXTRA   0x04
#define FHNAME    0x08
#define FHCOMMENT 0x10

void GZIP::dump_header() {
    is->seekg(0);        
    
    uint8_t b1 = read_uint8();
    uint8_t b2 = read_uint8();
    if (b1 != 0x1f || b2 != 0x8b) {
        error("Not a gzip-file");    
    }

    uint8_t compression_method = read_uint8();

    if (compression_method != 8) {
        error("Unknown compression");    
    }

    uint8_t flags = read_uint8();

    // Skip MTIME, XFL, OS fields
    is->ignore(4+1+1);

    if (flags & FHEXTRA) {
        error("Implement me: skip extras");
    }

    // If FNAME is set, an original file name is present,
    // terminated by a zero byte.  The name must consist of ISO
    // 8859-1 (LATIN-1) characters
    if (flags & FHNAME) {
	cout << "Skipping fhname" << endl;
        while(read_uint8()) {};
    }

    // If FCOMMENT is set, a zero-terminated file comment is
    // present.  This comment is not interpreted; it is only
    // intended for human consumption.  The comment must consist of
    // ISO 8859-1 (LATIN-1) characters.
    if (flags & FHCOMMENT) {
	cout << "Skipping fhcomment" << endl;
        while(read_uint8()) {};
    }

    // If FHCRC is set, a CRC16 for the gzip header is present,
    // immediately before the compressed data. The CRC16 consists
    // of the two least significant bytes of the CRC32 for all
    // bytes of the gzip header up to and not including the CRC16.
    if (flags & FHCRC) {
	cout << "Skipping fhcrc" << endl;
	is->ignore(2);
    }	

    // Note, the file is ended by CRC32 AND ISIZE two 32 bit numbers.
    // ISIZE is the uncompressed filesize modulo 2^32
}

void GZIP::skip_header() {
    is->seekg(0);        
    dump_header();  
    
    tree_t tree[8];
    tree[0].len = 3;
    tree[1].len = 3;
    tree[2].len = 3;
    tree[3].len = 3;
    tree[4].len = 3;
    tree[5].len = 2;
    tree[6].len = 4;
    tree[7].len = 4;
    expand_tree(tree,7);
    dump_codes(tree,7);  
}



void GZIP::deflate() {
    is->seekg(data_pos);
    int BFINAL = 0;
    int BTYPE = 0;
    
    // Iterate blocks
    do {
        // BFINAL is set if and only if this is the last block of the dataset.            
        BFINAL = read_bits(&global_filepos, 1);
        
        // BTYPE 00 - no compression
        // BTYPE 01 - compressed with fixed Huffman codes
        // BTYPE 10 - compressed with dynamic Huffman codes
        // BTYPE 11 - reserved (error)
        BTYPE = read_bits(&global_filepos, 2);
        
        if (BTYPE == 3) error("Illegal BTYPE");
        if (BTYPE == 0) {
            // skip any remaining bits in current partially
            // processed byte
            global_filepos.bits_left_in_cur_byte = 0;

            // read LEN and NLEN (see next section)
            uint16_t LEN = read_bits(&global_filepos, 16);
            uint16_t NLEN = read_bits(&global_filepos, 16);     

            // copy LEN bytes of data to output
            for(uint16_t i = 0; i < LEN; i++) {
                uint8_t b = read_uint8();    
                // output b
            }
        } else {
            if (BTYPE == 2) {
                // compressed with dynamic Huffman codes    
                // read representation of code trees
                
                
            }
            
                
        }
        return;
                    
    } while (!BFINAL);
}

// All tree.len must be filled.
// tree must point to array of length max_code+1 
// See RFC 1951, sec. 3.2.2
void GZIP::expand_tree(GZIP::tree_t* tree, uint32_t max_code) {
    uint8_t max_bits = 0;
    
    for(uint32_t i = 0; i  <= max_code; i++) {
        if (tree[i].len > max_bits) {
            max_bits = tree[i].len;        
        }
    }
    
    uint32_t bl_count[max_bits];
    std::fill_n(bl_count, max_bits, 0);
    for(uint32_t i = 0; i <= max_code; i++) {
        bl_count[tree[i].len]++;    
    }
    
    uint32_t next_code[max_bits];
    uint32_t code = 0;
    for(uint bits = 1; bits <= max_bits; bits++) {
        code = (code + bl_count[bits-1]) << 1;
        next_code[bits] = code;
    }
    
    for(uint n = 0;  n <= max_code; n++) {
        uint len = tree[n].len;
        if (len != 0) {
            tree[n].code = next_code[len];
            next_code[len]++;
        }
    }    
}

void GZIP::dump_codes(GZIP::tree_t* tree, uint32_t max_code) {
    for(uint32_t i = 0; i <= max_code; i++) {
        uint32_t len = tree[i].len;    
        cout << "Symbol " << setfill(' ') << setw(4) << dec << i << ":  bits " << dec << setw(2) << len << ", code " << setfill('0') << setw(3) << hex << tree[i].code << endl;
    }        
}

uint32_t GZIP::read_bits(GZIP::file_pos_t* pos, uint8_t num) {
    uint32_t bits = 0;
    for(uint8_t i = 0; i < num; i++) {
        if (pos->bits_left_in_cur_byte == 0) {
            pos->cur_byte = read_uint8();    
            pos->bits_left_in_cur_byte = 8;   
        }
        bits = (bits << 1) | (pos->cur_byte >> 7);
        pos->cur_byte <<= 1;
        pos->bits_left_in_cur_byte--;
    }
    return bits;   
}

uint8_t GZIP::read_uint8() {
    uint8_t result;
    is->read((char*)&result, 1);
    return result;            
}
