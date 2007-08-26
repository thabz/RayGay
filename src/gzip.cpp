
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
    for(uint32_t i = 000; i <= 143; i++) fixed_lit_alphabet[i].len = 8;
    for(uint32_t i = 144; i <= 255; i++) fixed_lit_alphabet[i].len = 9;
    for(uint32_t i = 256; i <= 279; i++) fixed_lit_alphabet[i].len = 7;
    for(uint32_t i = 280; i <= 287; i++) fixed_lit_alphabet[i].len = 8;
    expand_alphabet(fixed_lit_alphabet, 287);

    for(uint32_t i = 000; i <= 31; i++) fixed_dist_alphabet[i].len = 5;
    expand_alphabet(fixed_dist_alphabet, 31);
        
    cout << "Fixed dist" << endl;
    dump_codes(fixed_dist_alphabet, 31);
    
    skip_header();
    data_pos = is->tellg();
}

void GZIP::error(std::string problem) {
    throw_exception(filename + ": " + problem);    
}

// Header flag bits
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
    
    alphabet_t tree[8];
    tree[0].len = 3;
    tree[1].len = 3;
    tree[2].len = 3;
    tree[3].len = 3;
    tree[4].len = 3;
    tree[5].len = 2;
    tree[6].len = 4;
    tree[7].len = 4;
    expand_alphabet(tree,7);
    dump_codes(tree,7);  
}


uint8_t weird_code_index[19] = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

void GZIP::process_non_compressed_block() {
    // Skip any remaining bits in current partially processed byte
    global_filepos.bits_left_in_cur_byte = 0;

    // Read LEN and NLEN (see next section)
    uint16_t LEN = read_bits(&global_filepos, 16);
    uint16_t NLEN = read_bits(&global_filepos, 16);     

    // Copy LEN bytes of data to output
    for(uint16_t i = 0; i < LEN; i++) {
        uint8_t b = read_uint8();    
        buffer_out_uint8(b);
    }
}

void GZIP::process_fixed_huffman_block() {
    process_huffman_block(fixed_lit_alphabet, fixed_dist_alphabet);
}

void GZIP::process_dynamic_huffman_block() {
    uint32_t HLIT = read_bits(&global_filepos, 5) + 257;
    uint32_t HDIST = read_bits(&global_filepos, 5) + 1;
    uint32_t HCLEN = read_bits(&global_filepos, 4) + 4;
    
    // Create code_length_alphabet
    clear_alphabet(code_length_alphabet, 18);
    for(uint32_t i = 0; i < HCLEN; i++) {
        uint32_t len = read_bits(&global_filepos, 3);
        code_length_alphabet[weird_code_index[i]].len = len;
    }
    expand_alphabet(code_length_alphabet, 18);
    create_tree(code_length_tree, code_length_alphabet, 18);
    
    // Create the dynamic lit-len and dist alphabets using the code_length_alphabet
    create_code_length_encoded_alphabet(dynamic_lit_alphabet, 287, HLIT);
    create_code_length_encoded_alphabet(dynamic_dist_alphabet, 31, HDIST);
        
    process_huffman_block(dynamic_lit_alphabet, dynamic_dist_alphabet);
}

uint8_t len_extra_bits[] = { 0,0,0,0, 0,0,0,0,
                       1,1,1,1, 2,2,2,2,
                       3,3,3,3, 4,4,4,4,
                       5,5,5,5, 0 };
uint16_t len_lengths[] = { 3,4,5,6,7,8,9,10,
                         11,13,15,17, 19,23,27,31,
                         35,43,51,59, 67,83,99,115,
                         131,163,195,227, 258 };

uint8_t dist_extra_bits[] = { 0,0,0,0, 1,1,2,2,
                            3,3,4,4, 5,5,6,6,
                            7,7,8,8, 9,9,10,10,
                            11,11,12,12, 13,13 };
uint16_t dist_distances[] = { 1,2,3,4, 5,7,9,13, 
                            17,25,33,49, 65,97,129,193,
                            257,385,513,769, 1025,1537,2049,3073,
                            4097,6145,8193,12289, 16385,24577 };

// process compressed data using the lit-len and dist alphabets
void GZIP::process_huffman_block(GZIP::alphabet_t* lit_alphabet, GZIP::alphabet_t* dist_alphabet) {
    while(true) {
        uint32_t l = read_huffman_encoded(&global_filepos, lit_alphabet, 287);
        if (l < 256) {
            // We have a literal
            // output l as uint8    
        } else if (l == 256) {
            // We're done        
            return;        
        } else {
            // We have a length
            l -= 257;
            l = len_lengths[l] + read_bits(&global_filepos, len_extra_bits[l]);
            
            // Get the dist
            uint32_t d = read_huffman_encoded(&global_filepos, dist_alphabet, 31);
            d = dist_distances[d] + read_bits(&global_filepos, dist_extra_bits[d]);
            
            // Copy the bytes
            buffer_out_copy(l, d);
        }
    }        
}

void GZIP::deflate() {
    is->seekg(data_pos);
    uint32_t BFINAL, BTYPE;
    do {
        // BFINAL is set if and only if this is the last block of the dataset.            
        BFINAL = read_bits(&global_filepos, 1);
        BTYPE = read_bits(&global_filepos, 2);
        
        // BTYPE 00 - no compression
        // BTYPE 01 - compressed with fixed Huffman codes
        // BTYPE 10 - compressed with dynamic Huffman codes
        // BTYPE 11 - reserved (error)
        switch(BTYPE) {
            case 0 : process_non_compressed_block(); break;
            case 1 : process_fixed_huffman_block(); break;
            case 2 : process_dynamic_huffman_block(); break;
            default:  error("Illegal BTYPE");
        }

        return;
                    
    } while (!BFINAL);
}

/**
 * Reads and creates a new alphabet, that is Huffman encoded using the code_length_tree.
 * max_code is the max code of the alphabet to create. code_lengths is the number of lengths to read.
 */
void GZIP::create_code_length_encoded_alphabet(GZIP::alphabet_t* alphabet, uint32_t max_code, uint32_t code_lengths) {
    uint32_t last = 0, fill;
    uint32_t k = 0;
    clear_alphabet(alphabet, max_code);
    for(uint32_t i = 0; i < code_lengths; i++) {
        uint32_t code_length = read_huffman_encoded(&global_filepos, code_length_tree, 18);
        if (code_length < 16) {
            alphabet[k++].len = code_length;
            fill = last = code_length;
        } else { 
            uint32_t repeats = 0;
            if (code_length == 16) {
                repeats = 3 + read_bits(&global_filepos, 2);
                fill = last;        
            } else if (code_length == 17) {
                repeats = 3 + read_bits(&global_filepos, 3);
                fill = 0;
            } else /* code_length == 18 */ {
                repeats = 11 + read_bits(&global_filepos, 7);
                fill = 0;
            }
            for(uint32_t j = 0; j < repeats; j++) {
                alphabet[k++].len = fill;
            }
        }
    }
    expand_alphabet(alphabet, max_code);
}


// All tree.len must be filled.
// tree must point to array of length max_code+1 
// See RFC 1951, sec. 3.2.2
void GZIP::expand_alphabet(GZIP::alphabet_t* tree, uint32_t max_code) {
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

void GZIP::clear_alphabet(alphabet_t* tree, uint32_t max_code) {
    for(uint32_t i = 0; i <= max_code; i++) tree[i].len = 8;
}

void GZIP::dump_codes(GZIP::alphabet_t* tree, uint32_t max_code) {
    for(uint32_t i = 0; i <= max_code; i++) {
        uint32_t len = tree[i].len;    
        cout << "Symbol " << setfill(' ') << setw(4) << dec << i << ":  bits " << dec << setw(2) << len << ", code " << setfill('0') << setw(3) << hex << tree[i].code << endl;
    }        
}

uint32_t GZIP::read_huffman_encoded(GZIP::file_pos_t* pos, GZIP::tree_t* tree) {
     
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

void GZIP::buffer_out_uint8(uint8_t b) {
    buffer[buffer_pos++] = b;            
}

void GZIP::buffer_out_copy(uint32_t len, uint32_t dist) {
    std::copy(buffer+buffer_pos-dist, buffer+buffer_pos-dist+len, buffer+buffer_pos);
    buffer_pos += len;        
}

void GZIP::buffer_flush() {
        
}

