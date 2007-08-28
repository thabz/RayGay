
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
    create_tree(fixed_lit_tree, fixed_lit_alphabet, 287);

    for(uint32_t i = 000; i <= 31; i++) fixed_dist_alphabet[i].len = 5;
    expand_alphabet(fixed_dist_alphabet, 31);
    create_tree(fixed_dist_tree, fixed_dist_alphabet, 31);
    
    skip_header();
    data_pos = is->tellg();
    
    buffer = new uint8_t[100*1024];
    buffer_pos = 0;
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

void GZIP::skip_header() {
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

void GZIP::process_non_compressed_block() {
    // Skip any remaining bits in current partially processed byte
    global_filepos.bits_left_in_cur_byte = 0;

    // Read LEN and NLEN (see next section)
    uint16_t LEN = read_bits(&global_filepos, 16);
    uint16_t NLEN = read_bits(&global_filepos, 16);     
    if (NLEN != -LEN) error("Invalid LEN/NLEN");

    // Copy LEN bytes of data to output
    for(uint16_t i = 0; i < LEN; i++) {
        uint8_t b = read_uint8();    
        buffer_out_uint8(b);
    }
}

void GZIP::process_fixed_huffman_block() {
    process_huffman_block(fixed_lit_tree, fixed_dist_tree);
}

uint8_t weird_code_index[19] = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

void GZIP::process_dynamic_huffman_block() {
        
    alphabet_t joined_alphabet[288+32];        
        
    uint32_t HLIT = read_bits(&global_filepos, 5) + 257;
    uint32_t HDIST = read_bits(&global_filepos, 5) + 1;
    uint32_t HCLEN = read_bits(&global_filepos, 4) + 4;
    
    cout << "HLIT " << dec << HLIT << endl;
    cout << "HDIST " << dec << HDIST << endl;
    cout << "HCLEN " << dec << HCLEN << endl;

    // Create code_length_alphabet
    clear_alphabet(code_length_alphabet, 18);
    for(uint32_t i = 0; i < HCLEN; i++) {
        uint32_t len = read_bits(&global_filepos, 3);
        code_length_alphabet[weird_code_index[i]].len = len;
    }
    expand_alphabet(code_length_alphabet, 18);
    dump_codes(code_length_alphabet, 18);
    create_tree(code_length_tree, code_length_alphabet, 18);
    dump_tree(code_length_tree);
    cout << "E" << endl;
    
    // Create the dynamic lit-len and dist alphabets using the code_length_alphabet
    clear_alphabet(joined_alphabet, 288+32-1);
    cout << "F" << endl;
    create_code_length_encoded_alphabet(joined_alphabet, HLIT+HDIST);
    cout << "G" << endl;

    expand_alphabet(joined_alphabet, 287);
    cout << "G" << endl;
    expand_alphabet(&joined_alphabet[288], 31);

    // Create the dynamic lit-len and dist trees using the corresponding alphabets
    create_tree(dynamic_lit_tree, joined_alphabet, 287);
    create_tree(dynamic_dist_tree, &joined_alphabet[288], 31);
 
    cout << "Dynamic lit tree: " << endl;
    dump_tree(dynamic_dist_tree);

    cout << "I" << endl;
        
    process_huffman_block(dynamic_lit_tree, dynamic_dist_tree);
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
void GZIP::process_huffman_block(GZIP::tree_t* lit_tree, GZIP::tree_t* dist_tree) {
    while(true) {
        uint32_t l = read_huffman_encoded(&global_filepos, lit_tree);
        if (l < 256) {
            // We have a literal
            buffer_out_uint8(l);
        } else if (l == 256) {
            // We're done        
            return;        
        } else {
            // We have a length
            l -= 257;
            l = len_lengths[l] + read_bits(&global_filepos, len_extra_bits[l]);
            
            // Get the dist
            uint32_t d = read_huffman_encoded(&global_filepos, dist_tree);
            d = dist_distances[d] + read_bits(&global_filepos, dist_extra_bits[d]);
            
            // Copy the bytes
            buffer_out_copy(l, d);
        }
    }        
}

void GZIP::deflate() {
    cout << "Deflating" << endl;        
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
        cout << "BTYPE: " << BTYPE << endl;
        cout << "BFINAL: " << BFINAL << endl;
        switch(BTYPE) {
            case 0 : process_non_compressed_block(); break;
            case 1 : process_fixed_huffman_block(); break;
            case 2 : process_dynamic_huffman_block(); break;
            default:  error("Illegal BTYPE");
        }
    } while (!BFINAL);
}

/**
 * Reads and creates a new alphabet, that is Huffman encoded using the code_length_tree.
 * code_lengths is the number of lengths to read.
 *
 * Called from process_dynamic_huffman_block()
 */
void GZIP::create_code_length_encoded_alphabet(GZIP::alphabet_t* alphabet, uint32_t code_lengths) {
    uint32_t k = 0, repeats;
    for(uint32_t i = 0; i < code_lengths; i++) {
        uint32_t code = read_huffman_encoded(&global_filepos, code_length_tree);
        if (code < 16) {
            repeats = 1;
        } else if (code == 16) {
            repeats = 3 + read_bits(&global_filepos, 2);
            code = alphabet[k-1].len;
        } else if (code == 17) {
            repeats = 3 + read_bits(&global_filepos, 3);
            code = 0;
        } else if (code == 18) {
            repeats = 11 + read_bits(&global_filepos, 7);
            code = 0;
        } else {
            error("Illegal code length");
            return;    
        }
        //cout << "Repeats " << repeats << endl;
        for(uint32_t j = 0; j < repeats; j++) {
            alphabet[k++].len = code;
        }
    }
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

uint32_t reverse_bits(uint32_t bits, uint8_t num) {
    uint32_t result = 0;
    for(uint8_t i = 0; i < num; i++) {
        result = (result << 1) | ((bits >> num) & 1);    
    }
    return result;        
}

// Creates a tree from an alphabet. Assuming that tree points to enough reserved space.
void GZIP::create_tree(GZIP::tree_t* tree, GZIP::alphabet_t* alphabet, uint32_t max_code) {
    tree[0].left = -1;
    tree[0].right = -1;
    int16_t next_free = 1;
    for(uint32_t letter = 0; letter <= max_code; letter++) {
        uint8_t bits_num = alphabet[letter].len;
        uint32_t code = alphabet[letter].code;
        
        // Reverse bits in code
        //code = reverse_bits(code, bits_num);

        if (bits_num > 0) {        
            // Navigate through tree, creating new non-leaf nodes as needed.
            int16_t cur_pos = 0;
            for(int16_t bit = bits_num-1; bit >= 0; bit--) {
                int16_t* branch;
                if (code & (1 << bit)) {
                   branch = &(tree[cur_pos].right);
                } else {
                   branch = &(tree[cur_pos].left);
                }
                if (*branch == -1) {
                    *branch = next_free;
                    tree[next_free].right = -1;
                    tree[next_free].left = -1;
                    cur_pos = next_free;
                    next_free++;
                } else {
                    cur_pos = *branch;    
                }
            }
            tree[cur_pos].letter = letter;
        }
    }
}

void GZIP::dump_tree_recur(GZIP::tree_t* tree, uint16_t index, int indent) {
    if ((tree[index].left == -1 && tree[index].right != -1) || 
        (tree[index].left != -1 && tree[index].right == -1)) {
            error("Problemo in tree");
    }
        
    if (tree[index].left == -1) {
         if (tree[index].right != -1) error("Problem in tree");   
         for(uint16_t j = 0; j < indent; j++) cout << ">";  
         cout << " " << dec << tree[index].letter << endl;
    } else {
         dump_tree_recur(tree, tree[index].left, indent + 1);   
         dump_tree_recur(tree, tree[index].right, indent + 1);   
    }
}

void GZIP::dump_tree(GZIP::tree_t* tree) {
    cout << "** tree dump begin" << endl;        
    dump_tree_recur(tree, 0, 0);            
    cout << "** tree dump end" << endl;        
}


void GZIP::clear_alphabet(alphabet_t* tree, uint32_t max_code) {
    for(uint32_t i = 0; i <= max_code; i++) tree[i].len = 0;
}

void GZIP::dump_codes(GZIP::alphabet_t* tree, uint32_t max_code) {
    for(uint32_t i = 0; i <= max_code; i++) {
        uint32_t len = tree[i].len;    
        cout << "Symbol " << setfill(' ') << setw(4) << dec << i << ":  bits " << dec << setw(2) << len << ", code " << setfill('0') << setw(3) << hex << tree[i].code << endl;
    }        
}

int32_t find_in_tree(GZIP::tree_t* tree, uint32_t bits, uint8_t bits_num) {
    //cout << "Bits num " << dec << int(bits_num) << endl;
    uint16_t pos = 0;        
    while(bits_num > 0) {
        uint bit = bits & (1 << (--bits_num));
        pos = bit ? tree[pos].right : tree[pos].left;
    }        
    return tree[pos].left == -1 ? tree[pos].letter : -1;         
}

uint32_t GZIP::read_huffman_encoded(GZIP::file_pos_t* file_pos, GZIP::tree_t* tree) {
    uint32_t bits = 0;
    uint8_t bits_num = 0;
    do {
        uint32_t bit = GZIP::read_bits(file_pos, 1);
        bits = (bits << 1) | bit;
        bits_num++;
        int32_t letter = find_in_tree(tree, bits, bits_num);
        if (letter >= 0) {
            //cout << "Read letter " << dec << letter << endl;        
            return letter;         
        }    
    } while (bits_num < 255);
    error("Nothing matches in Huffman tree");
    return 0;
}

uint32_t GZIP::read_bits(GZIP::file_pos_t* pos, uint8_t num) {
    uint32_t bits = 0;
    for(uint8_t i = 0; i < num; i++) {
        if (pos->bits_left_in_cur_byte == 0) {
            pos->cur_byte = read_uint8();    
            pos->bits_left_in_cur_byte = 8;   
        }
#if 1
        bits |= (pos->cur_byte & 1) << i;
        pos->cur_byte >>= 1;
#else
        bits <<= 1;
        bits |= (pos->cur_byte & 1);
        pos->cur_byte >>= 1;
#endif        
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
//    cout << "Copying len " << len << " dist " << dist << endl;
    for(uint32_t i = 0; i < len; i++) {
        buffer[buffer_pos] = buffer[buffer_pos-dist];      
        buffer_pos++;
    }
}

void GZIP::buffer_flush() {
        
}

void GZIP::dump_buffer() {
    for(uint32_t i = 0; i < buffer_pos; i++) {
        cout << buffer[i];    
    }
    cout << "*EOF*" << flush << endl;
    cout << flush << endl;
}
