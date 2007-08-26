
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
        struct file_pos_t {
            uint8_t cur_byte;
            uint8_t bits_left_in_cur_byte;
            std::streampos pos;
        };
        struct alphabet_t {
            uint8_t len;
            uint32_t code;
        };
        struct tree_t {
            uint16_t left;
            uint16_t right;
            uint32_t code;            
        };
        GZIP(std::string filename);
        void dump_header();
	void deflate();
	
    private:	
        void expand_alphabet(alphabet_t* tree, uint32_t max_code);
        void clear_alphabet(alphabet_t* tree, uint32_t max_code);
        void dump_codes(alphabet_t* tree, uint32_t max_code);
        void create_code_length_encoded_alphabet(alphabet_t* alphabet, uint32_t max_code, uint32_t code_lengths);
        
        void process_non_compressed_block();
        void process_fixed_huffman_block();
        void process_dynamic_huffman_block();
        void process_huffman_block(alphabet_t*, alphabet_t*);
            
        uint8_t read_uint8();
        uint32_t read_bits(file_pos_t* state, uint8_t num);
        uint32_t read_huffman_encoded(GZIP::file_pos_t* pos, GZIP::alphabet_t* alphabet, uint32_t max_code);
        void write_uint8(uint32_t b);
        
        void skip_header();
        void error(std::string);
        std::string filename;

        std::streampos data_pos;
        file_pos_t global_filepos;
        std::ifstream* is;
	
	// Fixed literal/length alphabet
        alphabet_t fixed_lit_alphabet[287];

	// Fixed literal/length alphabet
        alphabet_t fixed_dist_alphabet[32];
        
	// Dynamic literal/length alphabet
        alphabet_t dynamic_lit_alphabet[287];
	
	// Dynamic distance alphabet
        alphabet_t dynamic_dist_alphabet[32];
        
        // Code length alphabet
        alphabet_t code_length_alphabet[19];
        
};

#endif
