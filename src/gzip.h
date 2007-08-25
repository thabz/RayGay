
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
        struct tree_t {
            uint8_t len;
            uint32_t code;
        };
        GZIP(std::string filename);
        void dump_header();
	void deflate();
	
    private:	
        void expand_tree(tree_t* tree, uint32_t max_code);
        void dump_codes(tree_t* tree, uint32_t max_code);
            
        uint8_t read_uint8();
        uint32_t read_bits(file_pos_t* state, uint8_t num);
        void skip_header();
        void error(std::string);
        std::string filename;

        std::streampos data_pos;
        file_pos_t global_filepos;
        std::ifstream* is;
	
        tree_t fixed_huffman_codes[287];
};

#endif
