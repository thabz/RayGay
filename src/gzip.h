
#ifndef RAYGAY_GZIP
#define RAYGAY_GZIP

#include <fstream>
#include <stdint.h>
#include <string>

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
    int16_t left;
    int16_t right;
    uint32_t letter;
  };

  GZIP(std::string filename);
  void deflate();
  void dump_buffer();

private:
  // Helpers for handling alphabets and Huffman trees
  void expand_alphabet(alphabet_t *tree, uint32_t max_code);
  void clear_alphabet(alphabet_t *tree, uint32_t max_code);
  void dump_codes(alphabet_t *tree, uint32_t max_code);
  void create_code_length_encoded_alphabet(alphabet_t *alphabet,
                                           uint32_t code_lengths,
                                           tree_t *code_length_tree);
  void create_tree(tree_t *tree, alphabet_t *alphabet, uint32_t max_code);
  void dump_tree(tree_t *tree);
  void dump_tree_recur(tree_t *tree, uint16_t index, int indent);

  // Methods for processing the diffent blocks
  void process_non_compressed_block();
  void process_fixed_huffman_block();
  void process_dynamic_huffman_block();
  void process_huffman_block(tree_t *, tree_t *);

  // Methods for reading bits and bytes
  uint8_t read_uint8();
  uint32_t read_bits(file_pos_t *state, uint8_t num);
  uint32_t read_huffman_encoded(GZIP::file_pos_t *pos, GZIP::tree_t *tree);

  // Methods for handling the output
  void buffer_out_uint8(uint8_t b);
  void buffer_out_copy(uint32_t len, uint32_t dist);
  void buffer_flush();

  void skip_header();
  void error(std::string);
  std::string filename;

  std::streampos data_pos;
  file_pos_t global_filepos;
  std::ifstream *is;

  uint8_t *buffer;
  uint64_t buffer_pos;
};

#endif
