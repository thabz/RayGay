
#include "parser/fileposition.h"

FilePosition::FilePosition(string filename) {
    this->filename = filename;
    this->line_num = 1;
}

FilePosition::FilePosition() {
    this->filename = "Unknown";
    this->line_num = 0;
}

FilePosition::~FilePosition() {
}

