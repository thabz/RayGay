
#ifndef PARSER_FILEPOSITION_H
#define PARSER_FILEPOSITION_H

#include <string>    

using namespace std;

class FilePosition {

    public:

	FilePosition(string filename) {
	    this->filename = filename;
	    this->line_num = 1;
	}

	FilePosition() {
	    this->filename = "Unknown";
	    this->line_num = 0;
	}

	~FilePosition() {}

	string getFilename() const { return filename; };
	unsigned int getLineNum() const { return line_num; };

	void setLineNum(unsigned int l) { line_num = l; };
	void incLineNum() { line_num++; };

    private:
	string filename;
	unsigned int line_num;
};

#endif

