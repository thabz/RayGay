
#ifndef PARSER_SYNTAX_NODE_H
#define PARSER_SYNTAX_NODE_H

#include <iostream>
#include "parser/fileposition.h"

using namespace std;

/**
 * Superclass for all nodes in the abstract syntax tree build 
 * by the scene-file parser.
 *
 * This provides basic error handling.
 */
class SyntaxNode {
    public:
	SyntaxNode() { };
	SyntaxNode(FilePosition f) {
	    this->fileposition = f;
	}

    protected:
	void runtime_error(string problem) {
	    cout << fileposition.getFilename() << ":"
		 << fileposition.getLineNum() << ":"
		 << " runtime error: " << problem << endl;
	    exit(1);
	}

    private:
	FilePosition fileposition;

};

class ValueNode : public SyntaxNode {
    public:
	enum ValueType {
	    VECTOR,
	    FLOAT
	};

	ValueNode() {};
	ValueNode(FilePosition pos) : SyntaxNode(pos) {};

	virtual ~ValueNode() {};

	virtual ValueType getType() = 0;
};

#endif
