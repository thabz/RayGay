
#ifndef PARSER_SYNTAX_NODE_H
#define PARSER_SYNTAX_NODE_H

#include <iostream>
#include "parser/fileposition.h"
#include "parser/runtimeexception.h"

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
	virtual ~SyntaxNode();
	SyntaxNode(FilePosition f) {
	    this->fileposition = f;
	}

    protected:
	void runtime_error(string problem) {
	    throw RuntimeException(problem,fileposition);
	}

	FilePosition getFilePosition() { return fileposition; }

    private:
	FilePosition fileposition;

};

/**
 * This is used for nodes that holds a value.
 */
class ValueNode : public SyntaxNode {
    public:
	enum ValueType {
	    VECTOR,
	    FLOAT
	};

	ValueNode() {};
	ValueNode(FilePosition pos) : SyntaxNode(pos) {};

	virtual ~ValueNode();

	virtual ValueType getType() = 0;
};

#endif
