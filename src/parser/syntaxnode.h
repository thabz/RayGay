
#ifndef PARSER_SYNTAX_NODE_H
#define PARSER_SYNTAX_NODE_H

/**
 * A node in the abstract syntax tree build by the scene-file parser.
 */
class SyntaxNode {

};

class ValueNode : public SyntaxNode {
    public:
	enum ValueType {
	    VECTOR,
	    FLOAT
	};

	virtual ValueType getType() = 0;
};

#endif
