
#ifndef PARSER_BOOL_NODES_H
#define PARSER_BOOL_NODES_H

class BoolNode {

    public:
	virtual bool eval() = 0;
};

class BoolAndNode {
    public:
	BoolAndNode(BoolNode* left, BoolNode* right) {
	    this->left = left;
	    this->right = right;
	}

	bool eval() {
	    return left->eval() && right->eval();
	}

    private:
	BoolNode* left;
	BoolNode* right;
};

class BoolOrNode {
    public:
	BoolOrNode(BoolNode* left, BoolNode* right) {
	    this->left = left;
	    this->right = right;
	}

	bool eval() {
	    return left->eval() || right->eval();
	}

    private:
	BoolNode* left;
	BoolNode* right;
};

class BoolNotNode {
    public:
	BoolNotNode(BoolNode* node) {
	    this->node = node;
	}

	bool eval() {
	    return !(node->eval());
	}

    private:
	BoolNode* node;
};


#endif
