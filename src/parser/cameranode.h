
#ifndef PARSER_CAMERA_NODE_H
#define PARSER_CAMERA_NODE_H

#include <string>
#include "cameras/camera.h"
#include "cameras/pinhole.h"
#include "parser/floatnodes.h"
#include "parser/vectornodes.h"
#include "parser/syntaxnode.h"

using namespace std;

/**
 * A node whose eval() constructs a Camera.
 */
class CameraNode : public SyntaxNode {

    public:

	CameraNode(FilePosition pos);
	virtual ~CameraNode();

	void setPosition(VectorNode* position) {
	    this->position = position;
	}
	void setLookAt(VectorNode* look_at) {
	    this->look_at = look_at;
	}
	void setUp(VectorNode* up) {
	    this->up = up;
	}
	void setFieldOfView(FloatNode* fov) {
	    this->fov = fov;
	}
	void enableAA(FloatNode* aa) {
	    this->aa = aa;
	}
	void setType(string type) {
	    // TODO: Check for known types
	    this->type = type;
	}
	void enableDoF(FloatNode* dof_aperture, FloatNode* dof_num) {
	    this->dof_aperture = dof_aperture;
	    this->dof_num = dof_num;
	}

	Camera* eval();

    private:
	VectorNode* position;
	VectorNode* look_at;
	VectorNode* up;
	FloatNode* fov;
	FloatNode* dof_num;
	FloatNode* dof_aperture;
	FloatNode* aa;
	string type;
};

#endif
