
#ifndef PARSER_CAMERA_NODE_H
#define PARSER_CAMERA_NODE_H

#include "camera.h"
#include "parser/floatnodes.h"
#include "parser/vectornodes.h"
#include "parser/syntaxnode.h"

/**
 * A node whose eval() constructs a Camera.
 */
class CameraNode : public SyntaxNode {

    public:

	CameraNode() {
	    position = NULL;
	    look_at = NULL;
	    up = NULL;
	    fov = NULL;
	    dof_num = NULL;
	    dof_aperture = NULL;
	    aa = NULL;
	}

	virtual ~CameraNode() {
	    if (position != NULL) delete position;
	    if (look_at != NULL) delete look_at;
	    if (up != NULL) delete up;
	    if (fov != NULL) delete fov;
	    if (aa != NULL) delete aa;
	    if (dof_num != NULL) {
		delete dof_num;
		delete dof_aperture;
	    }
	}

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
	void enableDoF(FloatNode* dof_aperture, FloatNode* dof_num) {
	    this->dof_aperture = dof_aperture;
	    this->dof_num = dof_num;
	}


	Camera* eval() {
	    Camera* result = new Camera();
	    if (position != NULL) result->setPosition(position->eval());
	    if (look_at != NULL) result->setLookAt(look_at->eval());
	    if (up != NULL) result->setUp(up->eval());
	    if (fov != NULL) result->setFieldOfView(fov->eval());
	    if (aa != NULL) result->enableAdaptiveSupersampling(int(aa->eval()));
	    if (dof_num != NULL) {
		result->enableDoF(dof_aperture->eval(),int(dof_num->eval()));
	    }
	    return result;
	}

    private:
	VectorNode* position;
	VectorNode* look_at;
	VectorNode* up;
	FloatNode* fov;
	FloatNode* dof_num;
	FloatNode* dof_aperture;
	FloatNode* aa;
};

#endif
