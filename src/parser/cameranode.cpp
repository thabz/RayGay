
#include "parser/cameranode.h"

CameraNode::CameraNode(FilePosition pos) : SyntaxNode(pos) {
    position = NULL;
    look_at = NULL;
    up = NULL;
    fov = NULL;
    dof_num = NULL;
    dof_aperture = NULL;
    aa = NULL;
}

CameraNode::~CameraNode() {
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

Camera* CameraNode::eval() {
    Camera* result = NULL;
    if (type == "pinhole" || type == "") {
	result = new Pinhole();
    } else {
	runtime_error("Unknown type of camera: " + type);
    }
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
