
#include "objects/transformer.h"

void Transformer::transform(const Matrix& m) {
    transformation = transformation * m;
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
    normal_transformation = inverse_rotation.transpose();
}

Vector Transformer::pointToObject(const Vector& p) const {
    return inverse_transformation * p;
} 

Vector Transformer::dirToObject(const Vector& d) const {
    return inverse_rotation * d;
}


BoundingBox Transformer::bboxToWorld(const BoundingBox& bbox) const {
    Vector* corners = bbox.getCorners();
    for(int i = 0; i < 8; i++) {
	corners[i] = pointToWorld(corners[i]);
    }
    BoundingBox result = BoundingBox::enclosure(corners,8);
    delete [] corners;
    return result;
}

