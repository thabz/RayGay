
#include "parser/schemeisosurface.h"

SchemeIsosurface::SchemeIsosurface(SCM procedure_name, BoundingBox bbox, unsigned int steps, double accuracy, double iso, Material* mat) : IsoSurface(steps, accuracy, iso, mat)
{
    this->bbox = bbox;
    this->procedure_name = procedure_name;
}

BoundingBox SchemeIsosurface::_boundingBoundingBox() const {
    return bbox;
}

double SchemeIsosurface::evaluateFunction(const Vector& point) const {
    // TODO: Beskyt af en mutex, da guile åbenbart ikke er trådsikker.
    SCM x = scm_double2num(point[0]); 
    SCM y = scm_double2num(point[1]); 
    SCM z = scm_double2num(point[2]); 
    SCM s_result = scm_call_3(procedure_name, x, y, z);
    return scm_num2double(s_result, 0, NULL);
}

SceneObject* SchemeIsosurface::clone() const {
    SchemeIsosurface* result = new SchemeIsosurface(*this);
    return result;
}
