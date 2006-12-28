
#include "parser/transformationfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "objects/sceneobject.h"
#include "math/matrix.h"
#include <guile/gh.h>

SCM TransformationFactory::rotate(SCM s_obj, SCM s_axis, SCM s_angle) 
{
    Vector axis = scm2vector(s_axis, "rotate", 2);
    double angle = scm_num2double(s_angle,3,"rotate");
    Matrix matrix = Matrix::matrixRotate(axis,angle);
    return transform(s_obj, matrix, "rotate");
}

SCM TransformationFactory::translate(SCM s_obj, SCM s_translation) 
{
    Vector translation = scm2vector(s_translation, "translate", 2);
    Matrix matrix = Matrix::matrixTranslate(translation);
    return transform(s_obj, matrix, "translate");
}

SCM TransformationFactory::scale(SCM s_obj, SCM s_scale) 
{
    Vector scale = scm2vector(s_scale, "scale", 2);
    Matrix matrix = Matrix::matrixScale(scale);
    return transform(s_obj, matrix, "scale");
}

/**
 * Transforms a scene object, a vector or a list of sceneobjects.
 */
SCM TransformationFactory::transform(SCM s_obj, const Matrix& m, char* subr) 
{
    // Tjek if it's a vector
    if (SCM_NFALSEP (scm_vector_p(s_obj))) {
	if (3 == scm_num2int(scm_vector_length(s_obj),0,"")) {
	    bool is_num = true;
	    for(uint32_t i = 0; i < 3; i++) {
		SCM thing = scm_vector_ref(s_obj, scm_int2num(i));
		is_num &= SCM_NFALSEP(scm_number_p(thing));
	    }
	    if (is_num) {
		Vector v = scm2vector(s_obj, subr, 0);
		return vector2scm(m * v);
	    }
	}
    }
    
    vector<SCM> objs;
    if (SCM_NFALSEP (scm_list_p(s_obj))) {
	uint32_t num = scm_num2int(scm_length(s_obj),0,"");
	for(uint32_t i = 0; i < num; i++) {
	    SCM s_value = scm_list_ref(s_obj, scm_int2num(i));
	    if (SCM_NFALSEP(scm_list_p(s_value))) {
	        // Recurse into embedded lists of objects    
	        transform(s_value, m, subr);    
	    } else {
	        objs.push_back(s_value);
            }
	}
    } else {
	objs.push_back(s_obj);
    }

    uint32_t num = objs.size();
    for(uint32_t i = 0; i < num; i++) {
	SceneObject* object = scm2sceneobject(objs[i], subr, i + 1);
	object->transform(m);
    }
    return s_obj;
}

void TransformationFactory::register_procs() 
{
    scm_c_define_gsubr("rotate",3,0,0,(SCM (*)()) TransformationFactory::rotate);
    scm_c_define_gsubr("translate",2,0,0,(SCM (*)()) TransformationFactory::translate);
    scm_c_define_gsubr("scale",2,0,0,(SCM (*)()) TransformationFactory::scale);
}
