
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
    transform(s_obj, matrix, "rotate");
    return s_obj;
}

SCM TransformationFactory::translate(SCM s_obj, SCM s_translation) 
{
    Vector translation = scm2vector(s_translation, "translate", 2);
    Matrix matrix = Matrix::matrixTranslate(translation);
    transform(s_obj, matrix, "translate");
    return s_obj;
}

/**
 * Transforms a scene object or a list of sceneobjects.
 */
void TransformationFactory::transform(SCM s_obj, const Matrix& m, char* subr) 
{
    vector<SCM> objs;
    
    if (SCM_NFALSEP (scm_list_p(s_obj))) {
	uint num = scm_num2int(scm_length(s_obj),0,"");
	for(uint i = 0; i < num; i++) {
	    SCM s_value = scm_list_ref(s_obj, scm_int2num(i));
	    objs.push_back(s_value);
	}
    } else {
	objs.push_back(s_obj);
    }

    uint num = objs.size();
    for(uint i = 0; i < num; i++) {
	SceneObject* object = scm2sceneobject(objs[i], subr, i + 1);
	object->transform(m);
    }
}

void TransformationFactory::register_procs() 
{
    scm_c_define_gsubr("rotate",3,0,0,(SCM (*)()) TransformationFactory::rotate);
    scm_c_define_gsubr("translate",2,0,0,(SCM (*)()) TransformationFactory::translate);
}
