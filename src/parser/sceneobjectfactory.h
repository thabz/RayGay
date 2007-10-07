
#ifndef PARSER_SCENEOBJECT_FACTORY_H
#define PARSER_SCENEOBJECT_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for sceneobject-related Scheme-procedures.
 */
class SceneObjectFactory {
    public:
        static SchemeObject* make_parametrized_surface(SchemeObject* s_proc, SchemeObject* s_u_res, SchemeObject* s_v_res, SchemeObject* s_u_close, SchemeObject* s_v_close, SchemeObject* s_material);    
        static SchemeObject* make_isosurface(SchemeObject* s_proc, SchemeObject* s_vec_lower, SchemeObject* s_vec_higher, SchemeObject* s_iso, SchemeObject* s_steps, SchemeObject* s_accuracy, SchemeObject* s_material);    
        static SchemeObject* make_text(SchemeObject* s_text, SchemeObject* s_font, SchemeObject* s_size, SchemeObject* s_depth, SchemeObject* s_material);    

	static void register_procs(Scheme* scheme);

    private:
        static Scheme* scheme;    	
};

#endif
