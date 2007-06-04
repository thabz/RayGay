
#ifndef PARSER_TRANSFORMATION_FACTORY_H
#define PARSER_TRANSFORMATION_FACTORY_H

#include "scheme/scheme.h"

class Matrix;

class TransformationFactory {

    public:
	static SchemeObject* rotate(SchemeObject* obj, SchemeObject* axis, SchemeObject* angle);
	static SchemeObject* translate(SchemeObject* obj, SchemeObject* translation);
	static SchemeObject* scale(SchemeObject* obj, SchemeObject* scale);

	static void register_procs(Scheme* scheme);

    private:
	static SchemeObject* transform(SchemeObject* obj, const Matrix& m, char* subr);
};

#endif
