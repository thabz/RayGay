
#ifndef PARSER_TRANSFORMATION_FACTORY_H
#define PARSER_TRANSFORMATION_FACTORY_H

#include "scheme/scheme.h"

class Matrix;

class TransformationFactory {

    public:
	static SchemeObject* rotate(Scheme* scheme, SchemeObject* obj, SchemeObject* axis, SchemeObject* angle);
	static SchemeObject* translate(Scheme* scheme, SchemeObject* obj, SchemeObject* translation);
	static SchemeObject* scale(Scheme* scheme, SchemeObject* obj, SchemeObject* scale);

	static void register_procs(Scheme* scheme);

    private:
	static SchemeObject* transform(Scheme* scheme, SchemeObject* obj, const Matrix& m, wchar_t* subr);
};

#endif
