
#ifndef PARSER_TRANSFORMATION_FACTORY_H
#define PARSER_TRANSFORMATION_FACTORY_H

#include <libguile.h>

class Matrix;

class TransformationFactory {

    public:
	static SCM rotate(SCM obj, SCM axis, SCM angle);
	static SCM translate(SCM obj, SCM translation);

	static void register_procs();

    private:
	static void transform(SCM obj, const Matrix& m, char* subr);
};

#endif
