
#ifndef FILTERS_MATRIX_FILTER_H
#define FILTERS_MATRIX_FILTER_H

#include "filters/filter.h"
#include "filters/colormatrix.h"

class MatrixFilter : public Filter2D {

    public:
	virtual void apply(Image* image);

    protected:
	MatrixFilter(const ColorMatrix& m);

    private:
	ColorMatrix matrix;
};

#endif
