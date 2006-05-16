
#ifndef FILTERS_MATRIX_FILTER_H
#define FILTERS_MATRIX_FILTER_H

#include "imagefilters/imagefilter.h"
#include "imagefilters/colormatrix.h"

class MatrixFilter : public ImageFilter {

    public:
	virtual void apply(Image* image);

    protected:
	MatrixFilter(const ColorMatrix& m);

    private:
	ColorMatrix matrix;
};

#endif
