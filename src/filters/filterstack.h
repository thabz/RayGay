

#ifndef FILTER_STACK_H
#define FILTER_STACK_H

#include <vector>

class Image;
class Filter2D;

class FilterStack {

    public:
	
	void pushFilter(Filter2D* filter);
	void apply(Image* image);

    private:
	std::vector<Filter2D*> filters;
};

#endif
