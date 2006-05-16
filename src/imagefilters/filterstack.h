

#ifndef FILTER_STACK_H
#define FILTER_STACK_H

#include <vector>

class Image;
class ImageFilter;

class FilterStack {

    public:
	
	void push(ImageFilter* filter);
	void apply(Image* image);

    private:
	std::vector<ImageFilter*> filters;
};

#endif
