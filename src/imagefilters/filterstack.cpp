
#include "imagefilters/filterstack.h"
#include "imagefilters/imagefilter.h"
#include <stdint.h>

void FilterStack::push(ImageFilter* filter) {
    filters.push_back(filter);
}

void FilterStack::apply(Image* image) {
    uint32_t num = filters.size();
    for(uint32_t i = 0; i < num; i++) {
	filters[i]->apply(image);
    }
}

