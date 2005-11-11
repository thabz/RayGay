
#include "filters/filterstack.h"
#include "filters/filter.h"

void FilterStack::push(Filter2D* filter) {
    filters.push_back(filter);
}

void FilterStack::apply(Image* image) {
    uint32_t num = filters.size();
    for(uint32_t i = 0; i < num; i++) {
	filters[i]->apply(image);
    }
}

