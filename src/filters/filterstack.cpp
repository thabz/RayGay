
#include "filters/filterstack.h"
#include "filters/filter.h"

void FilterStack::pushFilter(Filter2D* filter) {
    filters.push_back(filter);
}

void FilterStack::apply(Image* image) {
    unsigned int num = filters.size();
    for(unsigned int i = 0; i < num; i++) {
	filters[i]->apply(image);
    }
}

