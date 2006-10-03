
#include "filters/filter.h"

Filter::Filter(double w, double h) {
    this->w = w;
    this->h = h;
}

double Filter::filter(const Vector2& pos) const {
    return filter(pos[0],pos[1]);
}
