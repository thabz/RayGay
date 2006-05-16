
#ifndef FILTERS_FILTER
#define FILTERS_FILTER

class Filter {

    public:
	Filter(double w, double h);
	double filter(const Vector2& pos) const;
	virtual double filter(double x, double y) const = 0;
	virtual ~Filter() {};

    private:
	double w, double h;
};

Filter::Filter(double w, double h) : {
    this->w = w;
    this->h = h;
}

Filter::filter(const Vector2& pos) const {
    return filter(pos[0],pos[1]);
}

#endif
