
#ifndef FILTERS_GAUSSIAN
#define FILTERS_GAUSSIAN

class Gaussian : public Filter
{
    public:
	Gaussian(double w, double h, double a);
	double filter(double x, double y) const;

    private:
	double a;
	double exp_x;
	double exp_y;
	double g(double d, double v) const;

};

Gaussian::Gaussian(double w, double h, double a) 
: Filter(w,h)    
{
    this->a = a;
    exp_x = expf(-a * w * w);
    exp_y = expf(-a * h * h);
}

double Gaussian::g(double d, double v) const 
{
    return max(0.0,expf(-a * d * d) - v);
}

double Gaussian::filter(double x, double y) const 
{
    return g(x, exp_x) * g(y, exp_y);
}

#endif
