
#ifndef PIXEL_H
#define PIXEL_H

#include <math.h>

/// A pixel consists of four corner colors
class Pixel {
    public:
	/// Constructor
	Pixel(RGB c1, RGB c2, RGB c3, RGB c4) {
	    c[0] = c1; c[1] = c2;
	    c[2] = c3; c[3] = c4;
	};

	/// The average color
	RGB getAverage() const {
	    return (c[0]+c[1]+c[2]+c[3])/4.0;
	};

	/// Returns the squared variance
	double getVariance() const {
	    RGB avg = getAverage();
	    RGB t[4];
	    t[0] = c[0] - avg;
	    t[1] = c[1] - avg;
	    t[2] = c[2] - avg;
	    t[3] = c[3] - avg;
	    return (t[0] * t[0] + 
		    t[1] * t[1] + 
		    t[2] * t[2] + 
		    t[3] * t[3]) / 4.0;
	};

    private:
	/// The four colors
	RGB c[4];
};

#endif
