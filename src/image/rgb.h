
#ifndef RGB_H
#define RGB_H

#include "math/vector.h"

///Holds a color
/**
 *  The red, green and blue components should be in the [0,1] range.
 */
class RGB : public Vector {
    public:
	/// Default constructor makes a black color.
	RGB() {
	    Vector::_vector[0] = 0.0;
	    Vector::_vector[1] = 0.0;
	    Vector::_vector[2] = 0.0;
	};

	/// Construct a color from red, green and blue components
	RGB(double r, double g, double b) { 
	    Vector::_vector[0] = r;
	    Vector::_vector[1] = g;
	    Vector::_vector[2] = b;
	};
	/// Copy constructor
	RGB(Vector v) {
	    Vector::_vector[0] = v[0];
	    Vector::_vector[1] = v[1];
	    Vector::_vector[2] = v[2];
	};

	RGB(unsigned char r, unsigned char g, unsigned char b) {
	    Vector::_vector[0] = double(r) / 255.0;
	    Vector::_vector[1] = double(g) / 255.0;
	    Vector::_vector[2] = double(b) / 255.0;

	}

	void clip(); ///< Clips color components to be in the [0,1] range

	/// The red component
        double r() { return Vector::_vector[0]; };
        /// The green component
        double g() { return Vector::_vector[1]; };
        /// The blue component
        double b() { return Vector::_vector[2]; }; 

	double sqrDistance(const RGB& other) const;
};

#endif



