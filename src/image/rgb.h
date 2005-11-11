
#ifndef RGB_H
#define RGB_H

#include <iostream>
#include "math/constants.h"

///Holds a color
/**
 *  The red, green and blue components should be in the [0,1] range.
 */
class RGB  {
    friend RGB operator*(const double x, const RGB &v);
    friend std::ostream & operator<< (std::ostream &os, const RGB &x);

    public:
	/// Default constructor makes a black color.
	RGB() {
	    _vector[0] = 0.0;
	    _vector[1] = 0.0;
	    _vector[2] = 0.0;
	};

	/// Construct a color from red, green and blue components
	RGB(double r, double g, double b) { 
	    _vector[0] = r;
	    _vector[1] = g;
	    _vector[2] = b;
	};
	/// Copy constructor
	RGB(const RGB& v) {
	    _vector[0] = v._vector[0];
	    _vector[1] = v._vector[1];
	    _vector[2] = v._vector[2];
	};

	RGB(unsigned char r, unsigned char g, unsigned char b) {
	    _vector[0] = double(r) / 255.0;
	    _vector[1] = double(g) / 255.0;
	    _vector[2] = double(b) / 255.0;

	}

	void clip(); ///< Clips color components to be in the [0,1] range

	/// The red component
        double r() const { return _vector[0]; };
        /// The green component
        double g() const { return _vector[1]; };
        /// The blue component
        double b() const { return _vector[2]; }; 

	double sqrDistance(const RGB& other) const;

	RGB operator*(const RGB& o) const;
	RGB& operator*=(double c);
	RGB operator*(double c) const;
	RGB operator/(double c) const;
	RGB operator+(const RGB &v) const;
	RGB operator-(const RGB &v) const;
	RGB& operator+=(const RGB&v);
	RGB& operator=(const RGB& v);
	double &operator[](const int i); ///< Index into coordinates
	const double &operator[](const int i) const; ///< Index into coordinates
	/// Comparator
	bool operator==(const RGB &v) const;

	/// Return brightness of color
	double brightness() const;

	/// Convert color to grayscale
	RGB toGrayscale() const;

	double norm() const; ///< Returns squared length of vector

    protected:
	double _vector[3];
};

inline
const double &RGB::operator[](const int i) const {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline
double &RGB::operator[](const int i) {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline
RGB RGB::operator*(const RGB& o) const {
     return RGB(r()*o.r(),g()*o.g(),b()*o.b());
}

inline
RGB RGB::operator*(double c) const {
     return RGB(_vector[0]*c,_vector[1]*c,_vector[2]*c);
}

inline
RGB RGB::operator/(double c) const {
     return RGB(_vector[0]/c,_vector[1]/c,_vector[2]/c);
}

/**
 * Using the formula from ITU-R Recommendation BT.709, "Basic Parameter Values for the Studio and for International Programme Exchange (1990) [formerly CCIR Rec. 709] 
 */
inline
double RGB::brightness() const {
    return 0.2125 * r() + 0.7154 * g() + 0.0721 * b();
}

inline
bool RGB::operator==(const RGB &v) const {
    return _vector[0] == v[0] && _vector[1] == v[1] &&_vector[2] == v[2];
}

inline
RGB RGB::toGrayscale() const {
    double g = brightness();
    return RGB(g,g,g);
}

inline
RGB RGB::operator+(const RGB &v) const {
    return RGB(v._vector[0] + _vector[0], v._vector[1] + _vector[1], v._vector[2] + _vector[2]);
}

inline
RGB RGB::operator-(const RGB &v) const {
    return RGB( _vector[0] - v[0], _vector[1] - v[1], _vector[2] - v[2]);
}

inline
RGB& RGB::operator+=(const RGB &v) {
   _vector[0] += v._vector[0];
   _vector[1] += v._vector[1];
   _vector[2] += v._vector[2];
   return *this;
}

inline
RGB& RGB::operator*=(const double x) {
    double* d = _vector;
    *d++ *= x;
    *d++ *= x;
    *d   *= x;
   return *this;
}


inline
double RGB::norm() const {
    return _vector[0]*_vector[0] + _vector[1]*_vector[1] + _vector[2]*_vector[2];
}


inline
RGB operator*(const double x, const RGB &v) {
    return RGB(v._vector[0]*x, v._vector[1]*x, v._vector[2]*x);
}

inline
RGB& RGB::operator=(const RGB& v) {
    _vector[0] = v._vector[0];
    _vector[1] = v._vector[1];
    _vector[2] = v._vector[2];
    return *this;
}

#endif



