#ifndef LINE_H
#define LINE_H

#include "math/vector.h"

/// An infinite line
class Line {

    public:
	/// Constructor
	Line(const Vector& a, const Vector& b);
	/// Returns the distance from p to the line
	double distance(const Vector& p) const;
	/// Returns true if p is on the line
	bool contains(const Vector& p) const;
	/// Returns true if line is parallen to other
	bool isParallel(const Line& other) const;
	/// Returns the point this line intersects other
	Vector intersection(const Line& other) const;
	/// Returns direction vector for this line
	Vector getDirection() const;
	/// Comparator
	bool operator==(const Line &other) const;

    private:
	Vector a,b;

};

#endif /* LINE_H */
