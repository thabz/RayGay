#ifndef PATH_H
#define PATH_H

class Vector;

/// Abstract path
class Path {
    
    public:
	/// Get a point on the path where t in [0,1]
	virtual Vector getPoint(double t) const = 0;
	
	/// Get a tangent to the path where t in [0,1]
	virtual Vector getTangent(double t) const = 0;
	
	/// Get evenly spaced points on the path
	void getPoints(int num, Vector* out) const;

	/// Get evenly spaced tangents on the path
	void getTangents(int num, Vector* out) const;

	/// Return true if the endpoint equals the beginpoint
	bool isClosed() const;
};

#endif



