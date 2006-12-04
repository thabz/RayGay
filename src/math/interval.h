
#ifndef MATH_INTERVAL_H
#define MATH_INTERVAL_H

#include "math/vector2.h"
#include <vector>
#include <iosfwd>

class Interval 
{
    friend std::ostream & operator<< (std::ostream &os, const Interval &v);

    public:
	Interval();
	Interval(uint32_t initial_capacity);
	Interval(double from, double to);
	virtual ~Interval();
	bool isEmpty() const;
	void add(const Interval& i);
	void add(double from, double to);
	void subtract(const Interval& i);
	virtual void subtract(double from, double to);
	bool contains(double d) const;
	double length() const;
	double random() const;

	bool operator==(const Interval &v) const;
	bool operator!=(const Interval &v) const;

    private:
	Vector2 getSegment(int i) const;
	void setSegment(Vector2 s, int i);
	uint32_t getSegmentsNum() const;
	std::vector<double> segments;
};

inline 
Vector2 Interval::getSegment(int i) const
{
    return Vector2(segments[i*2+0], segments[i*2+1]);
}

inline 
void Interval::setSegment(Vector2 s, int i) {
    segments[i*2+0] = s[0];
    segments[i*2+1] = s[1];
}

inline 
uint32_t Interval::getSegmentsNum() const 
{
    return segments.size() / 2;
}

class ArcInterval : public Interval {
    public:
	ArcInterval(Vector2 c, double r);

	const Vector2& getC() { return c; };
	double getR() { return r; };
	
	Vector2 randomPoint() const ;

	// Subtract a circle
	void subtract(const Vector2& o_c, double o_r);
	
	// Subtract the outside of an axis aligned box.
	void subtract(const Vector2& lower, const Vector2& upper);
	
	// Subtract an angle segment
	void subtract(double from, double to);
	
    private:
	Vector2 c;
	double r;
};

#endif
