
#ifndef MATH_INTERVAL_H
#define MATH_INTERVAL_H

#include "math/vector2.h"
#include <vector>

class Interval 
{
    public:
	Interval();
	Interval(uint32_t initial_capacity);
	Interval(double from, double to);
	bool isEmpty() const;
	void add(const Interval& i);
	void add(double from, double to);
	void subtract(const Interval& i);
	void subtract(double from, double to);
	double length() const;

	bool operator==(const Interval &v) const;
	bool operator!=(const Interval &v) const;

    private:
	Vector2 getSegment(int i) const;
	void setSegment(Vector2 s, int i);
	uint32_t getSegmentsNum() const;
	void cleanUp();
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

#endif
