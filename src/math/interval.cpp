
#include "math/interval.h"

Interval::Interval(uint32_t initial_capacity) : segments(2*initial_capacity) {
}

Interval::Interval() : segments(2) {
}

Interval::Interval(double from, double to) : segments(2)
{
    segments.push_back(from);
    segments.push_back(to);
}

bool Interval::isEmpty() const 
{
    return segments.size() == 0;
}

double Interval::length() const 
{
    double length = 0.0;
    for(std::vector<double>::const_iterator p = segments.begin(); p != segments.end(); p += 2) {
	length += (*(p+1) - *p);
    }
    return length;
}

void Interval::cleanUp() {
    for(std::vector<double>::iterator p = segments.begin(); p != segments.end(); p += 2) {
	if ((*p) == 1 && (*(p+1)) == -1) {
	    segments.erase(p);
	    segments.erase(p);
	}
    }
}

void Interval::add(double f, double t) 
{
    /*
     * Cases:
     * 1: ---F--SSSSS---T---  Extend segment
     * 2: ---SSSSS---F---T--  Create segment
     * 3: ---F---T---SSSSS--  Create segment
     * 4: ---SSSFSSS---T----  Extend segment
     * 5: ---F---SSSTSSS----  Extend segment
     * 6: ---SSSFSSSTSSS----  Ignore segment
     */ 
    Vector2 new_segment = Vector2(1,-1);
    bool setting_from = true;
    for(uint32_t i = 0; i < getSegmentsNum(); i++) {
	Vector2 s = getSegment(i);
	if (setting_from) {
	    if (f < s[0] && t < s[1]) {
		// Insert new segment right here
		goto done;
	    } 
	    else if (f < s[0] && t > s[1]) {
		s[0] = f;
		setting_from = false;
	    }
	} else {
	    if (t < s[0]) {
		// Extend previous segment
		goto done;
	    }
	    else if (t < s[1]) {
		// Collapse previous and this segment
		goto done;
	    }
	}
    }
    // Extend last segment
done:
    f = 0;
}

void Interval::subtract(double f, double t)
{
    /*
     * Cases:
     * 1: ---F--SSSSS---T---  Remove segment
     * 2: ---SSSSS---F---T--  Ignore segment
     * 3: ---F---T---SSSSS--  Ignore segment
     * 4: ---SSSFSSS---T----  Clip segment
     * 5: ---F---SSSTSSS----  Clip segment
     * 6: ---SSSFSSSTSSS----  Split segment
     */ 
    Vector2 new_segment = Vector2(1,-1);
    for(uint32_t i = 0; i < getSegmentsNum(); i++) {
	Vector2 s = getSegment(i);
	if (f > s[1] || t < s[0]) {
	    // Case 2 and 3: ignore segment
	    continue;
	} 
	else if (s[0] > f && s[1] < t) {
	    // Case 1: remove segment
	    s = Vector2(1,-1);
	}
	else if (f > s[0] && f < s[1] && t > s[1]) {
	    // Case 4: Clip segment
	    s[0] = f;
	}
	else if (t > s[0] && t < s[1] && f < s[0]) {
	    // Case 5: Clip segment
	    s[0] = t;
	} else if (f > s[0] && t < s[1]) {
	    // Case 6: Split segment
	    s[1] = f;
	    new_segment = Vector2(t,s[1]);
	} 
	setSegment(s,i);
    }
    cleanUp();
    if (new_segment != Vector2(1,-1)) {
	add(new_segment[0], new_segment[1]);
    }
}

void Interval::add(const Interval& i) 
{
    Vector2 s;
    for(uint32_t j = 0; j < i.getSegmentsNum(); j++) {
	s = i.getSegment(j);
	add(s[0],s[1]);
    }
}

void Interval::subtract(const Interval& i) 
{
    Vector2 s;
    for(uint32_t j = 0; j < i.getSegmentsNum(); j++) {
	s = i.getSegment(j);
	subtract(s[0],s[1]);
    }
}


bool Interval::operator==(const Interval& x) const {
    uint32_t num = getSegmentsNum();
    if (num != x.getSegmentsNum()) {
	return false;
    }
    for(uint32_t i = 0; i < num; i++) {
	if (getSegment(i) != x.getSegment(i)) {
	    return false;
	}
    }	
    return true;
}

bool Interval::operator!=(const Interval& x) const {
    return !(*this == x);
}
