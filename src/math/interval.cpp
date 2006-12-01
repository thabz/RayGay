
#include "math/interval.h"
#include <iostream>
#include <cassert>

using namespace std;

Interval::Interval(uint32_t initial_capacity) {
    segments.reserve(2*initial_capacity);
}

Interval::Interval() {
}

Interval::Interval(double from, double to) 
{
    segments.reserve(2);
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
    std::vector<double>::iterator s;
    /*
     * Cases:
     * 1: ---F--SSSSS---T---  Remove segment
     * 2: ---SSSSS---F---T--  Ignore segment
     * 3: ---F---T---SSSSS--  Ignore segment
     * 4: ---SSSFSSS---T----  Clip segment
     * 5: ---F---SSSTSSS----  Clip segment
     * 6: ---SSSFSSSTSSS----  Split segment
     */ 
    for(uint32_t i = 0; i < getSegmentsNum(); i++) {
	s = segments.begin() + i * 2;
//	cout << "Segment: [" << s[0] << "-" << s[1] << "]" << endl;
	if (f > s[1] || t < s[0]) {
	    // Case 2 and 3: Ignore segment
	    continue;
	} 
	else if (s[0] >= f && s[1] <= t) {
	    // Case 1: Remove segment
	    segments.erase(s);
	    segments.erase(s);
	}
	else if (f > s[0] && f <= s[1] && t > s[1]) {
	    // Case 4: Clip segment
	    s[1] = f;
	}
	else if (t > s[0] && t <= s[1] && f <= s[0]) {
	    // Case 5: Clip segment
	    s[0] = t;
	    goto done;
	} else if (f > s[0] && t < s[1]) {
	    // Case 6: Split segment
	    double e = s[1];
	    s[1] = f;
	    segments.insert(s+2,t);
	    s = segments.begin() + i * 2;
	    segments.insert(s+3,e);
	    goto done;
	} 
    }
    done: return;
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

double Interval::random() const 
{
    assert(!isEmpty());
    double d = RANDOM(0,length());
    d += segments[0];
    for(std::vector<double>::const_iterator p = segments.begin(); p != segments.end(); p += 2) {
	if (IS_LESS_THAN(d,*(p+1))) {
	    return d;
	} else {
	    d += *(p+2) - *(p+1);
	}
    }
    assert(false);
}

bool Interval::contains(double d) 
{
    for(std::vector<double>::const_iterator p = segments.begin(); p != segments.end(); p += 2) {
	if (IS_LESS_THAN(*p,d) && IS_GREATER_THAN(*(p+1),d))
	    return true;
    }
    return false;
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

ostream & operator<<(ostream &os, const Interval &v) {
    if (v.isEmpty()) {
	os << "NIL";
    } else {
	for(std::vector<double>::const_iterator p = v.segments.begin(); p != v.segments.end(); p += 2) {
	    os << "[" << *p << "," << *(p+1) << "] ";
	}
    }
    return os;
}

