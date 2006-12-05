
#include "math/interval.h"
#include <iostream>
#include <cassert>

using namespace std;

////////////////////////////////////////////////////////////////
// Interval
////////////////////////////////////////////////////////////////

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

Interval::~Interval() {
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
	    i--;
	}
	else if (f > s[0] && f < s[1] && t >= s[1]) {
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
            assert(contains(d));
	    return d;
	} else {
	    d += *(p+2) - *(p+1);
	}
    }
    assert(false);
}

bool Interval::contains(double d) const
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

////////////////////////////////////////////////////////////////
// ArcInterval
////////////////////////////////////////////////////////////////


ArcInterval::ArcInterval(Vector2 c, double r) : Interval(0.0, M_2PI){
    this->c = c;
    this->r = r;
}

Vector2 ArcInterval::randomPoint() const {
    assert(!isEmpty());
    double angle = random();
    return c + r * Vector2(cos(angle),sin(angle));
}

void ArcInterval::subtract(const Vector2& o_c, double o_r) {
    Vector2 v = o_c - c;
    if (v.norm() < (r+o_r)*(r+o_r)-EPSILON) {
        double l = v.length();
        assert (l >= r - EPSILON);
        double angle = atan2(v[1],v[0]);
        double theta = acos((r-(r+o_r-l)/2.0)/r);
        subtract(angle - theta, angle + theta);
    }
}

#define get_angle(d,r) acos(fabs(d)/(r))
void ArcInterval::subtract(const Vector2& lower, const Vector2& upper) {
    double angle, theta;
    if (c[0] + r > upper[0]) {
        angle = 0.0 * M_PI;    
        theta = get_angle(upper[0]-c[0],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[0] - r < lower[0]) {
        angle = 1.0 * M_PI;    
        theta = get_angle(c[0]-lower[0],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[1] + r > upper[1]) {
        angle = 0.5 * M_PI;    
        theta = get_angle(upper[1]-c[1],r);
        subtract(angle-theta,angle+theta);
    }
    if (c[1] - r < lower[1]) {
        angle = 1.5 * M_PI;    
        theta = get_angle(c[1]-lower[1],r);
        subtract(angle-theta,angle+theta);
    }
}

void ArcInterval::subtract(double from, double to) {
    if (from > to) swap(from,to);
    if (from > M_2PI) {
        subtract(from - M_2PI, to - M_2PI);    
    } else if (to < 0) {
        subtract(from + M_2PI, to + M_2PI);      
    } else if (from < 0) {
        subtract(0,to);
        subtract(M_2PI + from, M_2PI); 
    } else if (to > M_2PI) {
        subtract(from,M_2PI);
        subtract(0,to - M_2PI);    
    } else {
        assert(from >= 0 && to <= M_2PI);
        Interval::subtract(from, to);
    }
}

