
#include "contour.h"

using namespace std;

#include <algorithm>
#include "functions.h"

Contour::Contour() {
}

Contour::~Contour() {
}

bool Contour::isInside(const Vector2& p) const {
    return false;        
}

vector<double> Contour::rasterize(double x_min, double x_max, double y, double size) const {
    vector<double> result;        
    Vector2 c0 = coords[0] * size;
    for(uint32_t k = 1; k <= coords.size(); k++) {
        uint32_t j = k % coords.size();    
        Vector2 c1 = coords[j] * size;
        if (onCurve[j]) {
            double root;
            int n = intersect(x_min, y, c0, c1, &root);
            if (n > 0) {
                result.push_back(root);    
            }
            c0 = c1;
        } else {
            uint32_t j = (k+1) % coords.size();    
            Vector2 c2 = coords[j] * size;
            if (!onCurve[j]) {
                // Reconstruct a new c2 that is on curve    
                c2 = (c1 + c2) * 0.5;
            }
            double roots[2];
            int n = intersect(x_min, y, c0, c1, c2, roots);
            for(int i = 0; i < n; i++) {
                result.push_back(roots[i]);    
            }
            c0 = c2;
        }
    }
    std::sort(result.begin(), result.end());
    return result;
}

/// Intersect a quadratic Bezier curve with the ray with origin (x_min,y) and direction (1,0).
/// This allows for two intersections that are returned as a pair of distances from the rays origin.
/// @return the number of real roots
int Contour::intersect(double x_min, double y, const Vector2& p0, const Vector2& p1, const Vector2& p2, double* result) {
    double A = p0[1] - 2*p1[1] + p2[1];
    double B = 2*p1[1] - 2*p0[1];
    double C = p0[1] - y;

    double roots[2];
    int num = Math::solveQuadratic(A,B,C,roots);

    int n = 0;
    for(int i = 0; i < num; i++) {
        double t = roots[i]; 
        if (t >= 0 && t <= 1) {
            double s = (p0[0]-2*p1[0]+p2[0])*t*t + (2*p1[0]-2*p0[0])*t + p0[0] - x_min;
            if (s >= 0) {
                result[n] = s;        
                n++; 
            }
        }   
    }
    return n;
}

/// Intersect a linesegment with the ray with origin (x_min,y) and direction (1,0).
/// If no intersection is found NaN is returned. Otherwise the distance from the rays origin.
int Contour::intersect(double x_min, double y, const Vector2& a, const Vector2& b, double* result) {
     double t = (y - b[1]) / (a[1] - b[1]);
     if (t >= 0 && t <= 1) {
         double s = t*a[0] + (1-t)*b[0] - x_min;
         if (s >= 0) {
             *result = s;
             return 1;
         }             
     }
     return 0;     
}
