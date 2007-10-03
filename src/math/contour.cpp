
#include "contour.h"

#include <algorithm>
#include "functions.h"

using namespace std;

bool Contours::isInside(const Vector2& p) const {
    return false;        
}

vector<double> Contours::rasterize(double x_min, double x_max, double y, double size) const {
    vector<double> result;        

    for(uint32_t m = 0; m < contours.size(); m++) {
        const Contour& contour = contours[m];    
        Vector2 c0 = contour.coords[0] * size;
        for(uint32_t k = 1; k <= contour.coords.size(); k++) {
            uint32_t j = k % contour.coords.size();    
            Vector2 c1 = contour.coords[j] * size;
            if (contour.onCurve[j]) {
                double root;
                int n = intersect(x_min, y, c0, c1, &root);
                if (n > 0) {
                    result.push_back(root);    
                }
                c0 = c1;
            } else {
                uint32_t j = (k+1) % contour.coords.size();    
                Vector2 c2 = contour.coords[j] * size;
                if (!contour.onCurve[j]) {
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
    }
    
    std::sort(result.begin(), result.end());
    return result;
}

/// Intersect a quadratic Bezier curve with the ray with origin (x_min,y) and direction (1,0).
/// This allows for two intersections that are returned as a pair of distances from the rays origin.
/// @return the number of real roots
int Contours::intersect(double x_min, double y, const Vector2& p0, const Vector2& p1, const Vector2& p2, double* result) {
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
// @return 1 if intersecting, 0 otherwise. Distance written to *result.
int Contours::intersect(double x_min, double y, const Vector2& a, const Vector2& b, double* result) {
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

Contours::Contours() {
}

Contours::~Contours() {
}

// See http://developer.apple.com/textfonts/TTRefMan/RM06/Chap6glyf.html
void Contours::transform(float a, float b, float c, float d, float e, float f) {
    float m = std::max(fabs(a),fabs(b));
    float n = std::max(fabs(c),fabs(d));
    if (fabs(fabs(a)-fabs(c)) <= 33/65536) m *= 2;
    if (fabs(fabs(c)-fabs(d)) <= 33/65536) n *= 2;
        
    for(uint32_t i = 0; i < contours.size(); i++) {
        Contour& contour = contours[i];  
        for(uint32_t j = 0; j < contour.coords.size(); j++) {
            Vector2& v = contour.coords[j];        
            float x = m * ((a/m)*v[0] + (c/m)*v[1] + e);
            float y = n * ((b/n)*v[0] + (d/n)*v[1] + f);
            v[0] = x;
            v[1] = y;
        }
    }   
}
