
#include "contour.h"

#include <algorithm>
#include "functions.h"

using namespace std;

bool Contours::isInside(const Vector2& p, double x_max, double size) const {
    // TODO: Use zero-winding rule as described by 
    // Apple in http://developer.apple.com/textfonts/TTRefMan/RM02/Chap2.html#distinguishing
    
    double y = p.y();
    double x_min = p.x();
    int intersections = 0;
    for(uint32_t m = 0; m < contours.size(); m++) {
        const Contour& contour = contours[m];    
        Vector2 c0 = contour.coords[0] * size;
        for(uint32_t k = 1; k <= contour.coords.size(); k++) {
            uint32_t j = k % contour.coords.size();    
            Vector2 c1 = contour.coords[j] * size;
            if (contour.onCurve[j]) {
                double root;
                if (IS_NEQUAL(c0.y(),c1.y()) && IS_NEQUAL(y,c0.y()) && IS_NEQUAL(y, c1.y()) 
                    && IS_NEQUAL(c0.x(), x_min) && IS_NEQUAL(c1.x(), x_min))  {
                    int n = intersect(x_min, y, c0, c1, &root);
                    if (n > 0) {
                        intersections++;    
                    }
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
                    intersections++;    
                }
                c0 = c2;
            }
        }
    }

    return intersections % 2 == 1;
}

vector<double> Contours::rasterize(double x_min, double x_max, double y, double size) const {
    // TODO: Use zero-winding rule as described by 
    // Apple in http://developer.apple.com/textfonts/TTRefMan/RM02/Chap2.html#distinguishing

    vector<double> result;        

    for(uint32_t m = 0; m < contours.size(); m++) {
        const Contour& contour = contours[m];    
        Vector2 c0 = contour.coords[0] * size;
        for(uint32_t k = 1; k <= contour.coords.size(); k++) {
            uint32_t j = k % contour.coords.size();    
            Vector2 c1 = contour.coords[j] * size;
            if (contour.onCurve[j]) {
                double root;
                if (IS_NEQUAL(c0.y(),c1.y()) && IS_NEQUAL(y,c0.y()) && IS_NEQUAL(y, c1.y()) 
                    && IS_NEQUAL(c0.x(), x_min) && IS_NEQUAL(c1.x(), x_min))  {
                int n = intersect(x_min, y, c0, c1, &root);
                if (n > 0) {
                    result.push_back(root+x_min);    
                }
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
                    result.push_back(roots[i]+x_min);    
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
    if (IS_ZERO(A) && IS_ZERO(B)) return 0;
    int num = Math::solveQuadratic(A,B,C,roots);

    int n = 0;
    for(int i = 0; i < num; i++) {
        double t = roots[i]; 
        // TODO: Skal nok være t < 1.0, så vi ikke løber ind i dobbelskæringer.
        // Se plottet af @ på http://localhost/blog/files/filled-text.png
        if (t > 0 && t <= 1.0) {
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
int Contours::intersect(double x_min, double y, const Vector2& a, const Vector2& b, double* result) 
{
     // Skip horizonal lines
     if (IS_EQUAL(a[1], b[1])) return 0;        
        
     double t = (y - b[1]) / (a[1] - b[1]);
     if (t > 0 && t <= 1.0) {
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
