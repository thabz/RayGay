#include <iostream>
#include <stdlib.h>
#include "src/vector.h"
#include "src/matrix.h"
#include "src/sphere.h"

using namespace std;

void testSphereIntersection() {
    Sphere s = Sphere(Vector(0,0,0),20.0);
    cout << "Sphere: " << s << endl;
    Ray r = Ray(Vector(0,0,20),Vector(0,1,0));
    Intersection i = s.intersect(r);
    cout << "Intersection at " << i.point;
}

