
#include "necklace.h"
#include "paths/path.h"
#include "sphere.h"
#include "spacesubdivider.h"

Necklace::Necklace(const Path& path, int num, double r, const Material& material) {
    Vector* c = new Vector[num];
    path.getPoints(num,c);
    _num = num;

    for (int i = 0; i < num; i++) {
	Sphere* s = new Sphere(c[i],r,material);
	objects.push_back(s);
    }
}

void Necklace::transform(const Matrix& m) {
    for(int i = 0; i < _num; i++) {
	objects[i]->transform(m);
    }
}
	
void Necklace::addParts(SpaceSubdivider* space) {
    for(int i = 0; i < _num; i++) {
	space->addObject(objects[i]);
    }
}
