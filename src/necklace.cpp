
#include "necklace.h"
#include "paths/path.h"
#include "sphere.h"
#include "spacesubdivider.h"

/**
 * @param path The path to place the spheres along
 * @param num Number of spheres to place in the scene
 * @param r Radius of the spheres
 * @param material Material of the spheres
 */
Necklace::Necklace(const Path& path, int num, double r, const Material& material) {
    Vector* c = new Vector[num];
    path.getPoints(num,c);
    _num = num;

    for (int i = 0; i < num; i++) {
	Sphere* s = new Sphere(c[i],r,material);
	addObject(s);
    }
}
