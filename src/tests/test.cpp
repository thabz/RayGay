/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Copyright 2003 by Jesper Christensen <jesper@kalliope.org>
 *
 * Added Thu Apr 17 2003
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <stdlib.h>
#include <vector>
#include <time.h>
#include <stdio.h>

#include "vector.h"
#include "matrix.h"
#include "sphere.h"
#include "scene.h"
#include "camera.h"
#include "ray.h"
#include "intersection.h"
#include "image.h"
#include "boolean.h"
#include "mesh.h"
#include "constants.h"
#include "box.h"
#include "tetrahedron.h"
#include "tessalation.h"
#include "paths/circle.h"
#include "cylinder.h"
#include "pixelstore.h"
#include "raytracer.h"
#include "bsp.h"
#include "necklace.h"

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"

#include "lights/pointlight.h"
#include "lights/spotlight.h"
#include "lights/arealight.h"

#include "materials/materials.h"

using namespace std;

int main(int argc, char *argv[]) {
    Sphere::test();
    Boolean::test();
    BoundingBox::test();
    Mesh::test();
    Box::test();
    Cylinder::test();
    PixelStore::test();
    Tetrahedron::test();
    Tessalation::test();
    BSP::test();
    cout << "Tests done." << endl;

    return EXIT_SUCCESS;
}


