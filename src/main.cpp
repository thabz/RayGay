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
#include "lightsource.h"
#include "boolean.h"
#include "boxobject.h"

using namespace std;

#define red Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30)
#define green Material(RGB(0.2,1.0,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30)
#define blue Material(RGB(0.0,0.2,1.0),0.75,RGB(1.0,1.0,1.0),0.20,10)
#define chrome Material(RGB(0.8,0.8,0.8),0.7,RGB(1.0,1.0,1.0),0.80,40)

void testScene4() {
    Scene scene;

#define NUM 6
#define SIZE double(300.0)
    for (int x = 0; x < NUM; x++) {
	for (int y = 0; y < NUM; y++) {
	    for (int z = 0; z < NUM; z++) {
		Material mat = Material(RGB(double(x)/double(NUM), double(y)/double(NUM), double(z)/double(NUM)),0.90,RGB(1.0,1.0,1.0),0.70,70);
		Sphere* s = new Sphere(Vector(SIZE*double(x)/double(NUM) - SIZE/2, SIZE*double(y)/double(NUM) - SIZE/2, SIZE*double(z)/double(NUM) - SIZE/2),20.0,mat);
		scene.addObject(s);
	    }
	}
    }

    Lightsource light1 = Lightsource(Vector(-4000,4000,4000));
    Lightsource light2 = Lightsource(Vector(4000,3000,6000));
    scene.addLight(&light1);
    scene.addLight(&light2);

    /*
    
    Boxobject b = Boxobject(Box(Vector(-300,-150,-300),Vector(300,-100,300)),green);
    scene.addObject(&b);
*/

    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-40.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,200));
    scene.transform(n);

    scene.setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1),scene);
    Image* img = new Image(640,480);
    time_t beginTime = time(NULL);
    cam.render(img);
    printf("Rendering took %ld seconds.\n",time(NULL) - beginTime);
    img->save("hej.rgb");
    delete img;
}

int main(int argc, char *argv[]) {
    Sphere::test();
    Boolean::test();
    Box::test();
    Matrix::test();
    // Test scene stuff
    testScene4();

    return EXIT_SUCCESS;
}



