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
#include "pointlight.h"
#include "boolean.h"
#include "mesh.h"
#include "spotlight.h"
#include "constants.h"
#include "box.h"
#include "tetrahedron.h"
#include "linesegment.h"
#include "circle.h"
#include "cylinder.h"
#include "arealight.h"
#include "pixelstore.h"
#include "raytracer.h"
#include "bsp.h"
#include "circle.h"
#include "necklace.h"
#include "spiral.h"

using namespace std;

#define red Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30)
#define green Material(RGB(0.2,1.0,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30)
#define blue Material(RGB(0.0,0.2,1.0),0.80,RGB(1.0,1.0,1.0),0.20,10)
#define chrome Material(RGB(0.8,0.8,0.8),0.7,RGB(1.0,1.0,1.0),0.80,40)

void testScene4() {
    Scene scene;

    Material mat = blue;
 //   mat.setTexturemap("earth.jpg");
    Sphere s1 = Sphere(Vector(200,50,200),130.0,chrome);
    Sphere s2 = Sphere(Vector(-200,50,200),130.0,chrome);
    Sphere s3 = Sphere(Vector(200,50,-200),130.0,chrome);
    Sphere s4 = Sphere(Vector(-200,50,-200),130.0,chrome);
    Sphere s5 = Sphere(Vector(0,150,0),130.0,chrome);
    //Cylinder c1 = Cylinder(Vector(0,0,0),Vector(0,200,0),130,10,red);
 /*   scene.addObject(&s1);
    scene.addObject(&s2);
    scene.addObject(&s3);
    scene.addObject(&s4);
    scene.addObject(&s5);
    scene.addObject(&c1);*/

    for(int x = -10; x <= 10; x += 5) {
       for(int y = -10; y <= 10; y += 5) {
           for(int z = -10; z <= 10; z += 5) {
	      Sphere* sx = new Sphere(Vector(x*20,y*20+50,z*20),10,chrome);
	//      scene.addObject(sx);
	   }
 	}
    }

    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Spiral spiral = Spiral(&circle1,100,10);
    Spiral spiral2 = Spiral(&spiral,30,100,0.5);

    Cylinder* torus = new Cylinder(circle1,100,30,20,blue);
    scene.addObject(torus);

    //Cylinder* tube = new Cylinder(spiral,10,16,200,chrome);
   // scene.addObject(tube); 
    
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    Spotlight spotlight2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-2000,2000,2000),Vector(1,-1,-1),1000,50,0.10);
    Arealight area2 = Arealight(Vector(2000,2000,2000),Vector(-1,-1,-1),1000,40,0.10);
    //scene.addLight(&area1);
    //scene.addLight(&area2);
    //scene.addLight(&spotlight2);
    scene.addLight(&light1);
    scene.addLight(&light3);
    
    //Box b = Box(Vector(-400,-200,-400),Vector(400,-150,400),green); /* Floor */
 //   scene.addObject(&b);
    
    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene.transform(n);

    scene.setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1));
    scene.setCamera(&cam);
    
    Image* img = new Image(640,480);
    //SpaceSubdivider* space = new Hierarchy();
    SpaceSubdivider* space = new BSP();

    Raytracer raytracer = Raytracer();

    time_t beginTime = time(NULL);
    raytracer.render(&scene,img,space);
    
    printf("Rendering took %ld seconds.\n",time(NULL) - beginTime);
    img->save("out.tga");
    delete img;
}

int main(int argc, char *argv[]) {
    Vector::test();
    Sphere::test();
    Boolean::test();
    BoundingBox::test();
    Matrix::test();
    Mesh::test();
    Box::test();
    Linesegment::test();
    Circle::test();
    Spiral::test();
    Cylinder::test();
    PixelStore::test();
    BSP::test();
    cout << "Tests done." << endl;
    // Test scene stuff
    testScene4();

    return EXIT_SUCCESS;
}



