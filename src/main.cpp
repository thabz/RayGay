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

using namespace std;

#define red Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30)
#define green Material(RGB(0.2,1.0,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30)
#define blue Material(RGB(0.0,0.2,1.0),0.75,RGB(1.0,1.0,1.0),0.20,10)
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
    scene.addObject(&s1);
    scene.addObject(&s2);
    scene.addObject(&s3);
    scene.addObject(&s4);
    scene.addObject(&s5);

   /*
    Cylinder cyl = Cylinder(Vector(-200,-50,200),Vector(-200,50,200),100.0,6,blue);
    scene.addObject(&cyl);
    */
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    Spotlight light2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-2000,2000,2000),Vector(1,-1,-1),1000,50,0.10);
    Arealight area2 = Arealight(Vector(2000,2000,2000),Vector(-1,-1,-1),1000,40,0.10);
//    scene.addLight(&area1);
//    scene.addLight(&area2);
    scene.addLight(&light1);
    scene.addLight(&light2);
    //scene.addLight(&light3);
    
    Box b = Box(Vector(-300,-150,-300),Vector(300,-100,300),green); /* Floor */
    scene.addObject(&b);
    
    Box b2 = Box(Vector(100,-50,100),Vector(150,100,150),red);
    //scene.addObject(&b2);
    
    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene.transform(n);

    scene.setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1));
    scene.setCamera(&cam);
    
    Image* img = new Image(640,480);
    Hierarchy space = Hierarchy();

    Raytracer raytracer = Raytracer();

    time_t beginTime = time(NULL);
    raytracer.render(&scene,img,&space);
    
    printf("Rendering took %ld seconds.\n",time(NULL) - beginTime);
    img->save("out.png");
    delete img;
}

int main(int argc, char *argv[]) {
    Sphere::test();
    Boolean::test();
    BoundingBox::test();
    Matrix::test();
    Mesh::test();
    Box::test();
    Linesegment::test();
    Circle::test();
    Cylinder::test();
    PixelStore::test();
    cout << "Tests done." << endl;
    // Test scene stuff
    testScene4();

    return EXIT_SUCCESS;
}



