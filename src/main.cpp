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

#include "stats.h"

#include "math/vector.h"
#include "math/matrix.h"

#include "sphere.h"
#include "scene.h"
#include "camera.h"
#include "ray.h"
#include "intersection.h"
#include "image/image.h"
#include "boolean.h"
#include "mesh.h"
#include "box.h"
#include "cylinder.h"
#include "tetrahedron.h"
#include "tessalation.h"
#include "paths/circle.h"
#include "extrusion.h"
#include "pixelstore.h"
#include "raytracer.h"
#include "bsp.h"
#include "necklace.h"
#include "wireframe.h"

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"

#include "lights/pointlight.h"
#include "lights/spotlight.h"
#include "lights/arealight.h"

#include "materials/materials.h"

using namespace std;

void testScene4() {
    Stats::getUniqueInstance()->clear();
    Scene scene;
 //   scene.setEnvironmentMap("stbp.tga");

    Material mat = MATERIAL_SHINY_BLUE;
    //mat.setBumpmap("stregerbump.tga",2.0);
    //mat.setTexturemap("blueplasma.tga");
    mat.setRepeatX(10);
    mat.setRepeatY(4);
    Sphere s1 = Sphere(Vector(200,50,200),130.0,mat);
    Sphere s2 = Sphere(Vector(-200,50,200),130.0,mat);
    Sphere s3 = Sphere(Vector(200,50,-200),130.0,mat);
    Sphere s4 = Sphere(Vector(-200,50,-200),130.0,mat);
    Sphere s5 = Sphere(Vector(0,150,0),130.0,mat);
    //Extrusion c1 = Extrusion(Vector(0,0,0),Vector(0,200,0),130,10,MATERIAL_SHINY_RED);
    
/*    scene.addObject(&s1);
    scene.addObject(&s2);
    scene.addObject(&s3);
    scene.addObject(&s4);
    scene.addObject(&s5);
  */  
  //  scene.addObject(&c1);

    Circle circle1 = Circle(Vector(0,75,0),200,Vector(0,1,0));
    Spiral spiral = Spiral(&circle1,100,10);
    Spiral spiral2 = Spiral(&spiral,30,100,0.5);

    Extrusion* torus = new Extrusion(circle1,100,50,30,mat);
    //scene.addObject(torus);

    Tessalation* tet = new Tessalation(Vector(0,100,0),250,1,MATERIAL_SHINY_BLUE);
    Wireframe* wire = new Wireframe(tet,20.0,MATERIAL_SHINY_BLUE);
    scene.addObject(wire);

    //Extrusion* tube = new Extrusion(spiral,10,16,200,MATERIAL_CHROME);
   // scene.addObject(tube); 


    // Use this when making fog later.
    /*
    for(int x = -300; x <= 300; x += 60) {
	for(int z = -300; z <= 300; z += 60) {
	    Box* b = new Box(Vector(x,0,z),40.0,40.0,40.0,MATERIAL_SHINY_BLUE);
	    Sphere* s = new Sphere(Vector(x,0,z),20.0,MATERIAL_SHINY_BLUE);
	    scene.addObject(b);
	}
    }
    */
    
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    Spotlight spotlight2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-4000,4000,4000),Vector(1,-1,-1),1000,40,0.10);
    Arealight area2 = Arealight(Vector(4000,4000,4000),Vector(-1,-1,-1),1000,40,0.10);
    //scene.addLight(&area1);
    //scene.addLight(&area2);
    //scene.addLight(&spotlight2);
    scene.addLight(&light1);
    scene.addLight(&light3);
    
    Box b = Box(Vector(-300,-200,-300),Vector(300,-150,300),MATERIAL_SHINY_GREEN); /* Floor */
    scene.addObject(&b);
    
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

    raytracer.render(&scene,img,space);
    
    img->save("out.tga");
    delete img;
    Stats::getUniqueInstance()->dump();
}

int main(int argc, char *argv[]) {
    PixelStore::test();
    BSP::test();
    cout << "Tests done." << endl;
    // Test scene stuff
    testScene4();

    return EXIT_SUCCESS;
}


