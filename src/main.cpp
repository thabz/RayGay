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

#include "importer.h"

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
#include "3ds.h"
#include "cylinder.h"
#include "tetrahedron.h"
#include "tessalation.h"
#include "paths/circle.h"
#include "extrusion.h"
#include "raytracer.h"
#include "bsp.h"
#include "necklace.h"
#include "wireframe.h"
#include "teapot.h"

#include "paths/linesegment.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "paths/bezierspline.h"

#include "lights/pointlight.h"
#include "lights/spotlight.h"
#include "lights/arealight.h"

#include "materials/materials.h"

using namespace std;

void testScene4() {
    Stats::getUniqueInstance()->clear();
    Scene scene;
    //scene.setFog(RGB(0.7,0.7,0.7),1500);
 //   scene.setEnvironmentMap("stbp.tga");

    Material mat = MATERIAL_SHINY_BLUE;
    //mat.setBumpmap("stregerbump.tga",2.0);
    mat.setTexturemap("checkboard.tga");
    mat.setRepeatX(8);
    mat.setRepeatY(8);

    Material red = MATERIAL_DULL_RED;
    Sphere* s = new Sphere(Vector(0,0,0),150.0,mat);
    scene.addObject(s);
    
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
   // light1.setAttenuation(4000,3);
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    //light3.setAttenuation(4000,3);
    Spotlight spotlight2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-4000,4000,4000),Vector(1,-1,-1),1000,32,0.10);
    Arealight area2 = Arealight(Vector(4000,4000,4000),Vector(-1,-1,-1),1000,32,0.10);
    //scene.addLight(&area1);
    //scene.addLight(&area2);
    //scene.addLight(&spotlight2);
    scene.addLight(&light1);
    scene.addLight(&light3);
    
    Material chrome = MATERIAL_CHROME;
    chrome.enableGloss(9,2.0);

    Box b = Box(Vector(-500,-200,-500),Vector(500,-150,500),chrome); /* Floor */
    scene.addObject(&b);
    
    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene.transform(n);

    scene.setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1));
    cam.enableAdaptiveSupersampling(4);
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

void test_columns() {
    Stats::getUniqueInstance()->clear();
    Scene scene;
    
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
   // light1.setAttenuation(4000,3);
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    //light3.setAttenuation(4000,3);
    Spotlight spotlight2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-4000,4000,4000),Vector(1,-1,-1),1000,40,0.10);
    Arealight area2 = Arealight(Vector(4000,4000,4000),Vector(-1,-1,-1),1000,40,0.10);
    //scene.addLight(&area1);
    //scene.addLight(&area2);
    //scene.addLight(&spotlight2);
    scene.addLight(&light1);
    scene.addLight(&light3);

#define num 20
#define top 100
#define cyl_radii 20   
#define column_radius 100    
#define box_height 20    

    /*
    Circle* bottom_c = new Circle(Vector(0,-150,0),column_radius,Vector(0,1,0));
    Circle* top_c = new Circle(Vector(0,top,0),column_radius,Vector(0,1,0));
    Vector top_p[num];
    Vector bottom_p[num];
    top_c->getPoints(num,top_p);
    bottom_c->getPoints(num,bottom_p);
    for(int i = 0; i < num; i++) {
	Cylinder* cyl = new Cylinder(bottom_p[i],top_p[i],cyl_radii,MATERIAL_DULL_BLUE);
	scene.addObject(cyl);
    }
    Box* bottom_box = new Box(Vector(-column_radius-cyl_radii,-150,-column_radius-cyl_radii),Vector(column_radius+cyl_radii,-150+box_height,column_radius+cyl_radii),MATERIAL_DULL_BLUE); 
    scene.addObject(bottom_box);
    Box* top_box = new Box(Vector(-column_radius-cyl_radii,top,-column_radius-cyl_radii),Vector(column_radius+cyl_radii,top+box_height,column_radius+cyl_radii),MATERIAL_DULL_BLUE); 
    scene.addObject(top_box);
*/
    Material mat = MATERIAL_PORCELAIN;
    
    Box b = Box(Vector(-300,-200,-300),Vector(300,-150,300),MATERIAL_SHINY_GREEN); // Floor
    scene.addObject(&b);

    Teapot* teapot = new Teapot(Vector(0,-100,0),300,10,mat);
    scene.addObject(teapot);
    
    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    //scene.transform(n);

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

void test_3ds() {
    Stats::getUniqueInstance()->clear();
    Scene scene;
    
    Pointlight light1 = Pointlight(Vector(-4000,4000,4000));
   // light1.setAttenuation(4000,3);
    Pointlight light3 = Pointlight(Vector(4000,4000,4000));
    //light3.setAttenuation(4000,3);
    Spotlight spotlight2 = Spotlight(Vector(500,500,500),Vector(0,0,-1),DEG2RAD(10.0),DEG2RAD(8.0));
    Arealight area1 = Arealight(Vector(-4000,4000,4000),Vector(1,-1,-1),1000,32,0.10);
    Arealight area2 = Arealight(Vector(4000,4000,4000),Vector(-1,-1,-1),1000,32,0.10);
    scene.addLight(&area1);
    //scene.addLight(&area2);
    //scene.addLight(&spotlight2);
    //scene.addLight(&light1);
    //scene.addLight(&light3);
    
    Box b = Box(Vector(-300,-200,-300),Vector(300,-150,300),MATERIAL_SHINY_GREEN); /* Floor */
    scene.addObject(&b);

    //ThreeDS* egg = new ThreeDS("../3ds/swan.3ds",10,MATERIAL_DULL_BLUE);
    ThreeDS* egg = new ThreeDS("../3ds/egg-chair.3ds",10,MATERIAL_DULL_BLUE);
    Matrix ma = Matrix::matrixRotate(Vector(1,0,0),90);
    ma = ma * Matrix::matrixTranslate(Vector(0,-135,0));
    egg->transform(ma);
    scene.addObject(egg);
    

/*    ThreeDS* chair = new ThreeDS("../3ds/3300.3ds",10,MATERIAL_DULL_BLUE);
    ma = Matrix::matrixRotate(Vector(1,0,0),90);
    ma = ma * Matrix::matrixTranslate(Vector(0,-135,0));
    chair->transform(ma);
    scene.addObject(chair);
  */  
    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene.transform(n);

    scene.setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1));
    cam.enableAdaptiveSupersampling(4);
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

void read() {
    Stats::getUniqueInstance()->clear();
    Importer importer("testscene.gay");
    Scene* scene = importer.getScene();

    Matrix n = Matrix::matrixRotate(Vector(1,1,0),-20.0);
    n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene->transform(n);

    scene->setBackgroundColor(RGB(0.1,0.1,0.3));

    Camera cam = Camera(Vector(0,0,1500),Vector(0,0,-1));
    cam.enableAdaptiveSupersampling(4);
    scene->setCamera(&cam);
    
    Image* img = new Image(640,480);
    //SpaceSubdivider* space = new Hierarchy();
    SpaceSubdivider* space = new BSP();

    Raytracer raytracer = Raytracer();
    raytracer.render(scene,img,space);
    
    img->save("out.tga");
    delete img;
    Stats::getUniqueInstance()->dump();

}

int main(int argc, char *argv[]) {
    BSP::test();
    cout << "Tests done." << endl;
    // Test scene stuff
 //   testScene4();
    read(); 

    return EXIT_SUCCESS;
}


