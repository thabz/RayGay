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

#include "scene.h"
#include "image/image.h"
#include "bsp.h"
#include "kdtree.h"

#include "photonrenderer.h"
#include "raytracer.h"

#include "photonsettings.h"


using namespace std;

void work(string scenefile, string outputfile) {
    Stats::getUniqueInstance()->clear();
    cout << "Reading " << scenefile << endl;
    Importer importer(scenefile);
    Scene* scene = importer.getScene();
    cout << "Done." << endl;

    Vector2 img_size = importer.getImageSize();
    Image* img = new Image(int(img_size[0]),int(img_size[1]));

    Matrix n = Matrix::matrixRotate(Vector(1,1,1),15.0);
 //   n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    scene->transform(n);

    SpaceSubdivider* space = new BSP();
    scene->initSpace(space);

    PhotonSettings* photonsettings = importer.getPhotonSettings();

#define PHOTON_CODE

#ifdef PHOTON_CODE
    Renderer* renderer = new PhotonRenderer(photonsettings);
#else
    Renderer* renderer = new Raytracer();
#endif
    renderer->init(scene,space);
    renderer->render(scene,img,space);

    img->save(outputfile);
    delete img;
    Stats::getUniqueInstance()->dump();
}

int main(int argc, char *argv[]) {
    cout << "Raygay version 0.1" << endl;
    if (argc < 3) {
	cout << "USAGE: tracer <scenefile.ray> <outputfile.tga>" << endl;
	return EXIT_FAILURE;
    }
    srand(1); // Make sure rand is seeded consistenly.
    work(string(argv[1]),string(argv[2])); 
    return EXIT_SUCCESS;
}


