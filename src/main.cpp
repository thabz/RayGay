#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>
//#include <unistd.h>

#include "stats.h"

#include "importer.h"

#include "math/vector.h"
#include "math/matrix.h"

#include "scene.h"
#include "image/image.h"
#include "space/kdtree.h"

#include "photonrenderer.h"
#include "raytracer.h"

#include "renderersettings.h"


using namespace std;

void work(string scenefile, string outputfile,int jobs) {
    Stats::getUniqueInstance()->clear();
    //Stats::getUniqueInstance()->disable();

    cout << "Reading " << scenefile << endl;
    Importer importer(scenefile);
    Scene* scene = importer.getScene();
    cout << "Done." << endl;

    Vector2 img_size = importer.getImageSize();
    Image* img = new Image(int(img_size[0]),int(img_size[1]));

    Matrix n = Matrix::matrixRotate(Vector(1,1,0),21.0);
    //   n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    //scene->transform(n);

    SpaceSubdivider* space = new KdTree();
    scene->initSpace(space);

    RendererSettings* renderersettings = importer.getRendererSettings();
    Renderer* renderer;

    if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
	renderer = new PhotonRenderer(renderersettings,scene,space);
    } else if (renderersettings->renderertype == RendererSettings::RAYTRACER) {
	renderer = new Raytracer(renderersettings,scene,space);
    } else {
	cout << "main.cpp: Unknown renderer" << endl;
	exit(EXIT_FAILURE);
    }
    renderer->init();
    renderer->render(img);

    img->save(outputfile);
    delete img;
    Stats::getUniqueInstance()->dump();
}

void print_usage() {
    cout << "Usage: tracer [OPTION...] SCENEFILENAME OUTPUTFILENAME" << endl;
    cout << "       -j NUM               Number of threads to run" << endl;
    cout << "       -h                   Show this help message" << endl;
    cout << "       -v                   Show current versionnumber" << endl;

}

int main(int argc, char *argv[]) {
    // Use getopt to parse arguments.
    int c;
    opterr = 0;
    int jobs = 1;
    while ((c = getopt (argc, argv, "vhj:")) != -1) {
	switch(c) {
	    case 'h':
		print_usage();
		return EXIT_SUCCESS;
	    case 'v':
		cout << "Raygay 0.1" << endl;
		return EXIT_SUCCESS;
	    case 'j':
		if (sscanf(optarg,"%u",&jobs) != 1 || jobs < 1) {
		    cerr << "Illegal -j option" << endl;
		    print_usage();
		    return EXIT_FAILURE;
		};
		break;
	    case '?':
		cerr << "Unknown option -" << char(optopt) << endl << endl;
		print_usage();
		return EXIT_FAILURE;
	    default:
		return EXIT_FAILURE;
	}
    }
    
    string scenefile;
    string outfile;
    if (optind != argc - 2) {
	cerr << "Not enough arguments" << endl << endl;
	print_usage();
	return EXIT_FAILURE;
    } else {
	scenefile = string(argv[optind]);
	outfile = string(argv[optind+1]);
    }
    srand(1); // Make sure rand is seeded consistently.
    cout << "Number of threads: " << jobs << endl;
    work(scenefile,outfile,jobs); 
    return EXIT_SUCCESS;
}


