#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>
#include <vector>
#include <unistd.h>

#include "stats.h"
#include "exception.h"

#include "importer.h"

#include "math/vector.h"
#include "math/matrix.h"

#include "scene.h"
#include "image/image.h"
#include "image/texture.h"
#include "space/kdtree.h"

#include "photonrenderer.h"
#include "raytracer.h"

#include "photon/globalphotonmap.h"
#include "photon/causticsmap.h"
#include "photon/photontracer.h"
#include "photon/irradiancecache.h"

#include "parser/parser.h"

#include "renderersettings.h"
#include "renderjobs.h"

using namespace std;

extern FILE* yyin;
extern void yyparse();
extern void init_parser();

void preparePhotonMaps(Scene* scene,
	               KdTree* space,
		       RendererSettings* renderersettings,
	               GlobalPhotonMap** globalphotonmap,
		       CausticsMap** causticsphotonmap,
		       IrradianceCache** irradiancecache) {

    (*globalphotonmap) = new GlobalPhotonMap(renderersettings->global_photons_num,renderersettings->estimate_radius,renderersettings->estimate_samples);
    (*causticsphotonmap) = new CausticsMap(renderersettings->caustic_photons_num,renderersettings->estimate_radius,renderersettings->estimate_samples); 

    PhotonTracer* photontracer = new PhotonTracer(scene,space,(*globalphotonmap),(*causticsphotonmap));

    cout << "Tracing photons..." << endl;
    photontracer->trace(renderersettings->threads_num);
    cout << "Done." << endl;

    
    cout << "Balancing photonmaps..." << endl;
    Stats::getUniqueInstance()->beginTimer("Balance photonmaps");
    int total_photons_num = renderersettings->global_photons_num + renderersettings->caustic_photons_num;
    (*globalphotonmap)->scale_photon_power(1.0/double(total_photons_num));
    (*globalphotonmap)->balance();
    (*causticsphotonmap)->scale_photon_power(1.0/double(total_photons_num));
    (*causticsphotonmap)->balance();
    Stats::getUniqueInstance()->endTimer("Balance photonmaps");
    cout << "Done." << endl;

    cout << "Precomputing irradiances..." << endl;
    (*globalphotonmap)->preComputeIrradiances(4,renderersettings->threads_num);
    cout << "Done." << endl;

    delete photontracer;
    
    //irradiance_cache = new IrradianceCache(space->getWorldBoundingBox(),5);
    BoundingBox bbox = BoundingBox(Vector(-733,-733,-733),Vector(733,733,733));
    (*irradiancecache) = new IrradianceCache(bbox,renderersettings->cache_tolerance);
}

void prepareJobPool(RenderJobPool* pool, Image* img, int cell_size) {
    RenderJob job;
    for(int y = 0; y < (img->getHeight() / cell_size)+1; y++) {
	job.begin_y = y*cell_size;
	job.end_y = min((y+1)*cell_size,img->getHeight());
	for(int x = 0; x < (img->getWidth() / cell_size)+1; x++) {
	    job.begin_x = x*cell_size;
	    job.end_x = min((x+1)*cell_size, img->getWidth());
	    if (job.begin_x < img->getWidth() &&
		job.begin_y < img->getHeight() &&
		job.begin_x < job.end_x && 
		job.begin_y < job.end_y) {
		pool->addJob(job);
	    }
	}
    }
}

void* renderThreadDo(void* obj) {
    Renderer* tracer = (Renderer*) obj;
    tracer->run();
    return NULL;
}

void work(string scenefile, string outputfile,int jobs) {
    Stats::getUniqueInstance()->clear();
    //Stats::getUniqueInstance()->disable();
    Stats::getUniqueInstance()->put(STATS_THREADS,jobs);

    Importer importer(scenefile);
    Scene* scene = importer.getScene();

    Vector2 img_size = importer.getImageSize();
    Image* img = new Image(int(img_size[0]),int(img_size[1]));

    //Matrix n = Matrix::matrixRotate(Vector(1,1,0),21.0);
    //   n = n * Matrix::matrixTranslate(Vector(0,0,-500));
    //scene->transform(n);

    cout << "Preparing space..." << endl;
    KdTree* space = new KdTree();
    scene->initSpace(space);
    cout << "Done." << endl;

    RendererSettings* renderersettings = importer.getRendererSettings();
    renderersettings->threads_num = jobs;

    // Prepare photon maps if necessary
    GlobalPhotonMap* globalphotonmap;
    CausticsMap* causticsmap;
    IrradianceCache* irradiancecache;
    if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
	preparePhotonMaps(scene,space,renderersettings,&globalphotonmap,&causticsmap,&irradiancecache);
    }

    // Create and prepare job pool
    RenderJobPool* job_pool = new RenderJobPool();
    prepareJobPool(job_pool,img,64);

    // Spawn renderer threads
    Stats::getUniqueInstance()->beginTimer("Rendering");
    Renderer* renderers[renderersettings->threads_num];
    pthread_t threads[renderersettings->threads_num];
    for(int i = 0; i < renderersettings->threads_num; i++) {
	if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
	    renderers[i] = new PhotonRenderer(renderersettings,img,scene,space,job_pool,i,globalphotonmap,causticsmap,irradiancecache);
	} else if (renderersettings->renderertype == RendererSettings::RAYTRACER) {
	    renderers[i] = new Raytracer(renderersettings,img,scene,space,job_pool,i);
	} else {
	    throw_exception("Unknown renderer");
	}
	pthread_create(&threads[i], NULL, renderThreadDo, renderers[i]);
    }

    // Wait for threads to finish
    for(int i = 0; i < renderersettings->threads_num; i++) {
	pthread_join(threads[i], NULL);
    }
    Stats::getUniqueInstance()->endTimer("Rendering");

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
    int parser_to_use = 0;
    while ((c = getopt (argc, argv, "nvhj:")) != -1) {
	switch(c) {
	    case 'n':
		parser_to_use = 1;
		break;
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

    try {
	if (parser_to_use == 1) {
	    init_parser();
	    yyin = fopen(scenefile.c_str(),"r");
	    yyparse();
	} else {
	    work(scenefile,outfile,jobs); 
	}
    } catch (Exception e) {
	cout << "Exception: " << e.getMessage() 
	    << " at " << e.getSourceFile() << ":" << e.getSourceLine() << endl;
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}


