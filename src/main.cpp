
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
#include "camera.h"
#include "image/image.h"
#include "image/texture.h"
#include "space/kdtree.h"

#include "photonrenderer.h"
#include "raytracer.h"
#include "pathtracer.h"

#include "photon/globalphotonmap.h"
#include "photon/causticsmap.h"
#include "photon/photontracer.h"
#include "photon/irradiancecache.h"

#include "parser/assignments.h"    
#include "parser/floatnodes.h"    
#include "parser/syntaxnode.h"    
#include "parser/vectornodes.h"    
#include "parser/rgbnodes.h"    
#include "parser/langnodes.h"    
#include "parser/transformationnodes.h"    
#include "parser/lightnodes.h"    
#include "parser/cameranode.h"    
#include "parser/parser.h"

#include "renderersettings.h"
#include "renderjobs.h"

#include "window.h"

using namespace std;

extern FILE* yyin;
extern void yyparse();
extern void run_interpreter();
extern void delete_interpreter();
extern void init_parser(string filename);
extern Vector2 getImageSize();
extern RendererSettings* getRendererSettings();

//Assignments* global_assigments = new Assignments();

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

void render_frame(int cur_frame, string outputfile, int jobs) {

    Stats::getUniqueInstance()->clear();
    //Stats::getUniqueInstance()->disable();
    Stats::getUniqueInstance()->put(STATS_THREADS,jobs);


    RendererSettings* renderersettings = getRendererSettings();

    Scene* scene = new Scene();
    Environment::getUniqueInstance()->setScene(scene);

    Assignments::getUniqueInstance()->setNamedFloat("frame",double(cur_frame));
    Assignments::getUniqueInstance()->setNamedFloat("clock",double(cur_frame)/double(renderersettings->anim_frames));
    run_interpreter();


    if (renderersettings->renderertype == RendererSettings::NONE) {
	return;
    }

    Vector2 img_size = getImageSize();
    scene->getCamera()->setImageSize(int(img_size[0]),int(img_size[1]));
    Image* img = new Image(int(img_size[0]),int(img_size[1]));
    Environment::getUniqueInstance()->getPreviewWindow()->setImage(img);

    if (scene->getObjects().size() == 0) {
	throw_exception("No objects in scene.");
    }

    cout << "Preparing space..." << endl;
    KdTree* space = new KdTree();
    scene->initSpace(space);
    cout << "Done." << endl;

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

    if (renderersettings->anim_frames == 1) {
	cout << "Still render (" << img->getWidth() << "x" << img->getHeight() << ")" << endl;
    } else {
	cout << "Animation render (" << img->getWidth() << "x" << img->getHeight() << ", " << renderersettings->anim_frames << "frames)" << endl;

    }


    Stats::getUniqueInstance()->beginTimer("Rendering");

    if (renderersettings->threads_num == 1) {
	// If only one thread is wanted we don't spawn a render-thread
	// but keep the rendering in-process. This allows for profiling 
	// with gprof, which doesn't support profiling of multithreaded 
	// programs.
	Renderer* renderer = NULL;
	if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
	    renderer = new PhotonRenderer(renderersettings,img,scene,space,job_pool,0,globalphotonmap,causticsmap,irradiancecache);
	} else if (renderersettings->renderertype == RendererSettings::RAYTRACER) {
	    renderer = new Raytracer(renderersettings,img,scene,space,job_pool,0);
	} else if (renderersettings->renderertype == RendererSettings::PATHTRACER) {
	    renderer = new Pathtracer(renderersettings,img,scene,space,job_pool,0);
	}
	renderer->run();
	delete renderer;
    } else {
	// Spawn renderer threads
	Renderer* renderers[renderersettings->threads_num];
	pthread_t threads[renderersettings->threads_num];
	for(int i = 0; i < renderersettings->threads_num; i++) {
	    if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
		renderers[i] = new PhotonRenderer(renderersettings,img,scene,space,job_pool,i,globalphotonmap,causticsmap,irradiancecache);
	    } else if (renderersettings->renderertype == RendererSettings::RAYTRACER) {
		renderers[i] = new Raytracer(renderersettings,img,scene,space,job_pool,i);
	    } else if (renderersettings->renderertype == RendererSettings::PATHTRACER) {
		renderers[i] = new Pathtracer(renderersettings,img,scene,space,job_pool,i);
	    } else {
		throw_exception("Unknown renderer");
	    }
	    pthread_create(&threads[i], NULL, renderThreadDo, renderers[i]);
	}

	// Wait for threads to finish
	for(int i = 0; i < renderersettings->threads_num; i++) {
	    pthread_join(threads[i], NULL);
	    delete renderers[i];
	}
    }
    Stats::getUniqueInstance()->endTimer("Rendering");

    img->save(outputfile);
    delete img;
    delete space;
    delete scene;
    delete job_pool;
    Stats::getUniqueInstance()->dump();
}

void work(string scenefile, string outputfile, int jobs) {
    char original_working_dir[1024];
    getcwd(original_working_dir,1024);

    init_parser(scenefile);
    yyparse();

    chdir(original_working_dir);
    
    int frames_num = getRendererSettings()->anim_frames;
#ifdef HAVE_GTK
    Vector2 size = getImageSize();
    PreviewWindow* preview_window = new PreviewWindow(int(size[0]),int(size[1]));
    Environment::getUniqueInstance()->setPreviewWindow(preview_window);
    preview_window->run();
#endif
    if (frames_num == 1) {
	render_frame(0,outputfile,jobs);
    } else {
	char file_prefix[50];
	for (int frame = 0; frame < frames_num; frame++) {
	    cout << "Rendering frame " << frame << "/" << frames_num << endl;
	    sprintf(file_prefix,"%05d",frame);
	    render_frame(frame,file_prefix + outputfile,jobs);
	}
    }
    delete_interpreter();
#ifdef HAVE_GTK
    preview_window->stop();
#endif
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

    try {
	work(scenefile,outfile,jobs); 
    } catch (Exception e) {
	cout << "Exception: " << e.getMessage() 
	    << " at " << e.getSourceFile() << ":" << e.getSourceLine() << endl;
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}


