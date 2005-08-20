
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
#include "environment.h"

#include "math/vector.h"
#include "math/matrix.h"

#include "scene.h"
#include "cameras/camera.h"
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

#include "parser/parser.h"

#include "renderersettings.h"
#include "renderjobs.h"

#include "filters/filterstack.h"

#include "window.h"
#include "window-gtk.h"

using namespace std;

RendererSettings* renderer_settings = new RendererSettings();
Parser* parser = NULL;
PreviewWindow* preview_window = NULL;


RendererSettings* getRendererSettings() {
    return renderer_settings;
}

vector<Renderer*> active_renderers;

void abortRenderingCB() {
    cout << "Aborting render..." << endl;
    for(unsigned int i = 0; i < active_renderers.size(); i++) {
	active_renderers[i]->abort();
    }
}

enum windowToolkitId {
    NONE,
    GTK,
    COCOA
};

windowToolkitId availableWindowToolkit() {
#ifdef HAVE_GTK
    return GTK;
#endif
#ifdef HAVE_COCOA
    return COCOA;
#endif    
    return NONE;
}

void parser_assign_var(string name, double value) {

}

PreviewWindow* windowFactory(int w, int h) {
    PreviewWindow* result;
    cout << "Toolkit: " << availableWindowToolkit() << endl;
    switch (availableWindowToolkit()) {
	case GTK:
#ifdef HAVE_GTK
	    result = new PreviewWindowGTK(w, h, abortRenderingCB);
#endif	    
	    break;
	case COCOA:
	    throw_exception("COCOA Toolkit not supported");
	    break;
	case NONE:
	default:
	    result = NULL;
	    break;
    }
    return result;
}


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
    // TODO: Woa! What's this crap!?
    AABox bbox = AABox(Vector(-733,-733,-733),Vector(733,733,733));
    (*irradiancecache) = new IrradianceCache(bbox,renderersettings->cache_tolerance);
}

void* renderThreadDo(void* obj) {
    Renderer* tracer = (Renderer*) obj;
    tracer->run();
    return NULL;
}

void do_filtering(Image* image, FilterStack* filterstack) {
    cout << "Applying filters..." << endl;
    filterstack->apply(image);
    
    Environment* env = Environment::getUniqueInstance();
    if (env->hasPreviewWindow()) {
	PreviewWindow* win = env->getPreviewWindow();
	win->drawBlock(0,0,image->getWidth(),image->getHeight());
    }

    cout << "Done." << endl;
}

void render_frame(int frame, int frames, string outputfile, int jobs) {

    Stats::getUniqueInstance()->clear();
    Statistics::put("Renderer","Threads",jobs);

    srand(1); // Make sure rand is seeded consistently.

    RendererSettings* renderersettings = getRendererSettings();

    Scene* scene = new Scene();

    Environment::getUniqueInstance()->setScene(scene);

    parser_assign_var("frame",double(frame));
    parser_assign_var("clock",double(frame)/double(frames));

    parser->run();
    parser->populate(scene,renderersettings);

    if (renderersettings->renderertype == RendererSettings::NONE) {
	return;
    }

    if (scene->getObjects().size() == 0) {
	throw_exception("No objects in scene.");
    }

    cout << "Preparing space..." << endl;
    KdTree* space = new KdTree();
    scene->initSpace(space);
    cout << "Done." << endl;


    int img_w = renderersettings->image_width;
    int img_h = renderersettings->image_height;

    scene->getCamera()->setImageSize(img_w,img_h);
    Image* img = new Image(img_w, img_h);

    Environment* env = Environment::getUniqueInstance();

    // Open preview window if enabled and not yet open
    if (env->hasPreviewWindow() && preview_window == NULL) {
	preview_window = windowFactory(img_w, img_h);
	env->setPreviewWindow(preview_window);
	preview_window->run();
	preview_window->setImage(img);
    }

    renderersettings->threads_num = jobs;

    // Prepare photon maps if necessary
    GlobalPhotonMap* globalphotonmap;
    CausticsMap* causticsmap;
    IrradianceCache* irradiancecache;
    if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
	preparePhotonMaps(scene,space,renderersettings,&globalphotonmap,&causticsmap,&irradiancecache);
    }

    // Create and prepare job pool
    RenderJobPool* job_pool = new RenderJobPool(img_w,img_h,64);

    if (frames == 1) {
	cout << "Still render (" << img_w << "x" << img_h << ")" << endl;
    } else {
	cout << "Animation render (" << img_w << "x" << img_h 
	     << ", " << frames << " frames)" << endl;
    }

    TimerStats* rendering_time = new TimerStats("Renderer","Time");
    rendering_time->startTimer();

    Stats::getUniqueInstance()->beginTimer("Rendering");
    
    active_renderers.clear();

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
	active_renderers.push_back(renderer);
	renderer->run();
	delete renderer;
    } else {
	// Spawn renderer threads
	Renderer* renderers[renderersettings->threads_num];
	pthread_t threads[renderersettings->threads_num];
	for(int i = 0; i < renderersettings->threads_num; i++) {
	    switch (renderersettings->renderertype) {
		case RendererSettings::PHOTON_RENDERER:
		    renderers[i] = new PhotonRenderer(renderersettings,img,scene,space,job_pool,i,globalphotonmap,causticsmap,irradiancecache);
		    break;
		case RendererSettings::RAYTRACER:
		    renderers[i] = new Raytracer(renderersettings,img,scene,space,job_pool,i);
		    break;
		case RendererSettings::PATHTRACER:
		    renderers[i] = new Pathtracer(renderersettings,img,scene,space,job_pool,i);
		    break;
		default:
		    throw_exception("Unknown renderer");
	    }
	    active_renderers.push_back(renderers[i]);
	    pthread_create(&threads[i], NULL, renderThreadDo, renderers[i]);
	}

	// Wait for threads to finish
	for(int i = 0; i < renderersettings->threads_num; i++) {
	    pthread_join(threads[i], NULL);
	    delete renderers[i];
	}
    }
    Stats::getUniqueInstance()->endTimer("Rendering");
    rendering_time->stopTimer();


    // Apply filters if any
    FilterStack* filterstack = Environment::getUniqueInstance()->getFilterStack();
    if (filterstack != NULL) {
	Stats::getUniqueInstance()->beginTimer("Applying filters");
	do_filtering(img,filterstack);
	Stats::getUniqueInstance()->endTimer("Applying filters");
    }


    img->save(outputfile);
    delete img;
    delete space;
    delete scene;
    delete job_pool;
    Stats::getUniqueInstance()->dump();
    Statistics::dumpAll();
}

void work(string scenefile, string outputfile, int jobs, int frame, int frames) {

    parser = new Parser(scenefile);
    
    int frames_num = getRendererSettings()->anim_frames;
    Environment* env = Environment::getUniqueInstance();

    if (frames_num > 1) {
        cout << "Rendering frame " << (frame+1) << "/" << frames_num << endl;
    }
    render_frame(frame,frames,outputfile,jobs);

    // TODO: delete_interpreter();

    if (env->hasPreviewWindow() && preview_window != NULL) {
	preview_window->stop();
    }

}

void print_usage() {
    cout << "Usage: tracer [OPTION...] SCENEFILENAME OUTPUTFILENAME" << endl;
    cout << "       -j NUM               Number of threads to run" << endl;
    cout << "       -x                   Disable preview window" << endl;
    cout << "       -f NUM               Frame to render" << endl;
    cout << "       -F NUM               Total number of frames" << endl;
    cout << "       -d                   Print debugging information" << endl;
    cout << "       -h                   Show this help message" << endl;
    cout << "       -v                   Show current versionnumber" << endl;

}

int main(int argc, char *argv[]) {
    Environment* env = Environment::getUniqueInstance();
    if (availableWindowToolkit() != NONE) {
	env->hasPreviewWindow(true);
    } else {
	env->hasPreviewWindow(false);
    }

    // Use getopt to parse arguments.
    int c;
    opterr = 0;
    int jobs = 1;
    int frame_to_render = 0;
    int frames_total = 0;
    while ((c = getopt (argc, argv, "vdhxj:f:F:")) != -1) {
	switch(c) {
	    case 'h':
		print_usage();
		return EXIT_SUCCESS;
	    case 'v':
		cout << "Raygay 0.1" << endl;
		return EXIT_SUCCESS;
	    case 'x':
		env->hasPreviewWindow(false);
		break;
	    case 'd':
		env->isVerbose(true);
		break;
	    case 'j':
		if (sscanf(optarg,"%u",&jobs) != 1 || jobs < 1) {
		    cerr << "Illegal -j option" << endl;
		    print_usage();
		    return EXIT_FAILURE;
		};
		break;
	    case 'f':
		if (sscanf(optarg,"%u",&frame_to_render) != 1 || frame_to_render < 0) {
		    cerr << "Illegal -f option" << endl;
		    print_usage();
		    return EXIT_FAILURE;
		};
		break;
	    case 'F':
		if (sscanf(optarg,"%u",&frames_total) != 1 || frames_total < 0) {
		    cerr << "Illegal -F option" << endl;
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
	work(scenefile, outfile, jobs, frame_to_render, frames_total); 
    } catch (Exception e) {
	cout << "Exception: " << e.getMessage() 
	    << " at " << e.getSourceFile() << ":" << e.getSourceLine() << endl;
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}


