
/*
    Copyright (C) 2004-2006 Jesper Christensen <jesper@kalliope.org>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <string>
#include <vector>

extern "C" {
#include <unistd.h>
}

#ifdef OS_DARWIN
#include <Carbon/Carbon.h>
#endif

#include "environment.h"
#include "exception.h"
#include "stats.h"

#include "math/matrix.h"
#include "math/vector.h"

#include "cameras/camera.h"
#include "image/imageimpl.h"
#include "image/texture.h"
#include "scene.h"
#include "space/kdtree.h"

#include "pathtracer.h"
#include "photonrenderer.h"
#include "raytracer.h"

#include "photon/causticsmap.h"
#include "photon/globalphotonmap.h"
#include "photon/irradiancecache.h"
#include "photon/photontracer.h"

#include "parser/sceneparser.h"

#include "profiler.h"
#include "renderersettings.h"
#include "renderjobs.h"

#include "imagefilters/filterstack.h"

#include "window-gtk.h"
#include "window.h"

using namespace std;

RendererSettings *renderer_settings = RendererSettings::uniqueInstance();
Scene *scene = new Scene();
SceneParser *parser = new SceneParser(scene);
PreviewWindow *preview_window = NULL;
std::string scenefile;

RendererSettings *getRendererSettings() { return renderer_settings; }

vector<Renderer *> active_renderers;

void abortRenderingCB() {
  cout << "Aborting render..." << endl;
  for (uint32_t i = 0; i < active_renderers.size(); i++) {
    if (active_renderers[i] != NULL) {
      active_renderers[i]->abort();
    }
  }
}

enum windowToolkitId { NONE, GTK, COCOA };

windowToolkitId availableWindowToolkit() {
#ifdef HAVE_GTK
  return GTK;
#endif
#ifdef HAVE_COCOA
  return COCOA;
#endif
  return NONE;
}

PreviewWindow *windowFactory(int w, int h) {
  PreviewWindow *result = NULL;
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

/**
 * Returns number of CPUs runtime
 * See http://www.ks.uiuc.edu/Research/vmd/doxygen/VMDThreads_8C-source.html
 * for how to do this on other architectures.
 */
uint32_t getNumberOfCPUs() {
  uint32_t a;
#ifdef OS_DARWIN
  a = MPProcessorsScheduled();
#elif OS_LINUX
  a = sysconf(_SC_NPROCESSORS_ONLN);
#elif OS_SOLARIS
  a = sysconf(_SC_NPROCESSORS_ONLN);
#elif OS_IRIX
  a = sysconf(_SC_NPROC_ONLN);
#else
  a = 1;
#endif
  return a;
}

// Assignments* global_assigments = new Assignments();

void preparePhotonMaps(Scene *scene, KdTree *space,
                       RendererSettings *renderersettings,
                       GlobalPhotonMap **globalphotonmap,
                       CausticsMap **causticsphotonmap,
                       IrradianceCache **irradiancecache) {
  Profiler *profiler;

  (*globalphotonmap) = new GlobalPhotonMap(renderersettings->global_photons_num,
                                           renderersettings->estimate_radius,
                                           renderersettings->estimate_samples);
  (*causticsphotonmap) = new CausticsMap(renderersettings->caustic_photons_num,
                                         renderersettings->estimate_radius,
                                         renderersettings->estimate_samples);

  PhotonTracer *photontracer =
      new PhotonTracer(scene, space, (*globalphotonmap), (*causticsphotonmap));

  profiler = Profiler::create("Tracing photons", "Prepare scene");
  profiler->start();
  cout << "Tracing photons..." << flush;
  photontracer->trace(renderersettings->threads_num);
  cout << "Done." << endl;
  profiler->stop();

  profiler = Profiler::create("Balance photonmaps", "Prepare scene");
  profiler->start();
  cout << "Balancing photonmaps..." << flush;
  int total_photons_num = renderersettings->global_photons_num +
                          renderersettings->caustic_photons_num;
  (*globalphotonmap)->scale_photon_power(1.0 / double(total_photons_num));
  (*globalphotonmap)->balance();
  (*causticsphotonmap)->scale_photon_power(1.0 / double(total_photons_num));
  (*causticsphotonmap)->balance();
  cout << "Done." << endl;
  profiler->stop();

  profiler = Profiler::create("Precompute irradiances", "Prepare scene");
  profiler->start();
  cout << "Precomputing irradiances..." << flush;
  (*globalphotonmap)->preComputeIrradiances(4, renderersettings->threads_num);
  cout << "Done." << endl;
  profiler->stop();

  delete photontracer;

  AABox bbox = space->boundingBox();
  bbox.growPercentage(10);
  (*irradiancecache) =
      new IrradianceCache(bbox, renderersettings->cache_tolerance);
}

void *renderThreadDo(void *obj) {
  Renderer *tracer = (Renderer *)obj;
  try {
    tracer->run();
  } catch (Exception e) {
    cout << endl
         << "Exception: " << e.getMessage() << " at " << e.getSourceFile()
         << ":" << e.getSourceLine() << endl;
    exit(EXIT_FAILURE);
  } catch (scheme_exception e) {
    wcerr << L"FAIL: " << e.toString() << endl;
    exit(EXIT_FAILURE);
  }
  return NULL;
}

void do_filtering(Image *image, FilterStack *filterstack) {
  cout << "Applying filters... " << flush;
  filterstack->apply(image);

  Environment *env = Environment::getUniqueInstance();
  if (env->hasPreviewWindow()) {
    PreviewWindow *win = env->getPreviewWindow();
    win->drawBlock(0, 0, image->getWidth(), image->getHeight());
  }

  cout << "Done." << endl;
}

void render_frame(string outputfile, int jobs) {
  Profiler *profiler = Profiler::create("Prepare scene", "RayGay");
  profiler->start();
  Stats::getUniqueInstance()->clear();
  Statistics::put("Renderer", "Threads", jobs);

  srand(1); // Make sure rand is seeded consistently.

  RendererSettings *renderersettings = getRendererSettings();

  Environment::getUniqueInstance()->setScene(scene);

  Profiler *parser_profiler = Profiler::create("Parsing", "Prepare scene");
  parser_profiler->start();

  char original_working_dir[2048];
  getcwd(original_working_dir, 2048);
  wstring original_cwds =
      SchemeFilenames::toString(string(original_working_dir));
  wstring cwd = original_cwds + L"/" + SchemeFilenames::toString(scenefile);
  wstring filename_clean = wstring(cwd);
  int idx = cwd.find_last_of(L'/');
  cwd.resize(idx);
  filename_clean = filename_clean.substr(idx + 1, filename_clean.length());
  chdir(SchemeFilenames::toFilename(cwd).c_str());

  parser->parse_file(filename_clean);
  parser->populate(scene, renderersettings);

  parser_profiler->stop();

  if (renderersettings->renderertype == RendererSettings::NONE) {
    return;
  }

  if (scene->getObjects().size() == 0) {
    throw_exception("No objects in scene.");
  }

  cout << "Preparing object and space... " << flush;
  KdTree *space = new KdTree();
  scene->initSpace(space);
  cout << "Done." << endl;
  profiler->stop();

  int img_w = renderersettings->image_width;
  int img_h = renderersettings->image_height;

  scene->getCamera()->setAspectRatio(double(img_h) / double(img_w));
  Image *img = new ImageImpl<float, 4>(img_w, img_h,
                                       renderersettings->image_alloc_model);

  Environment *env = Environment::getUniqueInstance();

  // Open preview window if enabled and not yet open
  if (env->hasPreviewWindow() && preview_window == NULL) {
    preview_window = windowFactory(img_w, img_h);
    env->setPreviewWindow(preview_window);
    preview_window->run();
    preview_window->setImage(img);
  }

  renderersettings->threads_num = jobs;

  // Prepare photon maps if necessary
  GlobalPhotonMap *globalphotonmap;
  CausticsMap *causticsmap;
  IrradianceCache *irradiancecache;
  if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
    preparePhotonMaps(scene, space, renderersettings, &globalphotonmap,
                      &causticsmap, &irradiancecache);
  }

  // Create and prepare job pool
  RenderJobPool *job_pool = new RenderJobPool(img_w, img_h);

  TimerStats *rendering_time = new TimerStats("Renderer", "CPU time");
  WalltimeStats *rendering_walltime =
      new WalltimeStats("Renderer", "Wall time");
  rendering_time->startTimer();
  rendering_walltime->startTimer();

  active_renderers.clear();

  if (renderersettings->threads_num == 1) {
    // If only one thread is wanted we don't spawn a render-thread
    // but keep the rendering in-process. This allows for profiling
    // with gprof, which doesn't support profiling of multithreaded
    // programs.
    Renderer *renderer = NULL;
    if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
      renderer =
          new PhotonRenderer(renderersettings, img, scene, space, job_pool, 0,
                             globalphotonmap, causticsmap, irradiancecache);
    } else if (renderersettings->renderertype == RendererSettings::RAYTRACER) {
      renderer =
          new Raytracer(renderersettings, img, scene, space, job_pool, 0);
    } else if (renderersettings->renderertype == RendererSettings::PATHTRACER) {
      renderer =
          new Pathtracer(renderersettings, img, scene, space, job_pool, 0);
    }
    active_renderers.push_back(renderer);
    Profiler *p = Profiler::create("Rendering", "RayGay");
    p->start();
    renderer->run();
    p->stop();
    delete renderer;
  } else {
    // Spawn renderer threads
    Renderer *renderers[renderersettings->threads_num];
    pthread_t threads[renderersettings->threads_num];
    for (int i = 0; i < renderersettings->threads_num; i++) {
      switch (renderersettings->renderertype) {
      case RendererSettings::PHOTON_RENDERER:
        renderers[i] =
            new PhotonRenderer(renderersettings, img, scene, space, job_pool, i,
                               globalphotonmap, causticsmap, irradiancecache);
        break;
      case RendererSettings::RAYTRACER:
        renderers[i] =
            new Raytracer(renderersettings, img, scene, space, job_pool, i);
        break;
      case RendererSettings::PATHTRACER:
        renderers[i] =
            new Pathtracer(renderersettings, img, scene, space, job_pool, i);
        break;
      default:
        throw_exception("Unknown renderer");
      }
      active_renderers.push_back(renderers[i]);
      pthread_create(&threads[i], NULL, renderThreadDo, renderers[i]);
    }

    // Wait for threads to finish
    for (int i = 0; i < renderersettings->threads_num; i++) {
      pthread_join(threads[i], NULL);
      active_renderers[i] = NULL;
      delete renderers[i];
    }
  }
  rendering_time->stopTimer();
  rendering_walltime->stopTimer();

  if (renderersettings->renderertype == RendererSettings::PHOTON_RENDERER) {
    delete globalphotonmap;
    delete causticsmap;
    delete irradiancecache;
  }

  // Apply filters if any
  FilterStack *filterstack = Environment::getUniqueInstance()->getFilterStack();
  if (filterstack != NULL) {
    Stats::getUniqueInstance()->beginTimer("Applying filters");
    do_filtering(img, filterstack);
    Stats::getUniqueInstance()->endTimer("Applying filters");
  }

  chdir(original_working_dir);

  Profiler *save_profiler = Profiler::create("Saving image", "RayGay");
  save_profiler->start();
  img->save(outputfile);
  save_profiler->stop();
  delete img;
  delete space;
  delete scene;
  delete job_pool;
  Stats::getUniqueInstance()->dump();
  Statistics::dumpAll();

  if (env->hasPreviewWindow() && preview_window != NULL) {
    preview_window->stop();
  }
}

void print_usage() {
  uint32_t cpus = getNumberOfCPUs();
  cout << "Usage: tracer [OPTION...] SCENE-FILENAME OUTPUT-FILENAME" << endl;
  cout << "       -j NUM               Number of threads to run (default "
       << cpus << ")" << endl;
  if (availableWindowToolkit() != NONE) {
    cout << "       -b                   Run in background with no GUI" << endl;
  }
  cout << "       -e EXPR              Eval a Scheme-expr prior to" << endl;
  cout << "                            parsing the scenefile" << endl;
  cout << "       -f                   Fast preview rendering" << endl;
  cout << "       -d                   Print debugging information" << endl;
  cout << "       -p                   Dump profile after run" << endl;
  cout << "       -h                   Show this help message" << endl;
  cout << "       -v                   Show version" << endl;
}

void print_version() {
  cout << "Raygay " << VERSION << endl;
  cout << "Copyright (C) 2004-2008 Jesper Christensen" << endl;
  cout << "   Parser: " << parser->version() << endl;
  cout << "   Kernel pagesize: " << getpagesize() << " bytes" << endl;
  cout << "   CPUs: " << getNumberOfCPUs() << endl;
  cout << "   Image formats: ";
  vector<string> formats = Image::supportedFormats();
  for (uint32_t i = 0; i < formats.size(); i++) {
    cout << formats[i];
    if (i != formats.size() - 1) {
      cout << ", ";
    }
  }
  cout << endl;
}

int main(int argc, char *argv[]) {
  Environment *env = Environment::getUniqueInstance();
  if (availableWindowToolkit() != NONE) {
    env->hasPreviewWindow(true);
  } else {
    env->hasPreviewWindow(false);
  }

  // Use getopt to parse arguments.
  int c;
  opterr = 0;
  int jobs = getNumberOfCPUs();
  while ((c = getopt(argc, argv, "Vvfpdhbj:e:")) != -1) {
    switch (c) {
    case 'h':
      print_usage();
      return EXIT_SUCCESS;
    case 'v':
      print_version();
      return EXIT_SUCCESS;
    case 'f':
      RendererSettings::uniqueInstance()->fast_preview = true;
      break;
    case 'b':
      env->hasPreviewWindow(false);
      break;
    case 'd':
      env->isVerbose(true);
      break;
    case 'p':
      env->isProfilingEnabled(true);
      break;
    case 'j':
      if (sscanf(optarg, "%u", &jobs) != 1 || jobs < 1) {
        cerr << "Illegal -j option" << endl;
        print_usage();
        return EXIT_FAILURE;
      };
      break;
    case 'e':
      parser->parse_expr(SchemeFilenames::toString(optarg));
      break;
    case '?':
      cerr << "Unknown option -" << char(optopt) << endl << endl;
      print_usage();
      return EXIT_FAILURE;
    default:
      return EXIT_FAILURE;
    }
  }

  string outfile;
  if (optind != argc - 2) {
    cerr << "Not enough arguments" << endl << endl;
    print_usage();
    return EXIT_FAILURE;
  } else {
    scenefile = string(argv[optind]);
    outfile = string(argv[optind + 1]);
  }

  // TODO: Use our own RNG instead. Ex. the Mersenne Twister
  srand(1); // Make sure rand is seeded consistently.

  if (env->isProfilingEnabled() && jobs != 1) {
    cout << "Running in one thread because of profiling." << endl;
    jobs = 1;
  }
  try {
    Profiler *profiler = Profiler::create("RayGay");
    profiler->start();
    render_frame(outfile, jobs);
    profiler->stop();
    if (env->isProfilingEnabled()) {
      Profiler::dump();
    }
  } catch (Exception e) {
    cout << "Exception: " << e.getMessage() << " at " << e.getSourceFile()
         << ":" << e.getSourceLine() << endl;
    return EXIT_FAILURE;
  } catch (scheme_exception e) {
    wcerr << L"FAIL: " << e.toString() << endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
