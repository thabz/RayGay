cmake_minimum_required(VERSION 3.20)

if(NOT DEFINED CMAKE_OSX_DEPLOYMENT_TARGET OR CMAKE_OSX_DEPLOYMENT_TARGET STREQUAL "")
  set(CMAKE_OSX_DEPLOYMENT_TARGET "11.0" CACHE STRING
    "Minimum macOS deployment version" FORCE)
endif()

project(raygay VERSION 0.7.0 LANGUAGES C CXX)

include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CTest)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Build type" FORCE)
endif()

option(RAYGAY_ENABLE_GTK "Build the GTK preview window when GTK+-2.0 is found" ON)
option(RAYGAY_ENABLE_MPI "Build the MPI renderer when MPI is found" OFF)
option(RAYGAY_ENABLE_READLINE "Use readline in raygay-repl when found" ON)

check_include_file("ApplicationServices/ApplicationServices.h" HAVE_APPLICATIONSERVICES_APPLICATIONSERVICES_H)
check_include_file("dirent.h" HAVE_DIRENT_H)
check_include_file("dlfcn.h" HAVE_DLFCN_H)
check_include_file("inttypes.h" HAVE_INTTYPES_H)
check_include_file("ndir.h" HAVE_NDIR_H)
check_include_file("pthread.h" HAVE_PTHREAD_H)
check_include_file("stdint.h" HAVE_STDINT_H)
check_include_file("stdio.h" HAVE_STDIO_H)
check_include_file("stdlib.h" HAVE_STDLIB_H)
check_include_file("strings.h" HAVE_STRINGS_H)
check_include_file("string.h" HAVE_STRING_H)
check_include_file("sys/dir.h" HAVE_SYS_DIR_H)
check_include_file("sys/ndir.h" HAVE_SYS_NDIR_H)
check_include_file("sys/stat.h" HAVE_SYS_STAT_H)
check_include_file("sys/time.h" HAVE_SYS_TIME_H)
check_include_file("sys/types.h" HAVE_SYS_TYPES_H)
check_include_file("unistd.h" HAVE_UNISTD_H)

set(STDC_HEADERS 1)
set(TIME_WITH_SYS_TIME 1)
set(HAVE_LONG_DOUBLE 1)
set(HAVE_LONG_DOUBLE_WIDER 1)

if(APPLE)
  set(OS_DARWIN 1)
  if(CMAKE_OSX_DEPLOYMENT_TARGET VERSION_LESS 11.0)
    message(FATAL_ERROR "RayGay requires macOS 11.0 or newer")
  endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  set(OS_LINUX 1)
endif()

find_package(Threads REQUIRED)
find_package(OpenEXR CONFIG QUIET)
if(APPLE)
  find_package(PNG QUIET)
  find_package(JPEG QUIET)
else()
  find_package(PNG REQUIRED)
  find_package(JPEG REQUIRED)
endif()

set(HAVE_PNG_H ${PNG_FOUND})
set(HAVE_JPEGLIB_H ${JPEG_FOUND})
set(HAVE_PTHREAD_H ${CMAKE_USE_PTHREADS_INIT})
set(HAVE_OPENEXR ${OpenEXR_FOUND})

if(UNIX AND NOT APPLE)
  find_library(MATH_LIBRARY m)
  if(MATH_LIBRARY)
    set(HAVE_LIBM 1)
    list(APPEND RAYGAY_PLATFORM_LIBS ${MATH_LIBRARY})
    set(CMAKE_REQUIRED_LIBRARIES ${MATH_LIBRARY})
  endif()
endif()

check_function_exists(fmax HAVE_FMAX)
check_function_exists(fmin HAVE_FMIN)
check_function_exists(trunc HAVE_TRUNC)

if(APPLE)
  find_library(APPLICATIONSERVICES_FRAMEWORK ApplicationServices REQUIRED)
  find_library(CARBON_FRAMEWORK Carbon REQUIRED)
  list(APPEND RAYGAY_PLATFORM_LIBS
    ${APPLICATIONSERVICES_FRAMEWORK}
    ${CARBON_FRAMEWORK}
  )
endif()

find_package(PkgConfig QUIET)
if(RAYGAY_ENABLE_GTK AND PkgConfig_FOUND)
  pkg_check_modules(GTK QUIET gtk+-2.0)
endif()
if(GTK_FOUND)
  set(HAVE_GTK 1)
endif()

if(RAYGAY_ENABLE_READLINE)
  find_library(READLINE_LIBRARY readline)
  find_library(CURSES_LIBRARY NAMES ncurses curses)
  check_include_file_cxx("readline/readline.h" HAVE_READLINE_READLINE_H)
  check_include_file_cxx("readline/history.h" HAVE_READLINE_HISTORY_H)
  if(READLINE_LIBRARY AND HAVE_READLINE_READLINE_H AND HAVE_READLINE_HISTORY_H)
    set(HAVE_LIBREADLINE 1)
    list(APPEND READLINE_LIBRARIES ${READLINE_LIBRARY})
    if(CURSES_LIBRARY)
      list(APPEND READLINE_LIBRARIES ${CURSES_LIBRARY})
    endif()
  endif()
endif()

if(RAYGAY_ENABLE_MPI)
  find_package(MPI COMPONENTS CXX)
  if(MPI_CXX_FOUND)
    set(HAVE_MPI 1)
  endif()
endif()

configure_file(config.h.cmake config.h)

add_library(raygay_options INTERFACE)
target_compile_features(raygay_options INTERFACE cxx_std_11)
target_compile_definitions(raygay_options INTERFACE HAVE_CONFIG_H=1)
target_include_directories(raygay_options INTERFACE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(raygay_options INTERFACE -Wall -Winline)
endif()

set(RAYGAY_SCHEME_SOURCES
  src/scheme/lexer.cpp
  src/scheme/interpreter.cpp
  src/scheme/objects.cpp
  src/scheme/parser.cpp
  src/scheme/heap.cpp
  src/scheme/bigint.cpp
  src/scheme/optimizer.cpp
  src/scheme/scheme.cpp
  src/scheme/filenames.cpp
  src/scheme/numbers.cpp
  src/scheme/compiler.cpp
  src/scheme/r6rs-lib-lists.cpp
  src/scheme/r6rs-lib-bytevectors.cpp
  src/scheme/r6rs-lib-hashtables.cpp
  src/scheme/r6rs-lib-sorting.cpp
  src/scheme/r6rs-lib-arithmetic.cpp
  src/scheme/r6rs-lib-io-common.cpp
  src/scheme/r6rs-lib-io-simple.cpp
  src/scheme/r6rs-lib-io-ports.cpp
  src/scheme/r6rs-lib-unicode.cpp
)

add_library(raygay_scheme STATIC ${RAYGAY_SCHEME_SOURCES})
target_link_libraries(raygay_scheme PUBLIC raygay_options)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang|GNU")
  target_compile_options(raygay_scheme PRIVATE -fno-fast-math)
endif()

set(RAYGAY_TRACER_SOURCES
  src/exception.cpp
  src/intersection.cpp
  src/ray.cpp
  src/scene.cpp
  src/renderer.cpp
  src/raytracer.cpp
  src/aabox.cpp
  src/stats.cpp
  src/photonrenderer.cpp
  src/photonmapdump.cpp
  src/renderjobs.cpp
  src/pathtracer.cpp
  src/transformer.cpp
  src/environment.cpp
  src/profiler.cpp
  src/allocator.cpp
  src/renderersettings.cpp
  src/ttf.cpp
  src/gzip.cpp
  src/materials/material.cpp
  src/materials/perlin.cpp
  src/materials/plastic.cpp
  src/materials/wood.cpp
  src/materials/checker.cpp
  src/materials/marble.cpp
  src/materials/normalperturbers/normalperturber.cpp
  src/paths/path.cpp
  src/paths/circle.cpp
  src/paths/spiral.cpp
  src/paths/linesegment.cpp
  src/paths/line.cpp
  src/paths/bezierspline.cpp
  src/paths/arc.cpp
  src/paths/catmullromspline.cpp
  src/paths/ellipse.cpp
  src/lights/arealight.cpp
  src/lights/pointlight.cpp
  src/lights/spotlight.cpp
  src/lights/skylight.cpp
  src/lights/lightsource.cpp
  src/lights/shadowcache.cpp
  src/math/vector.cpp
  src/math/matrix.cpp
  src/math/vector2.cpp
  src/math/functions.cpp
  src/math/halton.cpp
  src/math/rootfinder.cpp
  src/math/polynomial.cpp
  src/math/sturmsequence.cpp
  src/math/matrix3.cpp
  src/math/mersennetwister.cpp
  src/math/poisson_disc.cpp
  src/math/interval.cpp
  src/math/quaternion.cpp
  src/math/physics.cpp
  src/math/contour.cpp
  src/image/image.cpp
  src/image/rgb.cpp
  src/image/rgba.cpp
  src/image/texture.cpp
  src/image/simpletexture.cpp
  src/image/multitexture.cpp
  src/image/imageio.cpp
  src/image/imageio_tga.cpp
  src/image/imageio_hdri.cpp
  src/image/imageio_openexr.cpp
  src/image/imageio_jpeg.cpp
  src/image/imageio_png.cpp
  src/image/imageio_darwin.cpp
  src/image/imagedrawing.cpp
  src/photon/photon.cpp
  src/photon/globalphotonmap.cpp
  src/photon/causticsmap.cpp
  src/photon/photontracer.cpp
  src/photon/irradiancecache.cpp
  src/imagefilters/depthoffield.cpp
  src/imagefilters/grayscale.cpp
  src/imagefilters/filterstack.cpp
  src/imagefilters/specularbloom.cpp
  src/imagefilters/gaussianblur.cpp
  src/imagefilters/imagefilter.cpp
  src/imagefilters/sepia.cpp
  src/imagefilters/colormatrix.cpp
  src/imagefilters/matrixfilter.cpp
  src/objects/bezierpatch.cpp
  src/objects/blob.cpp
  src/objects/bound.cpp
  src/objects/box.cpp
  src/objects/cone.cpp
  src/objects/csg.cpp
  src/objects/csg_union.cpp
  src/objects/cylinder.cpp
  src/objects/ellipsoid.cpp
  src/objects/extrusion.cpp
  src/objects/halfspace.cpp
  src/objects/heightfield.cpp
  src/objects/isosurface.cpp
  src/objects/julia.cpp
  src/objects/marchingcubes.cpp
  src/objects/mesh.cpp
  src/objects/object.cpp
  src/objects/objectgroup.cpp
  src/objects/obj.cpp
  src/objects/parametrizedsurface.cpp
  src/objects/ply.cpp
  src/objects/sdf_object.cpp
  src/objects/solid.cpp
  src/objects/solidbox.cpp
  src/objects/sor.cpp
  src/objects/sphere.cpp
  src/objects/superellipsoid.cpp
  src/objects/tessalation.cpp
  src/objects/tetrahedron.cpp
  src/objects/text.cpp
  src/objects/torus.cpp
  src/objects/transformedinstance.cpp
  src/objects/triangle.cpp
  src/objects/wireframe.cpp
  src/space/kdtree.cpp
  src/space/hitcache.cpp
  src/shaders/blinn.cpp
  src/parser/pathfactory.cpp
  src/parser/camerafactory.cpp
  src/parser/sceneparser.cpp
  src/parser/sceneobjectfactory.cpp
  src/parser/schemeisosurface.cpp
  src/parser/schemenormalperturber.cpp
  src/parser/schemeparametrizedsurface.cpp
  src/parser/lightsourcefactory.cpp
  src/parser/transformationfactory.cpp
  src/parser/materialfactory.cpp
  src/parser/texturefactory.cpp
  src/parser/imagefactory.cpp
  src/parser/mathfactory.cpp
  src/parser/schemefunctions.cpp
  src/parser/converters.cpp
  src/parser/wrapper.cpp
  src/cameras/camera.cpp
  src/cameras/pinhole.cpp
  src/cameras/latlong.cpp
  src/cameras/fisheye.cpp
  src/collections/vectorlist.cpp
  src/samplers/sampler.cpp
  src/samplers/non_aa_sampler.cpp
  src/samplers/whitted_adaptive.cpp
  src/samplers/halton_sampler.cpp
  src/samplers/uniform_jitter.cpp
  src/filters/filter.cpp
)

add_library(raygay_tracer STATIC ${RAYGAY_TRACER_SOURCES})
target_link_libraries(raygay_tracer PUBLIC
  raygay_options
  raygay_scheme
  Threads::Threads
  ${RAYGAY_PLATFORM_LIBS}
)
if(PNG_FOUND)
  target_link_libraries(raygay_tracer PUBLIC PNG::PNG)
endif()
if(JPEG_FOUND)
  target_link_libraries(raygay_tracer PUBLIC JPEG::JPEG)
endif()
if(OpenEXR_FOUND)
  target_link_libraries(raygay_tracer PUBLIC OpenEXR::OpenEXR)
endif()

set(RAYGAY_HTTP_SOURCES
  src/http/httpserver.cpp
  src/http/httpcommon.cpp
  src/http/httpclient.cpp
  src/http/imageresponse.cpp
  src/http/md5.c
)

add_library(raygay_http STATIC ${RAYGAY_HTTP_SOURCES})
target_link_libraries(raygay_http PUBLIC raygay_tracer)

add_executable(raygay src/main.cpp src/window-gtk.cpp)
target_link_libraries(raygay PRIVATE raygay_tracer)
if(GTK_FOUND)
  target_include_directories(raygay PRIVATE ${GTK_INCLUDE_DIRS})
  target_compile_options(raygay PRIVATE ${GTK_CFLAGS_OTHER})
  target_link_directories(raygay PRIVATE ${GTK_LIBRARY_DIRS})
  target_link_libraries(raygay PRIVATE ${GTK_LIBRARIES})
endif()

add_executable(raygay-server src/server-main.cpp)
target_link_libraries(raygay-server PRIVATE raygay_http)

add_executable(raygay-queuemanager src/queuemanager-main.cpp)
target_link_libraries(raygay-queuemanager PRIVATE raygay_http)

add_executable(raygay-repl src/repl-main.cpp)
target_link_libraries(raygay-repl PRIVATE raygay_tracer ${READLINE_LIBRARIES})

add_executable(bench src/bench-main.cpp)
target_link_libraries(bench PRIVATE raygay_tracer)

if(APPLE)
  add_executable(win src/win-main.cpp)
  target_link_libraries(win PRIVATE raygay_tracer)
endif()

if(HAVE_MPI)
  add_executable(raygay-mpi src/mpi-main.cpp)
  target_link_libraries(raygay-mpi PRIVATE raygay_tracer MPI::MPI_CXX)
endif()

if(BUILD_TESTING)
  add_executable(testmath src/math/testmath.cpp)
  target_link_libraries(testmath PRIVATE raygay_tracer)
  add_test(NAME testmath COMMAND testmath)

  add_executable(testscheme src/scheme/testscheme.cpp)
  target_link_libraries(testscheme PRIVATE raygay_scheme)
  add_test(NAME testscheme COMMAND testscheme)

  add_executable(testrational src/scheme/testrational.cpp)
  target_link_libraries(testrational PRIVATE raygay_scheme)
  add_test(NAME testrational COMMAND testrational)

  add_executable(testbigint src/scheme/testbigint.cpp)
  target_link_libraries(testbigint PRIVATE raygay_scheme)
  add_test(NAME testbigint COMMAND testbigint)

  foreach(test_name IN ITEMS
    testobjects
    testimage
    teststats
    testlanguage
    testpaths
    testspace
    testthreads
    testparser
  )
    add_executable(${test_name} test/${test_name}.cpp)
    target_include_directories(${test_name} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/test)
    target_link_libraries(${test_name} PRIVATE raygay_tracer)
    add_test(NAME ${test_name} COMMAND ${test_name})
    set_tests_properties(${test_name} PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/test)
  endforeach()

  add_executable(testhttp test/testhttp.cpp)
  target_include_directories(testhttp PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/test)
  target_link_libraries(testhttp PRIVATE raygay_http)
  add_test(NAME testhttp COMMAND testhttp)

  add_executable(testspeed test/testspeed.cpp)
  target_include_directories(testspeed PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/test)
  target_link_libraries(testspeed PRIVATE raygay_tracer)
endif()
