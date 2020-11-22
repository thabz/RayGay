/**
 * \mainpage
 *
 * Documentation for the RayGay renderer.
 *
 * This project was begun Thu Apr 17 2003.
 *
 * Copyright 2003-2004 by Jesper Christensen <jesper@kalliope.org>
 *
 * \ref page_gay_talk
 *
 * \ref page_raytracing
 *
 * \ref page_photonmapping
 *
 * \ref page_researchers
 *
 * \ref page_papers
 */

/**
 * \page page_researchers Researchers
 *
 * Andrew Glassner
 * http://www.glassner.com/andrew/
 *
 * Jim Arvo
 * http://www.ics.uci.edu/~arvo/
 *
 * Henrik Wann Jensen
 * http://graphics.ucsd.edu/~henrik/
 */

/**
 * \page page_raytracing Raytracing
 *
 * This is a page about raytracing.
 *
 * @see Raytracer
 */

/**
 * \page page_photonmapping Photon mapping
 *
 * This is a page about photon mapping
 *
 * @see PhotonRenderer
 */

/**
 * \page page_papers Research papers
 *
 * Denis Zorin, Peter Schröder and Wim Sweldens, ``Interpolating subdivision for
 * meshes with arbitrary topology,'' in <i>Proceedings of SIGGRAPH 1996</i>, ACM
 * SIGGRAPH, 1996, pp. 189-192.
 * http://graphics.stanford.edu/~dzorin/multires/butterfly/ This paper
 * introduces the modified butterfly subdivision scheme.
 *
 * Per H. Christensen,  "Faster Photon Map Global Illumination".  <i>Journal of
 * Graphics Tools</i>, volume 4, number 3, pages 1-10.  ACM, April 2000.
 * http://www.seanet.com/~myandper/abstract/jgt99.htm This paper introduces
 * precomputing irradiance estimates at photon positions.
 */

/**
 * \page page_gay_talk GayTalk fileformat
 *
 * \section sec_obj Objects
 *
 * \subsection sec_obj_ellipsoid ellipsoid { MATERIAL CENTER RADII }
 * \subsection sec_obj_ellipsoid2 ellipsoid { CENTER RADII }
 *
 * Inserts an ellipsoid into the current container.
 *
 * @param MATERIAL The material
 * @param POSITION The center position of the ellipsoid
 * @param RADII The x,y,z radii of the ellipsoid expressed as a vector
 *
 * \subsection sec_obj_sphere sphere { MATERIAL RADIUS CENTER } TRANSFORMATIONS
 * \subsection sec_obj_sphere2 sphere { RADIUS CENTER } TRANSFORMATIONS
 *
 * Inserts a sphere into the current container.
 * \attention Note that scaling transformations doesn't affect a sphere. Use
 * an \ref sec_obj_ellipsoid instead.
 *
 * @param MATERIAL The material
 * @param RADIUS The radius of the sphere
 * @param CENTER The center vector
 *
 * \subsection sec_obj_blob blob { MATERIAL ISO WEIGHT STEPS ACCURACY GROUP }
 *
 * Inserts a blob into the current container.
 *
 * @param MATERIAL The material
 * @param STEPS The isosurface steps parameter
 * @param ACCURACY The isosurface accuracy parameter
 *
 * \section sec_paths Paths
 *
 * circle { CENTER RADIUS NORMAL }
 *
 * spiral { PATH RADIUS WINDINGS OFFSET }
 */
