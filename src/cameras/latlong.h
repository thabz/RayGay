
#ifndef CAMERAS_LATLONG_H
#define CAMERAS_LATLONG_H

#include "cameras/camera.h"

/**
 * A latitude/longitude camera. This basically renderers a 360 degree panoramic
 * view of the scene.
 * 
 */
class LatLong : public Camera {

    public:
	LatLong();

    protected:
	Ray _getRay(const double x, const double y);

};

#endif
