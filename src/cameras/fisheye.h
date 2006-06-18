
#ifndef CAMERAS_FISHEYE_H
#define CAMERAS_FISHEYE_H

#include "cameras/camera.h"

/**
 * A Fisheye camera
 * 
 */
class Fisheye : public Camera {

    public:
	Fisheye();

    protected:
	Ray _getRay(const double x, const double y);

};

#endif
