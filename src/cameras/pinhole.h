
#ifndef CAMERAS_PINHOLE_H
#define CAMERAS_PINHOLE_H

#include "cameras/camera.h"

class Pinhole : public Camera {

    public:
	Pinhole();
	Ray getRay(const double x, const double y);

};

#endif
