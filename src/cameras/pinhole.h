
#ifndef CAMERAS_PINHOLE_H
#define CAMERAS_PINHOLE_H

#include "cameras/camera.h"

class Pinhole : public Camera {

    public:
	Pinhole();

    protected:
	Ray _getRay(const double x, const double y);

};

#endif
