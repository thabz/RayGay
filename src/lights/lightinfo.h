#ifndef LIGHTINFO_H
#define LIGHTINFO_H

#include "math/vector.h"

/// The datavalue class lightsources returns to the camera
class Lightinfo {

    public:

	/// The intensity at a point by one lightsource
	double intensity;
	/// The cosine of the angle betweeen the incidentpoints normal and the ray towards the light
	double cos;
	/// The direction from the incidentpoint to the light
	Vector direction_to_light;
};

#endif 
