
#include "shaders/plastic.h"

RGB shade(const ShaderInput& si) {
    RGB result_color = RGB(0.0,0.0,0.0);

    // TODO:
    // normal = this->bump(si);
    
    double ambient_intensity = 0.2;
    RGB result_color = material->getDiffuseColor(intersection) * ambient_intensity;
    const vector<Lightsource*>& lights = scene->getLightsources();
    for (vector<Lightsource*>::const_iterator p = lights.begin(); p != lights.end(); p++) {
	double attenuation = (*p)->getAttenuation(point);

	if (attenuation > double(0)) {
	    Lightinfo info = (*p)->getLightinfo(intersection,normal,space,depth);
	    if (info.cos > 0.0) {
		RGB color = RGB(0.0,0.0,0.0);
		// Check for blocking objects
		if (info.intensity > 0.0) {
		    double intensity = info.intensity * attenuation;
		    // Diffuse color
		    color =  intensity * info.cos * material->getKd() * material->getDiffuseColor(intersection);

		    // Specular color (Phong)
		    Vector light_reflect = info.direction_to_light.reflect(normal);
		    light_reflect.normalize();
		    double rv = light_reflect * (-1 * ray.getDirection());
		    if (rv > 0.0) {
			rv = pow(rv,material->getSc());
			color = color + ( intensity * rv *  material->getKs() * material->getSpecularColor());
		    }
		}
		result_color += color;
	    } 
	}
    }

    return result;
}

