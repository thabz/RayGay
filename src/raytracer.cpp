#include "raytracer.h"

#include <iostream>
#include <math.h>

#include "math/vector.h"
#include "ray.h"
#include "intersection.h"
#include "object.h"
#include "image/rgb.h"
#include "math/matrix.h"
#include "scene.h"
#include "image/image.h"
#include "lights/lightsource.h"

Raytracer::Raytracer() : Renderer() {
}

RGB Raytracer::getPixel(const Vector2& v) {
    Stats::getUniqueInstance()->inc("Primary camera rays cast");
    Vector position = scene->getCamera()->getPosition();
    Vector scr = Vector(v[0],v[1],0);
    Vector raydir = scr - position;
    raydir.normalize();
    Ray ray = Ray(position,raydir,1.0);
    return trace(ray,1);
}


RGB Raytracer::trace(const Ray& ray, int depth) {
    Stats::getUniqueInstance()->inc("Total camera rays cast");
    RGB color; 
    Intersection intersection;
    bool intersected = space->intersect(ray);
    double intersect_distance;

    if (intersected) {
	intersection = *(space->getLastIntersection());
	intersect_distance = (intersection.getPoint() - ray.getOrigin()).length();
	color = shade(ray,intersection,depth);
    } else {
        color = scene->getBackgroundColor(ray);
	intersect_distance = HUGE_DOUBLE;
    }

    if (scene->fogEnabled()) {
	double D = scene->getFogDistance();
	double v = expf(-intersect_distance/D);
	color = v * color + (1-v) * scene->getFogColor();
    }
    return color;
}

RGB Raytracer::shade(const Ray& ray, const Intersection& intersection, int depth) {
    object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = object->normal(intersection);
    const Material& material = object->getMaterial();
    normal = material.bump(intersection,normal);
    double ambient_intensity = 0.2;
    RGB result_color = ambient_intensity * material.getDiffuseColor(intersection);
    vector<Lightsource*> lights = scene->getLightsources();
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
	double attenuation = (*p)->getAttenuation(point);

	if (attenuation > double(0)) {
	    Lightinfo info = (*p)->getLightinfo(intersection,normal,space);
	    if (info.cos > 0.0) {
		RGB color = RGB(0.0,0.0,0.0);
		// Check for blocking objects
		if (info.intensity > 0.0) {
		    double intensity = info.intensity * attenuation;
		    // Diffuse color
		    color =  intensity * info.cos * material.getKd() * material.getDiffuseColor(intersection);

		    // Specular color (Phong)
		    Vector light_reflect = info.direction_to_light.reflect(normal);
		    light_reflect.normalize();
		    double rv = light_reflect * (-1 * ray.getDirection());
		    if (rv > 0.0) {
			rv = pow(rv,material.getSc());
			color = color + ( intensity * rv *  material.getKs() * material.getSpecularColor());
		    }
		}
		result_color = result_color + color;
	    } 
	}
    }
    if (depth < 4) {
	/* Bounce a reflection off the intersected object */
	if (material.getKs() > 0) {
	    Vector refl_vector = -1 * ray.getDirection();
	    refl_vector = refl_vector.reflect(normal);
	    refl_vector.normalize();
	    Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
	    RGB refl_col = trace(refl_ray, depth + 1);
	    result_color = result_color + material.getKs() * refl_col;
	}

	/* Should we send a ray through the intersected object? */
	if (material.transmission_coefficient > 0.0) {
	    // Calculate refraction vector (page 757)
	    double my = ray.getIndiceOfRefraction() / material.indice_of_refraction;
	    Vector I = -1 * ray.getDirection();
	    double n = normal * I;
	    double p = my*my*(1 - n*n);
	    if (p < 1) {
		// No internal reflection (page 758)
		Vector T = (my*n - sqrt(1 - p))*normal - my*I;
		Ray trans_ray = Ray(point,T,material.indice_of_refraction);
		RGB trans_col = trace(trans_ray, depth + 1);
		result_color = result_color + material.transmission_coefficient * trans_col;
	    }
	}
    }

    result_color.clip();
    return result_color;
}

