#include "raytracer.h"

#include <iostream>
#include <cmath>

#include "math/vector.h"
#include "math/functions.h"
#include "ray.h"
#include "intersection.h"
#include "objects/object.h"
#include "image/rgb.h"
#include "math/matrix.h"
#include "scene.h"
#include "camera.h"
#include "lights/lightsource.h"
#include "materials/material.h"
#include "space/spacesubdivider.h"

Raytracer::Raytracer(RendererSettings* settings, Scene* scene, SpaceSubdivider* spc) : Renderer(settings,scene,spc) {
}

RGBA Raytracer::getPixel(const Vector2& v) {
    Camera* camera = scene->getCamera();
    if (camera->isDoFEnabled()) {
	int samples = camera->getDoFSamples();
	RGBA result = RGBA(0.0,0.0,0.0,0.0);
	for(int i = 0; i < samples; i++) {
	    Ray ray = camera->getRay(v[0],v[1]);
	    result = result + tracePrimary(ray);
	}
        return result / double(samples) ;
    } else {
	Ray ray = camera->getRay(v[0],v[1]);
	return tracePrimary(ray);
    }
}

RGBA Raytracer::traceSub(const bool intersected, const Ray& ray, const int depth, const double weight) {
    RGBA color; 
    Intersection intersection;
    double intersect_distance;

    if (intersected) {
	intersection = *(space->getLastIntersection());
	color = shade(ray,intersection,depth,weight);
    } else {
        color = scene->getBackgroundColor(ray);
	intersect_distance = HUGE_DOUBLE;
    }

    if (scene->fogEnabled()) {
	intersect_distance = (intersection.getPoint() - ray.getOrigin()).length();
	double D = scene->getFogDistance();
	double v = expf(-intersect_distance/D);
	color =  (color * v) + (scene->getFogColor() * (1-v));
    }
    return color;
}

RGB Raytracer::shade(const Ray& ray, const Intersection& intersection, const int depth, const double weight) {
    Object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = object->normal(intersection);
    const Material* material = object->getMaterial();

    /*
    if (material == NULL) {
	return RGB(0.0,0.0,0.0);
    }
    */

    normal = material->bump(intersection,normal);

    Vector2 fre = fresnel(normal,ray.getDirection(),material);
    double reflection = fre[0];
    double transmission = fre[1];
    
    double ambient_intensity = 0.2;
    RGB result_color = ambient_intensity * material->getDiffuseColor(intersection);
    vector<Lightsource*> lights = scene->getLightsources();
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
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
    if (depth < 7) {
	/* Bounce a reflection off the intersected object */
	if (material->getKs() > 0 && reflection > 0) {
	    Vector refl_vector = -1 * ray.getDirection();
	    refl_vector = refl_vector.reflect(normal);
	    refl_vector.normalize();
	    RGB refl_col = RGB(0.0,0.0,0.0);
	    if (material->glossEnabled()) {
		/* Distributed reflection */
		double max_angle = material->glossMaxAngle();
		int gloss_rays = material->glossRaysNum();
		for(int i = 0; i < gloss_rays; i++) {
		    Ray refl_ray = Ray(point,Math::perturbVector(refl_vector,max_angle),ray.getIndiceOfRefraction());
		    refl_col += trace(refl_ray, depth + 1, weight);
		}
		refl_col *= 1.0/double(gloss_rays);
	    } else {
		/* Single reflected ray */
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		refl_col = trace(refl_ray, depth + 1, reflection * weight);
	    }
	    result_color += reflection * refl_col;
	}

	/* Should we send a ray through the intersected object? */
	if (material->getKt() > 0.0 && transmission > 0) {
	    // TODO: Use the ior the rays holds to allow eg. glass in water.
	    double ior = material->getEta();
	    Vector T = ray.getDirection().refract(normal,ior);
	    if (!(T == Vector(0,0,0))) {
		Ray trans_ray = Ray(point+0.1*T,T,ior);
		RGB trans_col = trace(trans_ray, depth + 1, transmission * weight);
		result_color += transmission * trans_col;
	    } else {
		// Internal reflection, see page 757.
		Vector refl_vector = -1 * ray.getDirection();
		refl_vector = refl_vector.reflect(normal);
		refl_vector.normalize();
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		RGB refl_col = trace(refl_ray, depth + 1, transmission*weight);
		result_color += transmission * refl_col;
	    }
	}
    }

    result_color.clip();
    return result_color;
}


