#include "raytracer.h"

#include <iostream>
#include <cmath>
#include <cassert>

#include "math/vector.h"
#include "math/functions.h"
#include "ray.h"
#include "intersection.h"
#include "objects/object.h"
#include "image/rgb.h"
#include "math/matrix.h"
#include "scene.h"
#include "cameras/camera.h"
#include "lights/lightsource.h"
#include "materials/material.h"
#include "space/kdtree.h"

Raytracer::Raytracer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, unsigned int thread_id) : Renderer(settings,img,scene,spc,job_pool,thread_id) {
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
	ray.fromObject = camera;
	return tracePrimary(ray);
    }
}

RGBA Raytracer::traceSub(const bool intersected, Intersection& intersection, const Ray& ray, const int depth) {
    Stats::getUniqueInstance()->inc(STATS_TOTAL_CAMERA_RAYS_CAST);
    RGBA color; 
    double intersect_distance = HUGE_DOUBLE;

    if (intersected) {
	color = shade(ray,intersection,depth);
    } else {
        color = scene->getBackgroundColor(ray);
    }

    if (scene->fogEnabled()) {
	if (intersected) {
	    intersect_distance = (intersection.getPoint() - ray.getOrigin()).length();
	    double D = scene->getFogDistance();
	    double v = expf(-intersect_distance/D);
	    color =  (color * v) + (scene->getFogColor() * (1-v));
	} else {
	    color = scene->getFogColor();
	}
    }
    return color;
}

RGB Raytracer::shade(const Ray& ray, Intersection& intersection, const int depth) {
    Lightinfo info;
    Object* const object = intersection.getObject();
    const Vector point = intersection.getPoint();
    
    Vector normal = intersection.getNormal();
    const Material* material = object->getMaterial();
    normal = material->bump(intersection,normal);
    intersection.setNormal(normal);
    
    double ambient_intensity = 0.2;
    RGB result_color = material->getDiffuseColor(intersection) * ambient_intensity;
    const vector<Lightsource*>& lights = scene->getLightsources();
    for (vector<Lightsource*>::const_iterator p = lights.begin(); p != lights.end(); p++) {
	double attenuation = (*p)->getAttenuation(point);

	if (attenuation > double(0)) {
	    (*p)->getLightinfo(intersection,space,&info,depth);
	    if (info.cos > 0.0) {
		RGB color = RGB(0.0,0.0,0.0);
		// Check for blocking objects
		if (info.intensity > 0.0) {
		    double intensity = info.intensity * attenuation;
		    // Diffuse color
		    if (material->getKd() > 0) {
			color =  intensity * info.cos * material->getKd() * material->getDiffuseColor(intersection);
		    }

		    // Specular color (Phong)
		    if (material->getKs() > 0.0) {
			Vector light_reflect = info.direction_to_light.reflect(normal);
			light_reflect.normalize();
			double rv = light_reflect * (-1 * ray.getDirection());
			if (rv > 0.0) {
			    rv = pow(rv,material->getSc());
			    color = color + ( intensity * rv *  material->getKs() * material->getSpecularColor());
			}
		    }
		}
		result_color += color;
	    } 
	}
    }
    if (depth < 7) {
	Vector2 fre = fresnel(normal,ray.getDirection(),material);
	const double reflection = fre[0];
	const double transmission = fre[1];

	/* Bounce a reflection off the intersected object */
	if (material->getKs() > 0 && reflection > 0) {
	    result_color += reflection * calculate_reflection(ray,intersection,depth,material);
	}

	/* Should we send a ray through the intersected object? */
	if (material->getKt() > 0.0 && transmission > 0) {
	    // TODO: Use the ior the rays holds to allow eg. glass in water.
	    double ior = material->getEta();
	    Vector T = ray.getDirection().refract(normal,ior);
	    if (!(T == Vector(0,0,0))) {
		Ray trans_ray = Ray(point+0.1*T,T,ior);
		RGB trans_col = trace(trans_ray, depth + 1);
		result_color += transmission * trans_col;
	    } else {
		// Internal reflection, see page 757.
		/*
		   Vector refl_vector = -1 * ray.getDirection();
		   refl_vector = refl_vector.reflect(normal);
		   refl_vector.normalize();
		   Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		   RGB refl_col = trace(refl_ray, depth + 1);
		   result_color += transmission * refl_col;
		   */
	    }
	}
    }

    //   result_color.clip();
    return result_color;
}

RGB Raytracer::calculate_reflection(const Ray& ray, const Intersection& intersection, const int depth, const Material* material) {

    Vector refl_vector = -1 * ray.getDirection();
    Object* const object = intersection.getObject();
    refl_vector = refl_vector.reflect(intersection.getNormal());
    refl_vector.normalize();
    Vector point = intersection.getPoint();
    RGB refl_col = RGB(0.0,0.0,0.0);
    
    if (material->glossEnabled()) {
	/* Distributed reflection */
	double max_angle = material->glossMaxAngle();
	int gloss_rays = material->glossRaysNum();
	assert(gloss_rays > 0);
	gloss_sequence->reset();

	//gloss_rays /= depth;

#if 0
	/* Adaptive sampling. Only if the first half of the reflection rays
	 * vary too much, do we fire off the second half. */
	int num = 0;
	bool needs_more = false;
	RGB refl_col_tmp;

	for(int i = 0; i < gloss_rays/8; i++) {
	    Ray refl_ray = Ray(point,Math::perturbVector(refl_vector,max_angle,gloss_sequence),ray.getIndiceOfRefraction());
	    refl_col_tmp = trace(refl_ray, depth + 1);
	    refl_col += refl_col_tmp;
	    num++;
	    if (refl_col_tmp.sqrDistance(refl_col * 1.0/double(num)) > 0.1)
		needs_more = true;
	}

	if (needs_more) {
	    for(int i = gloss_rays/8; i < gloss_rays; i++) {
		Ray refl_ray = Ray(point,Math::perturbVector(refl_vector,max_angle,gloss_sequence),ray.getIndiceOfRefraction());
		refl_col += trace(refl_ray, depth + 1);
		num++;
	    }
	}
	refl_col *= 1.0 / double(num);
#else
    for(int i = 0; i < gloss_rays; i++) {
	Ray refl_ray = Ray(point,Math::perturbVector(refl_vector,max_angle,gloss_sequence),ray.getIndiceOfRefraction());
	refl_col += trace(refl_ray, depth + 1);
    }
    refl_col *= 1.0 / double(gloss_rays);


#endif	
    } else {
	/* Single reflected ray */
	Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
	refl_ray.fromObject = object;
	refl_col = trace(refl_ray, depth + 1);
    }
    return refl_col;
}



