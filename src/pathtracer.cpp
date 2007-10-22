#include "pathtracer.h"

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
#include "cameras/camera.h"
#include "lights/lightsource.h"
#include "materials/material.h"
#include "space/kdtree.h"
#include "math/halton.h"

Pathtracer::Pathtracer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, uint32_t thread_id) : Renderer(settings,img,scene,spc,job_pool,thread_id) {

    for(uint32_t i = 0; i < MAX_DEPTH+1; i++) {
	seqs.push_back(new Halton(2,2));
    }
}

RGBA Pathtracer::getPixel(const Vector2& v) {
    Camera* camera = scene->getCamera();
    Ray ray = camera->getRay(v[0],v[1]);
    if (ray.ignore()) {
	return RGBA(0,0,0,0);
    }
    ray.fromObject = camera;
    return tracePrimary(ray);
}

RGBA Pathtracer::tracePrimary(const Ray& ray) {
    Stats::getUniqueInstance()->inc(STATS_PRIMARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersectPrimary(ray, i);
    if (intersected) {
	// Reset the quasi monte carlo sequence for each trace depth
	//for(uint32_t j = 0; j < MAX_DEPTH; j++) {
	//    seqs[j]->reset(RANDOM(0,1));
	//}
        return traceSub(intersected, i, ray, int(RANDOM(MAX_DEPTH-5,MAX_DEPTH)));
    } else {
        return traceSub(intersected, i, ray, int(RANDOM(MAX_DEPTH-5,MAX_DEPTH)));
    }
}

RGBA Pathtracer::traceSub(const bool intersected, const Intersection& intersection, const Ray& ray, const int depth) {
    Stats::getUniqueInstance()->inc(STATS_TOTAL_CAMERA_RAYS_CAST);
    RGBA color; 
    double intersect_distance = HUGE_DOUBLE;

    if (intersected) {
	color = shade(ray,intersection,depth);
    } else {
	color = scene->getBackgroundColor(ray);
    }

    if (scene->fogEnabled()) {
	intersect_distance = (intersection.getPoint() - ray.getOrigin()).length();
	double D = scene->getFogDistance();
	double v = expf(-intersect_distance/D);
	color =  (color * v) + (scene->getFogColor() * (1-v));
    }
    return color;
}

RGB Pathtracer::shade(const Ray& ray, const Intersection& intersection, const int depth) {
    Lightinfo info;
    const Object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = intersection.getNormal();
    const Material* material = object->getMaterial();

    normal = material->bump(intersection,normal);

    RGB result_color = RGB(0.0,0.0,0.0);
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
		    // Direct diffuse color
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
    if (depth >= 0) {
        // Indirect diffuse color
	if (material->getKd() > 0) {
	    double* rnd = seqs[depth]->getNext();
	    //Vector dir = normal.randomHemisphere(rnd[0],rnd[1]);
	    Vector dir = normal.randomHemisphere(rnd[0],rnd[1],0.01);
	    //Vector dir = normal.randomHemisphere();
	    double cosa = dir * normal;
	    Ray new_ray = Ray(point + 0.1*dir,dir,-1);
	    RGB in_diff = trace(new_ray,depth-1);
	    result_color += material->getKd() * cosa * in_diff * material->getDiffuseColor(intersection);
	}

	// Reflection
	Vector2 fre = fresnel(normal,ray.getDirection(),material);
	const double reflection = fre[0];
	const double transmission = fre[1];

	/* Bounce a reflection off the intersected object */
	if (material->getKs() > 0 && reflection > 0) {
	    Vector refl_vector = -1 * ray.getDirection();
	    refl_vector = refl_vector.reflect(normal);
	    refl_vector.normalize();
	    if (material->glossEnabled()) {
		/* Perturb reflecteced ray */
		double max_angle = material->glossMaxAngle();
		refl_vector = Math::perturbVector(refl_vector,max_angle,seqs[depth]);
		//refl_vector = Math::perturbVector(refl_vector,max_angle);
	    }
	    Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
	    refl_ray.fromObject = object;
	    result_color += reflection * trace(refl_ray, depth - 1);
	}

	/* Should we send a ray through the intersected object? */
	if (material->getKt() > 0.0 && transmission > 0) {
	    // TODO: Use the ior the rays holds to allow eg. glass in water.
	    double ior = material->getEta();
	    Vector T = ray.getDirection().refract(normal,ior);
	    if (!(T == Vector(0,0,0))) {
		Ray trans_ray = Ray(point+0.1*T,T,ior);
		RGB trans_col = trace(trans_ray, depth - 1);
		result_color += transmission * trans_col;
	    } else {
		// Internal reflection, see page 757.
		/*
		Vector refl_vector = -1 * ray.getDirection();
		refl_vector = refl_vector.reflect(normal);
		refl_vector.normalize();
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		RGB refl_col = trace(refl_ray, depth - 1);
		result_color += transmission * refl_col;
		*/
	    }
	}
    }

 //   result_color.clip();
    return result_color;
}


