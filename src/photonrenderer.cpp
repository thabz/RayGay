#include "photonrenderer.h"

#include <iostream>
#include <cmath>
#include <cassert>

#include "math/vector.h"
#include "math/functions.h"
#include "math/matrix.h"
#include "ray.h"
#include "intersection.h"
#include "objects/object.h"
#include "image/rgb.h"
#include "scene.h"
#include "cameras/camera.h"
#include "image/image.h"
#include "lights/lightsource.h"
#include "stats.h"
#include "photon/globalphotonmap.h"
#include "photon/causticsmap.h"
#include "photon/photontracer.h"
#include "photon/irradiancecache.h"
#include "renderersettings.h"
#include "math/halton.h"
#include "materials/material.h"
#include "space/kdtree.h"

PhotonRenderer::PhotonRenderer(RendererSettings* settings,  
			       Image* img,
	                       Scene* scene, 
			       KdTree* spc, 
			       RenderJobPool* job_pool,
			       uint thread_id,
			       GlobalPhotonMap* globalphotonmap, 
			       CausticsMap* causticsmap, 
			       IrradianceCache* irradiancecache) : Renderer(settings,img,scene,spc,job_pool,thread_id) {
    this->globalphotonmap = globalphotonmap;
    this->causticsphotonmap = causticsmap;
    this->irradiance_cache = irradiancecache;
    this->qmc_sequence = new Halton(2,2);
    this->gloss_sequence = new Halton(2,2);
}

PhotonRenderer::~PhotonRenderer() {
    delete qmc_sequence;
    delete globalphotonmap;
    delete causticsphotonmap;
}

RGBA PhotonRenderer::getPixel(const Vector2& v) {
    Ray ray = scene->getCamera()->getRay(v[0],v[1]);
    return tracePrimary(ray);
}

RGBA PhotonRenderer::tracePrimary(const Ray& ray) {
    Stats::getUniqueInstance()->inc(STATS_PRIMARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersectPrimary(ray, i);
    return traceSub(intersected, i, ray, 1);
}

RGBA PhotonRenderer::trace(const Ray& ray, int depth) {
    Stats::getUniqueInstance()->inc(STATS_SECONDARY_RAYS_CAST);
    Intersection i;
    bool intersected = space->intersect(ray, i);
    return traceSub(intersected, i, ray, depth);
}

RGBA PhotonRenderer::traceSub(bool intersected, const Intersection& intersection, const Ray& ray, const int depth) {
    Stats::getUniqueInstance()->inc(STATS_TOTAL_CAMERA_RAYS_CAST);
    RGBA color; 
    double intersect_distance;

    if (intersected) {
	intersect_distance = (intersection.getPoint() - ray.getOrigin()).length();
	color = shade(ray,intersection,depth);
    } else {
        color = scene->getBackgroundColor(ray);
	intersect_distance = HUGE_DOUBLE;
    }

    if (scene->fogEnabled()) {
	double D = scene->getFogDistance();
	double v = expf(-intersect_distance/D);
	color = (color * v) + (scene->getFogColor() * (1-v));
    }
    return color;
}

RGB PhotonRenderer::shade(const Ray& ray, const Intersection& intersection, int depth) {
    Lightinfo info;
    Object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = intersection.getNormal();
    const Material* material = object->getMaterial();
    normal = material->bump(intersection,normal);
    
    Vector2 fre = fresnel(normal,ray.getDirection(),material);
    double reflection = fre[0];
    double transmission = fre[1];

    RGB material_diffuse = material->getDiffuseColor(intersection);

    RGB result_color = RGB(0.0,0.0,0.0);

    // Indirect diffuse light by one step of path tracing
    result_color += material_diffuse * getDiffuseIrradiance(point,normal,ray.getDirection());

    // Direct estimate from caustics map
    result_color += material_diffuse * causticsphotonmap->getFilteredIrradianceEstimate(point,normal);
    //result_color.clip();
    //return result_color;

    vector<Lightsource*> lights = scene->getLightsources();
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
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
			double power = 1; //(*p)->getPower().brightness();
			color =  power * intensity * info.cos * material->getKd() * material->getDiffuseColor(intersection);
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
		gloss_sequence->reset();
		for(int i = 0; i < gloss_rays; i++) {
		    Ray refl_ray = Ray(point,Math::perturbVector(refl_vector,max_angle,gloss_sequence),ray.getIndiceOfRefraction());
		    refl_col += trace(refl_ray, depth + 1);
		}
		refl_col *= 1.0/double(gloss_rays);
	    } else {
		/* Single reflected ray */
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		refl_col = trace(refl_ray, depth + 1);
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
		RGB trans_col = trace(trans_ray, depth + 1);
		result_color += transmission * trans_col;
	    } else {
		// Internal reflection, see page 757.
		Vector refl_vector = -1 * ray.getDirection();
		refl_vector = refl_vector.reflect(normal);
		refl_vector.normalize();
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		RGB refl_col = trace(refl_ray, depth + 1);
		result_color += transmission * refl_col;
	    }
	}
    }
    return result_color;
}

/**
 * Find the diffuse irradiance at a point.
 *
 * This uses the IrradianceCache.
 */
RGB PhotonRenderer::getDiffuseIrradiance(const Vector& point, const Vector& normal, const Vector& ray_dir) const {

    if (renderersettings->final_gather_rays == 0) {
	return globalphotonmap->irradianceEstimate(point,normal);
    }

    double hmd;
 //   return finalGather(point, normal, ray_dir, renderersettings->final_gather_rays, 0, &hmd);

    RGB irradiance;
    bool success = irradiance_cache->getEstimate(point,normal,&irradiance);

    if (!success) {
    //if (!success || RANDOM(0,100) < 1.0) {
	irradiance = finalGather(point, normal, ray_dir, renderersettings->final_gather_rays, 0, &hmd);
	irradiance_cache->putEstimate(point,normal,irradiance,hmd);
	//irradiance = RGB(10000.0,10000.0,10000.0);
    } 
    return irradiance;
}

/// 
/**
 * Final gathering does one step of path tracing.
 *
 * @param hmd Harmonic means distance is written here.
 */
RGB PhotonRenderer::finalGather(const Vector& point, const Vector& normal, const Vector& ray_dir, int gatherRays, int depth, double* hmd) const {
    assert(gatherRays > 0);

    Vector offset_point = point + (0.1*normal);

    RGB result = RGB(0.0,0.0,0.0);
    *hmd = HUGE_DOUBLE;
    double* rnd;
    qmc_sequence->reset();
    uint gatherHits = 0;
    for (int i = 0; i < gatherRays; i++) {
	rnd = qmc_sequence->getNext();
	Vector dir = normal.randomHemisphere(rnd[0],rnd[1]);
	//Vector dir = Math::perturbVector(normal,89);

	Ray ray = Ray(offset_point,dir,-1);
	Intersection inter;
	if (space->intersect(ray, inter)) {
	    gatherHits++;
	    Vector hitpoint = inter.getPoint();
	    Vector hitnormal = inter.getNormal();
	    RGB irra;
	    double dist = (hitpoint-point).length();
	    *hmd += 1.0 / dist;
	    //    if ( dist < renderersettings->estimate_radius && depth == 0 ) {
	    if (false) {
		// If too close do additional level of path tracing
		//irra += finalGather(hitpoint,hitnormal,dir,7, depth + 1);
	    } else {
		//irra += globalphotonmap->directIrradianceEstimate(hitpoint,hitnormal);
		irra += globalphotonmap->irradianceEstimate(hitpoint,hitnormal);
	    }
	    const Material* material = inter.getObject()->getMaterial();
	    RGB diffuse_col = material->getDiffuseColor(inter);
	    result += irra * diffuse_col;
	}
	}

	if (gatherHits != 0) {
	    result *= 1.0 / double(gatherRays);
	    *hmd = double(gatherHits) / *hmd;
	}
	return result;
    }

