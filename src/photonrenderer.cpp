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
#include "camera.h"
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
#include "space/spacesubdivider.h"

PhotonRenderer::PhotonRenderer(RendererSettings* settings,  Scene* scene, SpaceSubdivider* spc) : Renderer(settings,scene,spc) {
}

void PhotonRenderer::init() {
    this->globalphotonmap = new GlobalPhotonMap(renderersettings->global_photons_num,renderersettings->estimate_radius,renderersettings->estimate_samples);
    this->causticsphotonmap = new CausticsMap(renderersettings->caustic_photons_num,renderersettings->estimate_radius,renderersettings->estimate_samples); 

    PhotonTracer* photontracer = new PhotonTracer(scene,space,globalphotonmap,causticsphotonmap);
    cout << "Tracing photons..." << endl;
    photontracer->trace();
    cout << "Done." << endl;

    
    cout << "Balancing photonmaps..." << endl;
    Stats::getUniqueInstance()->beginTimer("Balance photonmaps");
    int total_photons_num = renderersettings->global_photons_num + renderersettings->caustic_photons_num;
    globalphotonmap->scale_photon_power(1.0/double(total_photons_num));
    globalphotonmap->balance();
    causticsphotonmap->scale_photon_power(1.0/double(total_photons_num));
    causticsphotonmap->balance();
    Stats::getUniqueInstance()->endTimer("Balance photonmaps");
    cout << "Done." << endl;

    cout << "Precomputing irradiances..." << endl;
    Stats::getUniqueInstance()->beginTimer("Precomputing irradiance");
    globalphotonmap->preComputeIrradiances(4);
    Stats::getUniqueInstance()->endTimer("Precomputing irradiance");
    cout << "Done." << endl;

    delete photontracer;
    
    //irradiance_cache = new IrradianceCache(space->getWorldBoundingBox(),5);
    BoundingBox bbox = BoundingBox(Vector(-733,-733,-733),Vector(733,733,733));
    irradiance_cache = new IrradianceCache(bbox,renderersettings->cache_tolerance);

    qmc_sequence = new Halton(2,2);
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
    Stats::getUniqueInstance()->inc("Primary camera rays cast");
    bool intersected = space->intersectPrimary(ray);
    return traceSub(intersected, ray, 1);
}

RGBA PhotonRenderer::trace(const Ray& ray, int depth) {
    bool intersected = space->intersect(ray);
    return traceSub(intersected, ray, depth);
}

RGBA PhotonRenderer::traceSub(bool intersected, const Ray& ray, int depth) {
    Stats::getUniqueInstance()->inc("Total camera rays cast");
    RGBA color; 
    Intersection intersection;
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
	color = (color * v) + (scene->getFogColor() * (1-v));
    }
    return color;
}

RGB PhotonRenderer::shade(const Ray& ray, const Intersection& intersection, int depth) {
    Object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = object->normal(intersection);
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
    if (depth < 4) {
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

    result_color.clip();
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

/// Final gathering does one step of path tracing
Vector PhotonRenderer::finalGather(const Vector& point, const Vector& normal, const Vector& ray_dir, int gatherRays, int depth, double* hmd) const {
    assert(gatherRays > 0);

    Vector offset_point = point + (0.1*normal);

    Vector result = Vector(0.0,0.0,0.0);
    *hmd = 0;
    double* rnd;
    qmc_sequence->reset();
    unsigned int gatherHits = 0;
    for (int i = 0; i < gatherRays; i++) {
	rnd = qmc_sequence->getNext();
	Vector dir = normal.randomHemisphere(rnd[0],rnd[1]);
	//Vector dir = Math::perturbVector(normal,89);

	Ray ray = Ray(offset_point,dir,-1);
	if (space->intersect(ray)) {
	    gatherHits++;
	    Intersection* inter = space->getLastIntersection();
	    Vector hitpoint = inter->getPoint();
	    Vector hitnormal = inter->getObject()->normal(*inter);
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
	    const Material* material = inter->getObject()->getMaterial();
	    RGB diffuse_col = material->getDiffuseColor(*inter);
	    result += irra * diffuse_col;
	}
    }
	
    result *= 1.0 / double(gatherRays);
    *hmd = double(gatherHits) / *hmd;
    return result;
}

