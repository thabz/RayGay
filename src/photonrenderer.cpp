#include "photonrenderer.h"

#include <iostream>
#include <cmath>

#include "math/vector.h"
#include "math/functions.h"
#include "math/matrix.h"
#include "ray.h"
#include "intersection.h"
#include "object.h"
#include "image/rgb.h"
#include "scene.h"
#include "camera.h"
#include "image/image.h"
#include "lights/lightsource.h"

#include "photon/globalphotonmap.h"
#include "photon/causticsmap.h"
#include "photon/photontracer.h"
#include "renderersettings.h"
#include "math/halton.h"

PhotonRenderer::PhotonRenderer(RendererSettings* settings,  Scene* scene, SpaceSubdivider* spc) : Renderer(settings,scene,spc) {
}

void PhotonRenderer::init() {
    int PHOTON_NUM = renderersettings->photons_num;
    this->globalphotonmap = new GlobalPhotonMap(PHOTON_NUM);
    this->causticsphotonmap = new CausticsMap(PHOTON_NUM); 

    PhotonTracer* photontracer = new PhotonTracer(scene,space,globalphotonmap,causticsphotonmap);
    photontracer->trace(PHOTON_NUM);

    globalphotonmap->scale_photon_power(1.0/double(PHOTON_NUM));
    globalphotonmap->balance();

    causticsphotonmap->scale_photon_power(1.0/double(PHOTON_NUM));
    causticsphotonmap->balance();

    delete photontracer;

    qmc_sequence = new Halton(2,2);
}

RGB PhotonRenderer::getPixel(const Vector2& v) {
    Ray ray = scene->getCamera()->getRay(v[0],v[1]);
    return tracePrimary(ray);
}

RGB PhotonRenderer::tracePrimary(const Ray& ray) {
    Stats::getUniqueInstance()->inc("Primary camera rays cast");
    bool intersected = space->intersectPrimary(ray);
    return traceSub(intersected, ray, 1);
}

RGB PhotonRenderer::trace(const Ray& ray, int depth) {
    bool intersected = space->intersect(ray);
    return traceSub(intersected, ray, depth);
}

RGB PhotonRenderer::traceSub(bool intersected, const Ray& ray, int depth) {
    Stats::getUniqueInstance()->inc("Total camera rays cast");
    RGB color; 
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
	color = v * color + (1-v) * scene->getFogColor();
    }
    return color;
}

RGB PhotonRenderer::shade(const Ray& ray, const Intersection& intersection, int depth) {
    Object* object = intersection.getObject();
    const Vector point = intersection.getPoint();
    Vector normal = object->normal(intersection);
    const Material& material = object->getMaterial();
    normal = material.bump(intersection,normal);
    
    Vector2 fre = fresnel(normal,ray.getDirection(),material);
    double reflection = fre[0];
    double transmission = fre[1];

    RGB result_color = RGB(0.0,0.0,0.0);

    result_color += gatherIrradiance(point,normal,ray.getDirection());
    //result_color.clip();
    //return result_color;
    // TODO: Add radiance_estimate from caustics map

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
		result_color += color;
	    } 
	}
    }
    if (depth < 4) {
	/* Bounce a reflection off the intersected object */
	if (material.getKs() > 0 && reflection > 0) {
	    Vector refl_vector = -1 * ray.getDirection();
	    refl_vector = refl_vector.reflect(normal);
	    refl_vector.normalize();
	    RGB refl_col = RGB(0.0,0.0,0.0);
	    if (material.glossEnabled()) {
		/* Distributed reflection */
		double max_angle = material.glossMaxAngle();
		int gloss_rays = material.glossRaysNum();
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
	if (material.getKt() > 0.0 && transmission > 0) {
	    // TODO: Use the ior the rays holds to allow eg. glass in water.
	    double ior = material.getEta();
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

Vector PhotonRenderer::gatherIrradiance(const Vector& point, const Vector& normal, const Vector& ray_dir) const {
    if (renderersettings->final_gather_rays == 0) {
	return M_PI * globalphotonmap->irradiance_estimate(point,normal,renderersettings->estimate_radius,renderersettings->estimate_samples) * 5000*100;
    }

    Vector result = Vector(0.0,0.0,0.0);
    double* rnd;
    qmc_sequence->reset();
    for (int i = 0; i < renderersettings->final_gather_rays; i++) {
	rnd = qmc_sequence->getNext();
	Vector dir = normal.randomHemisphere(rnd[0],rnd[1]);

	Ray ray = Ray(point+0.1*dir,dir,-1);
	if (space->intersect(ray)) {
	    Intersection* inter = space->getLastIntersection();
	    Vector hitpoint = inter->getPoint();
	    Vector hitnormal = inter->getObject()->normal(*inter);
	    double cos = normal * dir;
	    // TODO: If too close use another recursive final gather instead for estimate
	    result += renderersettings->estimate_radius * cos * globalphotonmap->irradiance_estimate(hitpoint,hitnormal,renderersettings->estimate_radius,renderersettings->estimate_samples);
	}
    }
	
    result *= M_PI / double(renderersettings->final_gather_rays);
    //result *= double(M_PI) / double(np.found - 1);
    result *= 5000;
    //result *= double(1) / M_PI;
    return result;
}

