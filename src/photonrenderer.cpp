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
#include "photonmap.h"
#include "photonsettings.h"

PhotonRenderer::PhotonRenderer(PhotonSettings* photonsettings, PhotonMap* photonmap) : Renderer() {
    this->photonmap = photonmap;
    this->photonsettings = photonsettings;
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
    RGB result_color = RGB(0.0,0.0,0.0);
    vector<Lightsource*> lights = scene->getLightsources();

    result_color += gatherIrradiance(point,normal,ray.getDirection());
 //   result_color.clip();
 //   return result_color;
    // TODO: Add radiance_estimate from caustics map
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
	    RGB refl_col = RGB(0.0,0.0,0.0);
	    if (material.glossEnabled()) {
		/* Distributed reflection */
		const double v = sin(material.glossMaxAngle());
		const double h = cos(material.glossMaxAngle());

		Vector corners[3];
		corners[0] = Vector(-v,v,h);
		corners[1] = Vector(v,v,h);
		corners[2] = Vector(-v,-v,h);
		corners[3] = Vector(v,-v,h);
		Matrix orient = Matrix::matrixOrient(refl_vector);
		orient = orient.inverse();
		for(unsigned int i = 0; i < 4; i++) {
		    corners[i] = orient*corners[i];
		    corners[i].normalize();
		}
		const unsigned int max_rays = material.glossRaysNum();
		Vector v1,v2;
		double x_factor,y_factor;
		for(unsigned int xx = 0; xx < max_rays; xx++) {
		    for(unsigned int yy = 0; yy < max_rays; yy++) {
			x_factor = (double(xx)+RANDOM(0,1)) / double(max_rays);
			y_factor = (double(yy)+RANDOM(0,1)) / double(max_rays);
			v1 = x_factor*corners[0] + (1-x_factor)*corners[1];
			v2 = x_factor*corners[2] + (1-x_factor)*corners[3];
			refl_vector =  y_factor*v1 + (1-y_factor)*v2;
			refl_vector.normalize();
			Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
			refl_col += trace(refl_ray, depth + 1);
		    }
		}
		refl_col *= 1.0/double(max_rays*max_rays);
	    } else {
		/* Single reflected ray */
		Ray refl_ray = Ray(point,refl_vector,ray.getIndiceOfRefraction());
		refl_col = trace(refl_ray, depth + 1);
	    }
	    result_color = result_color + material.getKs() * refl_col;
	}

	/* Should we send a ray through the intersected object? */
	if (material.transmission_coefficient > 0.0) {
	    // TODO: Use the ior the rays holds to allow eg. glass in water.
	    double ior = material.indice_of_refraction;
	    Vector T = ray.getDirection().refract(normal,ior);
	    if (!(T == Vector(0,0,0))) {
		Ray trans_ray = Ray(point+0.1*T,T,ior);
		RGB trans_col = trace(trans_ray, depth + 1);
		result_color += material.transmission_coefficient * trans_col;
	    } else {
		// TODO: Internal reflection, see page 757.
	    }
	}
    }

    result_color.clip();
    return result_color;
}

Vector PhotonRenderer::gatherIrradiance(const Vector& point, const Vector& normal, const Vector& ray_dir) const {
    if (photonsettings->final_gather_rays == 0) {
	return M_PI * photonmap->irradiance_estimate(point,normal,photonsettings->estimate_radius,photonsettings->estimate_samples) * 5000*100;
    }

    Vector result = Vector(0.0,0.0,0.0);

    /*
    NearestPhotons np;
    np.dist2 = (float*)alloca( sizeof(float)*(photonsettings->final_gather_rays+1) );
    np.index = (const Photon**)alloca( sizeof(Photon*)*(photonsettings->final_gather_rays+1) );
    np.pos[0] = point[0];
    np.pos[1] = point[1];
    np.pos[2] = point[2];
    np.max = photonsettings->final_gather_rays;;
    np.found = 0;
    np.got_heap = 0;
    np.dist2[0] = photonsettings->estimate_radius * photonsettings->estimate_radius;
    photonmap->locate_photons(&np,1);
    if (np.found < 4) return result;
*/
    Vector reflected = (-1 * ray_dir).reflect(normal);
    
    for (int i = 0; i < photonsettings->final_gather_rays; i++) {
 //   for (int i = 1; i < np.found; i++) {
	//const Photon* p = np.index[i];
	//Vector dir = double(-1) * photonmap->photon_dir(p);
	Vector dir = Math::perturbVector(reflected,DEG2RAD(30));

	Ray ray = Ray(point+0.1*dir,dir,-1);
	if (space->intersect(ray)) {
	    Intersection* inter = space->getLastIntersection();
	    Vector hitpoint = inter->getPoint();
	    Vector hitnormal = inter->getObject()->normal(*inter);
	    double cos = normal * dir;
	    // TODO: If too close use another recursive final gather instead for estimate
	    result += photonsettings->estimate_radius * cos * photonmap->irradiance_estimate(hitpoint,hitnormal,photonsettings->estimate_radius,photonsettings->estimate_samples);
	}
    }
	
    result *= M_PI / double(photonsettings->final_gather_rays);
    //result *= double(M_PI) / double(np.found - 1);
    result *= 5000;
    //result *= double(1) / M_PI;
    return result;
}

