
#include "camera.h"

#include <iostream>
#include <math.h>

#include "vector.h"
#include "ray.h"
#include "intersection.h"
#include "object.h"
#include "rgb.h"
#include "matrix.h"
#include "scene.h"
#include "image.h"
#include "lightsource.h"


Camera::Camera(Vector p, Vector d, Scene& sc) {
    position = p;
    direction = d;
    scene = sc;
}

Camera::~Camera() {
}

void Camera::transform(Matrix &m) {
    position = m * position;
}

Vector & Camera::getPosition() {
    return position;
}

void Camera::render(Image* img) {
    scene.prepare();
    Ray ray;
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    RGB* mesh = new RGB[(img_w+1)*(img_h+1)*3];

    for (double y = -img_h; y < img_h; y++) {
        for (double x = -img_w; x < img_w; x++) {
	    RGB color = getPixel(x,y);
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
    }
}

RGB Camera::getPixel(double x, double y) {
    Vector scr = Vector(x,y,0);
    Vector raydir = scr - position;
    raydir.normalize();
    Ray ray = Ray(position,raydir,1.0);
    return trace(ray,1);
}


RGB Camera::trace(const Ray& ray, int depth) {
    Intersection intersection = scene.intersect(ray);
    RGB color; // Set to scene's ambient color
    if (intersection.intersected) {
	color = shade(ray,intersection,depth);
    } else {
        color = scene.getBackgroundColor();
    }
    return color;
}

RGB Camera::shade(const Ray& ray, Intersection& intersection, int depth) {
    object* object = intersection.getObject();
    const Vector point = intersection.point;
    const Vector normal = object->normal(intersection);
    Material material = object->getMaterial();
    RGB result_color;
    vector<Lightsource*> lights = scene.getLightsources();
    for (vector<Lightsource*>::iterator p = lights.begin(); p != lights.end(); p++) {
	RGB color = RGB(0.0,0.0,0.0);
	Vector light_pos = (*p)->getPosition();;
	RGB ambient_color = RGB(1.0,1.0,1.0);
	Vector direction_to_light = light_pos - point;
	direction_to_light.normalize();
	double cos = direction_to_light * normal;
	if (cos > 0.0) {
	    // Check for blocking objects
	    Ray ray_to_light = Ray(point,direction_to_light,-1.0);
	    Intersection inter = scene.intersect(ray_to_light);
	    if (!inter.intersected) {
		double intensity = (*p)->getIntensity(direction_to_light,cos);
		// Diffuse color
		color =  intensity * cos * material.getKd() * material.getDiffuseColor(intersection);

		// Specular color (Phong)
		Vector light_reflect = direction_to_light.reflect(normal);
		light_reflect.normalize();
		double rv = light_reflect * (-1 * ray.direction);
		if (rv > 0.0) {
		   rv = pow(rv,material.getSc());
		   color = color + ( intensity * rv *  material.getKs() * material.getSpecularColor());
		}
	    }
	    
	} 
	result_color = result_color + color;
    }
    if (depth < 4) {
        /* Bounce a reflection off the intersected object */
	if (1 == 1) {
	    Vector refl_vector = -1 * ray.direction;
	    refl_vector = refl_vector.reflect(normal);
	    refl_vector.normalize();
	    Ray refl_ray = Ray(point,refl_vector,ray.indice_of_refraction);
	    RGB refl_col = trace(refl_ray, depth + 1);
	    result_color = result_color + material.getKs() * refl_col;
	}

	/* Should we send a ray through the intersected object? */
	if (material.transmission_coefficient > 0.0) {
	    // Calculate refraction vector (page 757)
	    double my = ray.indice_of_refraction / material.indice_of_refraction;
	    Vector I = -1 * ray.direction;
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

