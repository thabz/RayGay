
#include "irradiancecache.h"

/**
 * Constructor.
 *
 * @param tolerance the a-value in Greg Wards text.
 */
IrradianceCache::IrradianceCache(double tolerance) {
    this->tolerance = tolerance;
    this->inv_tolerance = 1.0 / tolerance;
}

void IrradianceCache::putEstimate(const Vector& point, const Vector& normal, const RGB& irrandiance, const double hmd) {
    nodes.push_back(CacheNode(point,normal,irrandiance,hmd,tolerance));
}

RGB IrradianceCache::getEstimate(const Vector& point, const Vector& normal) const {
    int nodes_num = nodes.size();
    RGB result = RGB(0.0,0.0,0.0);
    double weight_sum = 0;
    double weight;
    for(int i = 0; i < nodes_num; i++) {
	const CacheNode* node = &nodes[i];
	double dist = (point - node->getPoint()).norm();
	if (dist > node->getRadius())
	    continue;

	weight = node->getWeight(point,normal);

	if (weight < inv_tolerance)
	    continue;
	
	result = weight * node->getIrradiance();
	weight_sum += weight;
    }
    return result;
}

/**
 * Constructor for a cache node.
 */
IrradianceCache::CacheNode::CacheNode(const Vector& point, const Vector &normal, const RGB& irradiance, double hmd, double a) {
    this->point = point;
    this->normal = normal;
    this->irradiance = irradiance;
    this->hmd = hmd;
    this->radius = hmd*a*hmd*a;
}


/**
 * The weight function.
 *
 * Using Henrik Wann Jensens interpretation.
 */
double IrradianceCache::CacheNode::getWeight(const Vector& x, const Vector& n) const {
    double d1 = (x - point).length() / hmd;
    double d2 = sqrt(1.0 - n*normal);
    return 1.0 / (d1 + d2);
}
