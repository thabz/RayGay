
#include "photon/irradiancecache.h"
#include "stats.h"

/**
 * Constructor.
 *
 * @param tolerance the a-value in Greg Wards text.
 * @param bbox the bounding box of the scene 
 */
IrradianceCache::IrradianceCache(const BoundingBox& bbox, double tolerance) {
    this->tolerance = tolerance;
    this->inv_tolerance = 1.0 / tolerance;
    this->hierarchy_top = new HierarchyNode(bbox);
}

void IrradianceCache::putEstimate(const Vector& point, const Vector& normal, const RGB& irrandiance, const double hmd) {
    Stats::getUniqueInstance()->inc("Irradiance cache size");
    hierarchy_top->cache_nodes.push_back(CacheNode(point,normal,irrandiance,hmd,tolerance));
}

RGB IrradianceCache::getEstimate(const Vector& point, const Vector& normal) const {
    int nodes_num = hierarchy_top->cache_nodes.size();
    RGB result = RGB(0.0,0.0,0.0);
    double weight_sum = 0;
    double weight;
    int found = 0;
    for(int i = 0; i < nodes_num; i++) {
	const CacheNode* node = &(hierarchy_top->cache_nodes[i]);

	weight = node->getWeight(point,normal);
	if (weight < inv_tolerance)
	    continue;
	
	result = weight * node->getIrradiance();
	weight_sum += weight;
	found++;
    }
    if (found > 2) {
	Stats::getUniqueInstance()->inc("Irradiance cache hits");
	return result / weight_sum;
    } else {
	Stats::getUniqueInstance()->inc("Irradiance cache misses");
	return RGB(-1.0,-1.0,-1.0);
    }
}

void IrradianceCache::traverseOctree(const HierarchyNode* const node, const Vector& point, vector<const CacheNode*>* result) const {
    if (node->isLeaf) {
	// Add cache_nodes to result
	for(unsigned int i = 0; i < node->cache_nodes.size(); i++) {
	    const CacheNode* const cnode = &(node->cache_nodes[i]);
	    if ((point - cnode->getPoint()).norm() < cnode->getSquaredRadius()) {
		result->push_back(cnode);
	    }
	}
    } else {
	// traverse children
	for(unsigned int i = 0; i < 8; i++) {
	    HierarchyNode* child = node->children[i];
	    // FIXME: Simpler test possible
	    if (child->bbox.inside(point)) {
		traverseOctree(child,point,result);
	    }
	}
    }
}

/**
 * Constructor for a cache node.
 */
IrradianceCache::CacheNode::CacheNode(const Vector& point, const Vector &normal, const RGB& irradiance, double hmd, double a) {
    this->point = point;
    this->normal = normal;
    this->irradiance = irradiance;
    this->hmd = hmd;
    this->squared_radius = hmd*a*hmd*a;
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

IrradianceCache::HierarchyNode::HierarchyNode(const BoundingBox& bbox) {
    this->bbox = bbox;
    this->isLeaf = true;
    for(int i = 0; i < 8; i++) {
	children[i] = NULL;
    }
}

void IrradianceCache::HierarchyNode::add(const CacheNode& node) {
    if (isLeaf) {
	if (bbox.intersectSphere(node.getPoint(),node.getSquaredRadius())) {
	    cache_nodes.push_back(node);
	    // TODO: call split() if this node holds too many children
	}
    } else {
	// Add node to children (if they want it)
	for(unsigned int j = 0; j < 8; j++) {
	    children[j]->add(node);
	}
    }
}

/**
 * Create 8 children and copy my cache_nodes to them.
 */
void IrradianceCache::HierarchyNode::split() {
    // TODO: Create 8 children with right bbox'

    // Copy nodes to children if they want them
    unsigned int nodes_num = cache_nodes.size();
    for (unsigned int i = 0; i < nodes_num; i++) {
	for(unsigned int j = 0; j < 8; j++) {
	    children[j]->add(cache_nodes[i]);
	}
    }
    // Clean up this node and mark it as a knot
    cache_nodes.clear();
    isLeaf = false;
}
