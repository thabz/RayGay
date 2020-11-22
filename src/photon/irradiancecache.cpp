
#include "photon/irradiancecache.h"
#include "stats.h"

/**
 * Constructor.
 *
 * The tolerance should be in [0,1]. The smaller the value
 * the better the estimate. The default is 0.1.
 *
 * @param tolerance the a-value in Greg Wards text.
 * @param bbox the bounding box of the scene
 */
IrradianceCache::IrradianceCache(const AABox &bbox, double tolerance = 0.1) {
  this->tolerance = tolerance;
  this->inv_tolerance = 1.0 / tolerance;
  this->hierarchy_top = new HierarchyNode(bbox, 0);
  pthread_mutex_init(&mutex, NULL);
}

void IrradianceCache::putEstimate(const Vector &point, const Vector &normal,
                                  const RGB &irradiance, const double hmd) {
  pthread_mutex_lock(&mutex);
  Stats::getUniqueInstance()->inc(STATS_IRRADIANCE_CACHE_SIZE);
  hierarchy_top->add(CacheNode(point, normal, irradiance, hmd, tolerance));
  pthread_mutex_unlock(&mutex);
}

bool IrradianceCache::getEstimate(const Vector &point, const Vector &normal,
                                  RGB *dest) const {
  RGB result = RGB(0.0, 0.0, 0.0);
  double weight_sum = 0;
  int found = 0;

  vector<const CacheNode *> nodes_found;

  pthread_mutex_lock(&mutex);

  traverseOctree(hierarchy_top, point, &nodes_found);
  int nodes_num = nodes_found.size();

  for (int i = 0; i < nodes_num; i++) {
    const CacheNode *node = nodes_found[i];

    double weight = node->getWeight(point, normal);

    if (weight >= inv_tolerance) {
      result += weight * node->getIrradiance();
      weight_sum += weight;
      found++;
    }
  }
  pthread_mutex_unlock(&mutex);
  if (found > 0) {
    Stats::getUniqueInstance()->inc(STATS_IRRADIANCE_CACHE_HITS);
    *dest = result / weight_sum;
    return true;
  } else {
    Stats::getUniqueInstance()->inc(STATS_IRRADIANCE_CACHE_MISSES);
    return false;
  }
}

void IrradianceCache::traverseOctree(const HierarchyNode *const node,
                                     const Vector &point,
                                     vector<const CacheNode *> *result) const {
  // Add cache_nodes to result
  for (uint32_t i = 0; i < node->cache_nodes.size(); i++) {
    const CacheNode *const cnode = &(node->cache_nodes[i]);
    if ((point - cnode->getPoint()).norm() <= cnode->getSquaredRadius()) {
      result->push_back(cnode);
    }
  }

  if (node->isSplit) {
    // traverse children
    for (uint32_t i = 0; i < 8; i++) {
      const HierarchyNode *child = node->children[i];
      // FIXME: Simpler test possible
      if (child->bbox.insideOrTouching(point)) {
        traverseOctree(child, point, result);
      }
    }
  }
}

/**
 * Constructor for a cache node.
 */
IrradianceCache::CacheNode::CacheNode(const Vector &point, const Vector &normal,
                                      const RGB &irradiance, double hmd,
                                      double a) {
  for (int i = 0; i < 3; i++) {
    this->point[i] = point[i];
    this->normal[i] = normal[i];
    this->irradiance[i] = irradiance[i];
  }
  this->hmd = hmd;
  this->squared_radius = hmd * a * hmd * a;
}

/**
 * The weight function.
 *
 * Using Henrik Wann Jensens interpretation.
 */
double IrradianceCache::CacheNode::getWeight(const Vector &x,
                                             const Vector &n) const {
  double d1 = (x - getPoint()).length() / hmd;
  double d2 = sqrt(1.0 - n * getNormal());
  return 1.0 / (d1 + d2);
}

IrradianceCache::HierarchyNode::HierarchyNode(const AABox &bbox,
                                              uint32_t depth) {
  this->bbox = bbox;
  this->isSplit = false;
  this->depth = depth;
  this->length = bbox.maximum()[0] - bbox.minimum()[0];
}

void IrradianceCache::HierarchyNode::add(const CacheNode &node) {
  if (depth > IRRADIANCE_OCTREE_MAX_DEPTH ||
      node.getSquaredRadius() * 2 > length * length) {
    cache_nodes.push_back(node);
  } else {
    if (!isSplit)
      split();

    // Find how many and which siblings this node fits in
    bool accepted[8];
    int accepted_count = 0;
    for (uint32_t j = 0; j < 8; j++) {
      accepted[j] = false;
      HierarchyNode *const child = children[j];
      if (child->bbox.intersectSphere(node.getPoint(),
                                      node.getSquaredRadius())) {
        accepted_count++;
        accepted[j] = true;
      }
    }

    if (accepted_count == 8 || accepted_count == 0) {
      // It fits in all children, so keep it in this octree node
      cache_nodes.push_back(node);
    } else {
      // Put it in siblings
      for (uint32_t j = 0; j < 8; j++) {
        if (accepted[j]) {
          children[j]->add(node);
        }
      }
    }
  }
}

/**
 * Create 8 children and copy my cache_nodes to them.
 */
void IrradianceCache::HierarchyNode::split() {
  if (isSplit)
    return;

  // Create 8 children with right bbox'
  Vector *corners = bbox.getCorners();
  Vector center = bbox.center();
  for (int i = 0; i < 8; i++) {
    children[i] = new HierarchyNode(AABox(center, corners[i]), depth + 1);
  }
  delete[] corners;

  isSplit = true;
}

IrradianceCache::HierarchyNode::~HierarchyNode() {
  if (isSplit) {
    for (uint32_t i = 0; i < 8; i++) {
      delete children[i];
    }
  }
}
