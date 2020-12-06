
#ifndef GENERIC_KD_TREE_H
#define GENERIC_KD_TREE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "aabox.h"
#include <vector>

/*
 * Macros for accessing the packed KdNode.
 */
//#define leftNode(node) (node->left == 0 ? NULL : &(nodes[node->left]))
//#define rightNode(node) (node->left == 0 ? NULL : &(nodes[node->left+1]))
#define leftNode(node) (&(nodes[(node)->getLeft()]))
#define rightNode(node) (&(nodes[(node)->getLeft() + 1]))
#define getTopNode() (&(nodes[0]))

template <class ObjectType> class KdNode {
private:
  union {
    // [30 bits left/num | 2 bits axis]

    // Left child when not a leaf. Right child is left + 1.
    uint32_t left;
    // Number of objects when this is a leaf
    uint32_t num;

    // Axis where x,y,z is 0,1,2 and 3 denotes a leaf is
    // packed into left/num as first two bits.
  };
  union {
    // Enclosed objects when this is a leaf
    ObjectType **objects;
    // Enclosed object when this is a leaf with only one object
    ObjectType *object;
    // Position of splitting plane when not a leaf
    float splitPlane;
  };

public:
  void initLeafNode(uint32_t num, ObjectType **objects);
  void initInteriorNode(uint32_t axis, double plane, uint32_t left);
  bool isLeafNode() const;
  double getSplitValue() const;
  uint32_t getObjectNum() const;
  uint32_t getAxis() const;
  ObjectType *const *getObjects() const;
  uint32_t getLeft() const;
  ~KdNode();
};

template <class ObjectType> class BoundedObject {
public:
  AABox bbox;
  ObjectType *object;
};

template <class ObjectType> class GenericKdTree {

public:
  /// Destructor
  virtual ~GenericKdTree();

  /// Place a object in the kd-tree
  void addObject(ObjectType *obj);

  void prepare();

  /// The AABox around all objects added to the tree
  AABox boundingBox() const { return world_bbox; };

protected:
  /// Constructor
  GenericKdTree(uint32_t max_depth, uint32_t max_objs);

  AABox enclosure(BoundedObject<ObjectType> **bobs, uint32_t num) const;

  AABox world_bbox;
  uint32_t max_depth;

  // The kd-tree nodes
  vector<KdNode<ObjectType>> nodes;

private:
  bool prepared;
  uint32_t opt_max_depth;
  uint32_t opt_max_objs;

  // The I/O data for the findBestSplitPlane method
  struct CostResult {
    double axis; //> Output
    int dim;     //> Output
    int current_sort_dim;
    uint32_t left_index;  //> Output
    uint32_t right_index; //> Output
    double cost;
  };

  // The recursive prepare method
  void prepare(uint32_t num, const AABox &bbox, uint32_t depth,
               const uint32_t dest_idx);

  bool findBestSplitPlane(uint32_t size, const AABox &bbox,
                          CostResult &result) const;
  void findBestSplitPlane(uint32_t size, const AABox &bbox, CostResult &result,
                          uint32_t split_dim) const;

  BoundedObject<ObjectType> **left_bobs;
  BoundedObject<ObjectType> **right_bobs;

  std::vector<ObjectType *> *added_objects;
};

template <class ObjectType> class cmpL {
public:
  cmpL(uint32_t d) { this->d = d; }
  bool operator()(const BoundedObject<ObjectType> *const p1,
                  const BoundedObject<ObjectType> *const p2) const {
    return p1->bbox.minimum(d) < p2->bbox.minimum(d);
  }

private:
  uint32_t d;
};

template <class ObjectType> class cmpR {
public:
  cmpR(uint32_t d) { this->d = d; }
  bool operator()(const BoundedObject<ObjectType> *const p1,
                  const BoundedObject<ObjectType> *const p2) const {
    return p1->bbox.maximum(d) < p2->bbox.maximum(d);
  }

private:
  uint32_t d;
};

#include "space/generickdtree.cpp"

#endif
