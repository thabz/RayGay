
#include "parser/langnodes.h"
#include "objects/blob.h"
#include "objects/csg.h"
#include "objects/sphere.h"

//---------------------------------------------------------------------
// UnionNode 
//---------------------------------------------------------------------
SceneObject* UnionNode::eval() {
    ObjectCollector* oc = Environment::getUniqueInstance()->getObjectCollector();
    // Push a new object collector
    oc->pushCollection();

    // eval actions;
    actions->eval();


    // Pop collector and insert into a Union* result;
    vector<Solid*> solids;
    vector<SceneObject*> sos = oc->pop();
    for(unsigned int i = 0; i < sos.size(); i++) {
	Solid* s = dynamic_cast<Solid*>(sos[i]);
	if (s == NULL) {
	    // TODO: Runtime exception ("A union can only contain solids")
	}
	solids.push_back(s);
    }
    Material* m = material->eval();
    return new CSGUnion(&solids,m);
}

//---------------------------------------------------------------------
// BlobNode
//---------------------------------------------------------------------

BlobNode::BlobNode(FloatNode* iso, FloatNode* weight, FloatNode* steps, FloatNode* accuracy, ObjectGroupNode* spheres, MaterialNode* mat) {
    this->iso = iso;
    this->weight = weight;
    this->steps = steps;
    this->accuracy = accuracy;
    this->spheres = spheres;
    this->material = mat;
}

BlobNode::~BlobNode() {
    delete steps;
    delete accuracy;
    delete iso;
    delete weight;
    delete spheres;
    delete material;
}

SceneObject* BlobNode::eval() {
    double iso_v = iso->eval();
    double accuracy_v = accuracy->eval();
    double weight_v = weight->eval();
    unsigned int steps_v = (unsigned int)(steps->eval());
    Material* m = material->eval();
    Blob* blob = new Blob(iso_v,steps_v,accuracy_v,m);
    ObjectGroup* og = dynamic_cast<ObjectGroup*>(spheres->eval());
    vector<SceneObject*> spheres_v = og->getObjects();
    for(unsigned int i = 0; i < spheres_v.size(); i++) {
	Sphere* s = dynamic_cast<Sphere*>(spheres_v[i]);
	if (s == NULL) {
	    // TODO: Runtime exception ("A blob can only contain spheres")
	}
	blob->addAtom(s->getCenter(),s->getRadius(),weight_v);
    }
    return blob;
}
