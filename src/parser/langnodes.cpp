
#include "parser/langnodes.h"
#include "objects/blob.h"

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
	    // TODO: Runtime exception
	}
	blob->addAtom(s->getCenter(),s->getRadius(),weight_v);
    }
    return blob;
}
