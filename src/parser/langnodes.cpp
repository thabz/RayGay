
#include "parser/langnodes.h"
#include "objects/blob.h"
#include "objects/csg.h"
#include "objects/sphere.h"
#include "objects/objectgroup.h"

//---------------------------------------------------------------------
// ActionListNode
//---------------------------------------------------------------------
ActionListNode::ActionListNode() {
};

ActionListNode::~ActionListNode() {
    for(unsigned int i = 0; i < actions.size(); i++) {
	delete actions[i];
    }
}

void ActionListNode::addAction(ActionNode* action) {
    actions.push_back(action);
}

void ActionListNode::eval() {
    for(unsigned int i = 0; i < actions.size(); i++) {
	actions[i]->eval();
    }
}

//---------------------------------------------------------------------
// RepeatActionNode
//---------------------------------------------------------------------
RepeatActionNode::RepeatActionNode(FloatNode* num, ActionListNode* list) {
    this->num = num;
    this->list = list;
}

RepeatActionNode::~RepeatActionNode() {
    delete num;
    delete list;
}

void RepeatActionNode::eval() {
    int n = int(num->eval());
    if (n < 0) {
	throw_exception("Can't repeat negative number of times.");
    }
    for(int i = 0; i < n; i++) {
	list->eval();
    }
}

//---------------------------------------------------------------------
// WhileActionNode
//---------------------------------------------------------------------
WhileActionNode::WhileActionNode(BoolNode* cond, ActionListNode* list) {
    this->cond = cond;
    this->list = list;
}

WhileActionNode::~WhileActionNode() {
    delete cond;
    delete list;
}

void WhileActionNode::eval() {
    while ( cond->eval() ) {
	list->eval();
    }
}

//---------------------------------------------------------------------
// DoWhileActionNode
//---------------------------------------------------------------------
DoWhileActionNode::DoWhileActionNode(ActionListNode* list, BoolNode* cond) {
    this->cond = cond;
    this->list = list;
}

DoWhileActionNode::~DoWhileActionNode() {
    delete cond;
    delete list;
}

void DoWhileActionNode::eval() {
    do {
	list->eval();
    } while ( cond->eval() );
}

//---------------------------------------------------------------------
// IfActionNode
//---------------------------------------------------------------------
IfActionNode::IfActionNode(BoolNode* cond, ActionListNode* l1, ActionListNode* l2) {
    this->cond = cond;
    this->list1 = l1;
    this->list2 = l2;
}

IfActionNode::~IfActionNode() {
    delete cond;
    delete list1;
    if (list2 != NULL) {
	delete list2;
    }
}

void IfActionNode::eval() {
    if ( cond->eval() ) {
	list1->eval();
    } else {
	if (list2 != NULL) {
	    list2->eval();
	}
    }
}

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

//---------------------------------------------------------------------
// ObjectGroupNode
//---------------------------------------------------------------------
ObjectGroupNode::ObjectGroupNode(ActionListNode* actions) {
    this->actions = actions;
};

ObjectGroupNode::~ObjectGroupNode() {
    delete actions;
}

SceneObject* ObjectGroupNode::eval() {
    ObjectCollector* oc = Environment::getUniqueInstance()->getObjectCollector();
    // Push a new object collector
    oc->pushCollection();

    // eval actions;
    actions->eval();

    // Pop collector and insert into a ObjectGroup* result;
    vector<SceneObject*> nodes = oc->pop();
    ObjectGroup* result = new ObjectGroup();
    for(unsigned int i = 0; i < nodes.size(); i++) {
	result->addObject(nodes[i]);
    }
    return result;
}

