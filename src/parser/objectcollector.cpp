
#include "objectcollector.h"

ObjectCollector::ObjectCollector() {
    pushCollection();
}

void ObjectCollector::pushCollection() {
    the_stack.push_front(new list<SceneObjectNode*> );
}

ObjectGroupNode* ObjectCollector::popAsGroupNode() {
    ObjectGroupNode* result = new ObjectGroupNode();
    while (!the_stack.front()->empty()) {
	result->addSceneObjectNode(the_stack.front()->front());
	the_stack.front()->pop_front();
    }
    delete the_stack.front();
    the_stack.pop_front();
    return result;
}

ObjectListNode* ObjectCollector::popAsListNode() {
    ObjectListNode* result = new ObjectListNode();
    while (!the_stack.front()->empty()) {
	result->addSceneObjectNode(the_stack.front()->front());
	the_stack.front()->pop_front();
    }
    delete the_stack.front();
    the_stack.pop_front();
    return result;
}

void ObjectCollector::addObject(SceneObjectNode* node) {
    the_stack.front()->push_front(node);
}

