
#include "objectcollector.h"
#include <string>    
#include <iostream>

using namespace std;

ObjectCollector::ObjectCollector() {
    reset();
}

/**
 * The ObjectCollector must be reset between two executions of the
 * abstract parse tree.
 */
void ObjectCollector::reset() {
    if (the_stack.empty()) {
	pushCollection();
    }
}

void ObjectCollector::pushCollection() {
    the_stack.push_front(new list<SceneObject*> );
}

vector<SceneObject*> ObjectCollector::pop() {
    vector<SceneObject*> result;
    while (!(the_stack.front()->empty())) {
	result.push_back(the_stack.front()->front());
	the_stack.front()->pop_front();
    }
    delete the_stack.front();
    the_stack.pop_front();
    return result;
}

void ObjectCollector::addObject(SceneObject* node) {
    the_stack.front()->push_front(node);
}

