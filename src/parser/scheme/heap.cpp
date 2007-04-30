
#include "heap.h"
#include <iostream>

Heap* Heap::unique_instance = NULL;

Heap::Heap() {
    counter = 0;
}

void Heap::addRoot(SchemeObject* root) {
    // Only add root if it's not already in list of roots
    roots.push_back(root);
}

bool Heap::timeToGarbageCollect() {
    return ++counter > 50;
}

void Heap::popRoot() {
    roots.pop_back();
}

void Heap::addAllocation(SchemeObject* o) {
    allocations.push_back(o);
}

void Heap::garbageCollect(list<SchemeObject*> &stack) {
    //cout << "BEFORE: Size of heap: " << allocations.size() << endl;
    //cout << "BEFORE: Size of roots: " << roots.size() << endl;
    counter = 0;
    mark(stack);
    sweep();
    //cout << "AFTER: Size of heap: " << allocations.size() << endl;
    //cout << "AFTER: Size of roots: " << roots.size() << endl << endl;
}

void Heap::mark(list<SchemeObject*> &stack) {
    for(list<SchemeObject*>::iterator i = stack.begin(); i != stack.end(); i++) {
        assert(*i != NULL);
        (*i)->mark();
    }   
    for(list<SchemeObject*>::iterator i = roots.begin(); i != roots.end(); i++) {
        assert(*i != NULL);
        (*i)->mark();
    }   
}

void Heap::sweep() {
    list<SchemeObject*>::iterator i = allocations.begin();
    while (i != allocations.end()) {
        SchemeObject* o = (*i);
        assert(o != NULL);
        bool in_use = o->in_use;
        o->in_use = false;
        if (!in_use && o->type() != SchemeObject::SYMBOL) {
            //cout << "Deleting " << o << endl;
            i = allocations.erase(i);
            delete o;
        } else {
            ++i;
        }
    }   
}

