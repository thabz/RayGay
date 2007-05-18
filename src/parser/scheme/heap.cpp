
#include "heap.h"
#include "scheme.h"
#include <iostream>

Heap* Heap::unique_instance = NULL;

Heap::Heap(uint32_t slots_num) {
    allocations = new SchemeObject[slots_num];
    for(uint i = 0; i < slots_num; i++) {
        allocations[i].metadata = SchemeObject::BLANK;
    }
    next_free = allocations;
    free_slots = slots_num;
    this->slots_num = slots_num;
}

void Heap::addRoot(SchemeObject* root) {
    roots.push_back(root);
}

bool Heap::timeToGarbageCollect() {
    return next_free > &allocations[int(0.9 * SLOTS_NUM)];
}

void Heap::popRoot() {
    roots.pop_back();
}

SchemeObject* Heap::allocate(SchemeObject::ObjectType metadata) {
    while (next_free->type() != SchemeObject::BLANK && next_free < &allocations[SLOTS_NUM]) {
        next_free++;
    }
    if (next_free >= &allocations[SLOTS_NUM]) {
        throw scheme_exception("Out of heap space");
    }
    SchemeObject* result = next_free;
    result->metadata = uint32_t(metadata);
    result->set_immutable(false);
    next_free++;
    free_slots--;
    return result;
}

void Heap::garbageCollect(vector<SchemeObject*> &stack) {
    //cout << "BEFORE: Size of heap: " << slots_num - free_slots << endl;
    //cout << "BEFORE: Size of roots: " << roots.size() << endl;
    mark(stack);
    sweep();
    //cout << "AFTER: Size of heap: " << slots_num - free_slots << endl;
    //cout << "AFTER: Size of roots: " << roots.size() << endl << endl;
}

void Heap::mark(vector<SchemeObject*> &stack) {
    for(vector<SchemeObject*>::iterator i = stack.begin(); i != stack.end(); i++) {
        assert(*i != NULL);
        (*i)->mark();
    }   
    for(list<SchemeObject*>::iterator i = roots.begin(); i != roots.end(); i++) {
        assert(*i != NULL);
        (*i)->mark();
    }   
}

void Heap::sweep() {
    SchemeObject* cur = allocations;
    do {
        if (cur->type() == SchemeObject::BLANK) {
            if (cur < next_free) {
                cout << "Moved free pointer" << endl;
                next_free = cur;
            }
        } else {
            bool in_use = cur->inuse();
            cur->clear_inuse();
            if (!in_use && cur->type() != SchemeObject::SYMBOL) {
                cur->finalize();
                cur->metadata = SchemeObject::BLANK;
                free_slots++;
            }
        }
    } while (++cur != &allocations[SLOTS_NUM]);
    next_free = allocations;
}
