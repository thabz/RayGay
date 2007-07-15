
#include "heap.h"
#include "scheme.h"
#include <iostream>

Heap* Heap::unique_instance = NULL;

Heap::Heap(uint32_t slots_per_bank) {
    this->slots_per_bank = slots_per_bank;
    this->free_slots = 0;
    this->slots_num = 0;
    this->allocated = 0;
    allocateNewBank();
}

void Heap::addRoot(SchemeObject* root) {
    roots.push_back(root);
}

void Heap::popRoot() {
    roots.pop_back();
}

void Heap::allocateNewBank() {
    SchemeObject* bank = new SchemeObject[slots_per_bank];
    banks.push_back(bank);
    for(uint i = 0; i < slots_per_bank; i++) {
        bank[i].metadata = SchemeObject::BLANK;
    }
    next_free_slot_idx = 0;
    cur_bank_idx = banks.size() - 1;
    free_slots += slots_per_bank;
    slots_num += slots_per_bank;
    //cout << "New bank allocated " << endl;
}

SchemeObject* Heap::allocate(SchemeObject::ObjectType metadata) {
    SchemeObject* result = NULL;
    bool done = false;
    while (!done) {
        SchemeObject* cur_bank = banks[cur_bank_idx];    
        while (next_free_slot_idx < slots_per_bank && !done) {
            SchemeObject* p = &(cur_bank[next_free_slot_idx]);
            if (p->type() == SchemeObject::BLANK) {
                result = p;
                done = true;        
            }
            next_free_slot_idx++;
        }
        if (!done) {
            cur_bank_idx++;
            next_free_slot_idx = 0;
            if (cur_bank_idx >= banks.size()) {
                allocateNewBank();    
            }
        }
    }
    if (result == NULL) {
        throw scheme_exception("Out of heap space");    
    }
    result->metadata = uint32_t(metadata);
    result->set_immutable(false);
    free_slots--;
    allocated++;
    return result;
}

void Heap::garbageCollect(vector<SchemeObject*> &stack) {
    //cout << "BEFORE: Size of heap: " << slots_num << " free: " << free_slots << " (" << banks.size() << ")" << endl;
    //cout << "BEFORE: Size of roots: " << roots.size() << endl;
    mark(stack);
    sweep();
    //cout << "AFTER: Size of heap: " << slots_num << " free: " << free_slots << endl;
    //cout << "AFTER: Size of roots: " << roots.size() << endl << endl;
    allocated = 0;
}

void Heap::mark(vector<SchemeObject*> &stack) {
    for(vector<SchemeObject*>::iterator i = stack.begin(); i != stack.end(); i++) {
        assert(*i != NULL);
        (*i)->mark();
    }   
    for(vector<SchemeObject*>::iterator i = roots.begin(); i != roots.end(); i++) {
        assert(*i != NULL);
       (*i)->mark();
    }   
}

void Heap::sweep() {
    vector<SchemeObject*>::iterator banks_iterator = banks.begin();                
    for(int i = 0; banks_iterator != banks.end(); i++, banks_iterator++) {
        SchemeObject* bank = *banks_iterator;
        uint blank_found = 0;    
        for(uint j = 0; j < slots_per_bank; j++) {
            SchemeObject* cur = &(bank[j]);
            if (cur->type() != SchemeObject::BLANK) {
                bool in_use = cur->inuse();
                cur->clear_inuse();
                if (!in_use && cur->type() != SchemeObject::SYMBOL) {
                    // Reclaim the slot    
                    cur->finalize();
                    cur->metadata = SchemeObject::BLANK;
                    free_slots++;
                    blank_found++;    
                }
            } else {
                blank_found++;    
            }
            if (blank_found == slots_per_bank) {
                // Bank is all blank and can be free'd
                banks.erase(banks_iterator);
                delete [] bank;
            } else {
                if (next_free_slot_idx >= j && cur_bank_idx >= i) {
                    next_free_slot_idx = j;
                    cur_bank_idx = i;
                }    
            }
        }
    }
    // TODO: Set these to the first free slot while traversing the heap above
    // next_free_slot_idx = 0;
    // cur_bank_idx = 0;
}

void Heap::dumpStats() {
    cout << "Heap stats" << endl;        
}

