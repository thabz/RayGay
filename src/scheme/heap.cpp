
#include "heap.h"
#include "scheme.h"
#include <iostream>
#include <iomanip>

Heap* Heap::unique_instance = NULL;

Heap::Heap(uint32_t slots_per_bank) {
    this->slots_per_bank = slots_per_bank;
    this->free_slots = 0;
    this->slots_num = 0;
    this->allocated = 0;
    
    for(int i = 0; i < SchemeObject::ALL_TYPE_ARE_BEFORE_HERE; i++) {
        alloced_types[i] = 0;    
    }
    banks_created = 0;
    banks_freed = 0;
    gc_runs = 0;

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
    for(uint32_t i = 0; i < slots_per_bank; i++) {
        bank[i].metadata = SchemeObject::BLANK;
    }
    next_free_slot_idx = 0;
    cur_bank_idx = banks.size() - 1;
    free_slots += slots_per_bank;
    slots_num += slots_per_bank;
    banks_created++;
}

SchemeObject* Heap::allocate(SchemeObject::ObjectType metadata) {
    SchemeObject* result = NULL;
    while (true) {
	// Scanning through current bank looking for an empty slot
        SchemeObject* cur_bank = banks[cur_bank_idx];    
	SchemeObject* p = &(cur_bank[next_free_slot_idx]);
        while (next_free_slot_idx < slots_per_bank) {
            if (p->type() == SchemeObject::BLANK) {
                result = p;
		goto found_one;
            }
	    p++;
            next_free_slot_idx++;
        }

	// Didn't find any free slot in current bank. 
	// Move to the next bank. Create new bank if  
	// we're at the last.
        cur_bank_idx++;
        next_free_slot_idx = 0;
        if (cur_bank_idx >= banks.size()) {
            allocateNewBank();    
        }
    }
found_one:
    result->metadata = uint32_t(metadata);
    result->set_immutable(false);
    free_slots--;
    allocated++;
    alloced_types[metadata]++;
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
    gc_runs++;
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
    for(uint32_t i = 0; banks_iterator != banks.end(); i++, banks_iterator++) {
        SchemeObject* bank = *banks_iterator;
        uint32_t blank_found = 0;    
        for(uint32_t j = 0; j < slots_per_bank; j++) {
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
                banks_freed++;
                free_slots -= slots_per_bank;
            } else {
                if (next_free_slot_idx >= j && cur_bank_idx >= i) {
                    next_free_slot_idx = j;
                    cur_bank_idx = i;
                }    
            }
        }
    }
    // next_free_slot_idx = 0;
    // cur_bank_idx = 0;
}

void Heap::dumpStats() {
    cout << "Heap allocations" << endl;
    long total_count = 0;        
    for(int i = 0; i < SchemeObject::ALL_TYPE_ARE_BEFORE_HERE; i++) {
        long count = alloced_types[i];
        if (count > 0) {
            total_count += count;
            string type_name = SchemeObject::toString((SchemeObject::ObjectType)i);        
            cout << "    " << left << setw(20) << (type_name + "s") << ": " << count << endl;
        }    
    }
    cout << "    " << left << setw(20) << "Total" << ": " << total_count << endl;
    cout << "Heap banks" << endl;
    cout << "    Created             : " << banks_created << endl;
    cout << "    Freed               : " << banks_freed << endl;
    cout << "    Objects per bank    : " << SLOTS_NUM << endl;
    cout << "Garbage collection" << endl;
    cout << "    Mark and sweep runs : " << gc_runs << endl;
    
    // Dump symbols hash distributions
    cout << "Symbols hash distribution" << endl;
    uint32_t bits = 8;
    int bit_cor[bits]; // Bit correlations
    vector<SchemeObject*>::iterator banks_iterator = banks.begin();
    int hashes = 0;                

    for(uint32_t i = 0; i < bits; i++) {
        bit_cor[i] = 0;    
    }
    for(uint32_t i = 0; banks_iterator != banks.end(); i++, banks_iterator++) {
        SchemeObject* bank = *banks_iterator;
        for(uint32_t j = 0; j < slots_per_bank; j++) {
            SchemeObject* cur = &(bank[j]);
            if (cur->type() == SchemeObject::SYMBOL) {
                hashes++;
                uint32_t h = cur->hash % (1 << bits);
                for(uint32_t b = 0; b < bits; b++) {
                    if (h & (1 << b)) bit_cor[b]++;        
                }
            }
        }
    }
    cout << "    Bit correlations    : ";
    for(uint32_t i = 0; i < bits; i++) {
        cout << setprecision(3) << fixed << (double(bit_cor[i]) / double(hashes)) << " ";    
    }
    cout << endl;
}

