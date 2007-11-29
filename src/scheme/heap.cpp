
#include "heap.h"
#include "scheme.h"
#include <iostream>
#include <iomanip>

Heap* Heap::unique_instance = NULL;

Heap::Heap(uint32_t page_size) {
    this->page_size = page_size;
    this->free_slots = 0;
    this->slots_num = 0;
    this->allocated = 0;
    
    for(int i = 0; i < SchemeObject::ALL_TYPE_ARE_BEFORE_HERE; i++) {
        alloced_types[i] = 0;    
    }
    pages_created = 0;
    pages_freed = 0;
    gc_runs = 0;

    pthread_mutex_init(&mutex_reserve,NULL);
    pthread_key_create(&local_bank_key,NULL);	

    allocateNewPage();
}

Heap::~Heap() {
    // TODO: Dealloc all pages 
    // TODO: Dealloc all thread local banks         
}

void Heap::addRoot(SchemeObject* root) {
    roots.push_back(root);
}

void Heap::popRoot() {
    roots.pop_back();
}

void Heap::allocateNewPage() {
    SchemeObject* page = new SchemeObject[page_size];
    banks.push_back(page);
    for(uint32_t i = 0; i < page_size; i++) {
        page[i].metadata = SchemeObject::BLANK;
    }
    free_slots += page_size;
    slots_num += page_size;
    pages_created++;
}

SchemeObject* Heap::allocate(SchemeObject::ObjectType type) {
    assert(type < 256);        
    ThreadLocalCache* local = (ThreadLocalCache*) pthread_getspecific(local_bank_key);
    if (local == NULL) {
        local = new ThreadLocalCache();    
        pthread_setspecific(local_bank_key, local);
    }
    
    if (local->index >= SIZE_OF_LOCAL_SLOTS) {
	    reserve(local->bank, SIZE_OF_LOCAL_SLOTS);
	    local->index = 0;
    }
    SchemeObject* result = local->bank[local->index];
    local->index++;
    result->metadata = type;
    allocated++;
    alloced_types[type]++;
    return result;
}

void Heap::reserve(SchemeObject** result, uint32_t num) {
    pthread_mutex_lock(&mutex_reserve);
    
    // Create new page(s) if needed. 
    while (free_slots < num) {
        allocateNewPage();    
    }
    for(uint32_t i = 0; i < num; i++) {
        while (true) {
    	    // Scan through current page while 
       	    // looking for an empty slot
            SchemeObject* cur_bank = banks[cur_bank_idx];    
    	    SchemeObject* p = &(cur_bank[next_free_slot_idx]);
            while (next_free_slot_idx < page_size) {
                if (p->type() == SchemeObject::BLANK) {
                    *result = p;
    		    goto found_one;
                }
    	        p++;
                next_free_slot_idx++;
            }
    
            // Didn't find any free slot in current bank. 
            // Move to the next bank. 
            cur_bank_idx++;
            next_free_slot_idx = 0;
        }
    found_one:
        (*result)->metadata = SchemeObject::RESERVED;
        result++;
    }
    free_slots -= num;
    pthread_mutex_unlock(&mutex_reserve);
}

void Heap::garbageCollect(vector<SchemeObject*> &stack) {
    pthread_mutex_lock(&mutex_reserve);
    //cout << "BEFORE: Size of heap: " << slots_num << " free: " << free_slots << " (" << banks.size() << ")" << endl;
    //cout << "BEFORE: Size of roots: " << roots.size() << endl;
    mark(stack);
    sweep();
    //cout << "AFTER: Size of heap: " << slots_num << " free: " << free_slots << endl;
    //cout << "AFTER: Size of roots: " << roots.size() << endl << endl;
    allocated = 0;
    gc_runs++;
    pthread_mutex_unlock(&mutex_reserve);
}

void Heap::mark(vector<SchemeObject*> &stack) {
    /*        
    for(int i = 0; i < SIZE_OF_LOCAL_SLOTS; i++) {
	if (local_bank[i]->type() == SchemeObject::RESERVED) {
	    local_bank[i]->mark();
	}
    }
    */
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
        SchemeObject* cur = bank;
        uint32_t blank_found = 0; 
        bool reset = false;   
        for(uint32_t j = 0; j < page_size; j++, cur++) {
            if (cur->type() != SchemeObject::BLANK && cur->type() != SchemeObject::RESERVED) {
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
            if (!reset && next_free_slot_idx >= j && cur_bank_idx >= i) {
                next_free_slot_idx = j;
                cur_bank_idx = i;
                reset = true;
            }    
        }
        if (blank_found == page_size && i != cur_bank_idx) {
            // Page is all blank and can be free'd
            /*
            banks.erase(banks_iterator);
            delete [] bank;
            pages_freed++;
            free_slots -= page_size;
            */
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
            wstring type_name = SchemeObject::toString((SchemeObject::ObjectType)i);        
            wcout << "    " << left << setw(20) << (type_name + L"s") << ": " << count << endl;
        }    
    }
    cout << "    " << left << setw(20) << "Total" << ": " << total_count << endl;
    cout << "Heap pages" << endl;
    cout << "    Created             : " << pages_created << endl;
    cout << "    Freed               : " << pages_freed << endl;
    cout << "    Page size (objects) : " << HEAP_PAGE_SIZE << endl;
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
        for(uint32_t j = 0; j < page_size; j++) {
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

