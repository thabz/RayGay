
#ifndef SCHEME_HEAP
#define SCHEME_HEAP

#include "objects.h"
#include <list>
#include <vector>
#include <pthread.h>

using namespace std;

// TODO: http://en.wikipedia.org/wiki/Cheney's_algorithm is the simplest copying garbagecollector.

// Terminology: the heap is conceptually a number of pages each 
// with HEAP_PAGE_SIZE number of slots. Each slot fits one object.

#define HEAP_PAGE_SIZE 10000
#define SIZE_OF_LOCAL_SLOTS 100

class Heap {
    public:
        static Heap* getUniqueInstance();
        void addRoot(SchemeObject* root);
        void popRoot();
        SchemeObject* allocate(SchemeObject::ObjectType type);
        void garbageCollect(vector<SchemeObject*> &stack);
        bool timeToGarbageCollect();
        void dumpStats();
        ~Heap();
    private:
        Heap(uint32_t size);
        void mark(vector<SchemeObject*> &stack);
        void sweep();
        
        void reserve(SchemeObject** result, uint32_t num);
        void allocateNewPage();
        
        vector<SchemeObject*> roots;
        
        static Heap* unique_instance;
        uint32_t free_slots;
        uint32_t slots_num;
        uint32_t page_size;
        vector<SchemeObject*> banks;
        uint32_t cur_bank_idx;
        uint32_t next_free_slot_idx;
        uint32_t allocated;
        
    private:  /* Stats stuff */
        uint64_t alloced_types[SchemeObject::ALL_TYPE_ARE_BEFORE_HERE];
        int pages_created;        
        int pages_freed;
        int gc_runs;


    private: /* Thread local stuff */
        struct ThreadLocalCache {
            ThreadLocalCache() {
                index = SIZE_OF_LOCAL_SLOTS;    
            };        
            SchemeObject* bank[SIZE_OF_LOCAL_SLOTS];
            uint32_t index;
        };

        pthread_key_t local_bank_key;
	    pthread_mutex_t mutex_reserve;
};

inline
Heap* Heap::getUniqueInstance() {
    if (unique_instance == NULL) {
	    unique_instance = new Heap(HEAP_PAGE_SIZE);
    }
    return unique_instance;
}

inline
bool Heap::timeToGarbageCollect() {
    return free_slots < HEAP_PAGE_SIZE / 10 && allocated >= (9 * HEAP_PAGE_SIZE) / 10;
    //return allocated >= (9 * HEAP_PAGE_SIZE) / 10;
    //return cur_bank_idx == banks.size()-1 && next_free_slot_idx > int(0.9 * HEAP_PAGE_SIZE);
    //return allocated > 5;
}

#endif

