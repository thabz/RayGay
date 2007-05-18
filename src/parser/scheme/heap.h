
#ifndef SCHEME_HEAP
#define SCHEME_HEAP

#include "objects.h"
#include <list>
#include <vector>

using namespace std;

#define SLOTS_NUM 10000

class Heap {
    public:
        static Heap* getUniqueInstance();
        void addRoot(SchemeObject* root);
        void popRoot();
        SchemeObject* allocate(SchemeObject::ObjectType type);
        void garbageCollect(vector<SchemeObject*> &stack);
        bool timeToGarbageCollect();
        
    private:
        Heap(uint32_t size);
        void mark(vector<SchemeObject*> &stack);
        void sweep();
        
        list<SchemeObject*> roots;
        
        static Heap* unique_instance;
        int free_slots;
        uint32_t slots_num;
        SchemeObject* next_free;
        SchemeObject* allocations;
};

inline
Heap* Heap::getUniqueInstance() {
    if (unique_instance == NULL) {
	    unique_instance = new Heap(SLOTS_NUM);
    }
    return unique_instance;
}

#endif

