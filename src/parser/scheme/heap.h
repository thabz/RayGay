
#ifndef SCHEME_HEAP
#define SCHEME_HEAP

#include "objects.h"
#include <list>

using namespace std;

class Heap {
    public:
        static Heap* getUniqueInstance();
        void addRoot(SchemeObject* root);
        void popRoot();
        void addAllocation(SchemeObject* o);
        void garbageCollect(list<SchemeObject*> &stack);
        bool timeToGarbageCollect();
        
    private:
        Heap();
        void mark(list<SchemeObject*> &stack);
        void sweep();
        
        list<SchemeObject*> roots;
        list<SchemeObject*> allocations;
        
        static Heap* unique_instance;
        int counter;
};

inline
Heap* Heap::getUniqueInstance() {
    if (unique_instance == NULL) {
	    unique_instance = new Heap();
    }
    return unique_instance;
}

#endif

