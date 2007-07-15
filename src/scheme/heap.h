
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
        void dumpStats();
        
    private:
        Heap(uint32_t size);
        void mark(vector<SchemeObject*> &stack);
        void sweep();
        
        void allocateNewBank();
        
        vector<SchemeObject*> roots;
        
        static Heap* unique_instance;
        uint32_t free_slots;
        uint32_t slots_num;
        uint32_t slots_per_bank;
        vector<SchemeObject*> banks;
        uint32_t cur_bank_idx;
        uint32_t next_free_slot_idx;
        uint32_t allocated;
};

inline
Heap* Heap::getUniqueInstance() {
    if (unique_instance == NULL) {
	unique_instance = new Heap(SLOTS_NUM);
    }
    return unique_instance;
}

inline
bool Heap::timeToGarbageCollect() {
    //return allocated >= SLOTS_NUM;
    return cur_bank_idx == banks.size()-1 && next_free_slot_idx > int(0.9 * SLOTS_NUM);
    //return allocated > 5;
}

#endif

