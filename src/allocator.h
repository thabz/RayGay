
#ifndef TRACER_ALLOCATOR_H
#define TRACER_ALLOCATOR_H

#include <cctype>
#include <map>
#include <vector>
#include "math/vector.h"

class Allocator {
    public:
        enum model_t {
            MMAP_ONLY,
            MALLOC_ONLY,
            AUTO            
        };
        
        static void* allocate(size_t size, model_t type);
        static void* safe_allocate(size_t size, model_t type);
        static void free(void* ptr);

     private:        
        static void* allocate_mmap(size_t);
        static void* allocate_malloc(size_t);
        static void free_mmap(void* ptr);
        static void free_malloc(void* ptr);
        static std::map<void*,model_t> allocation_types;
        static std::map<void*,size_t> allocation_sizes;
        static std::map<void*,int> allocation_files;
        
};

#endif
