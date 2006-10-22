
#ifndef TRACER_ALLOCATOR_H
#define TRACER_ALLOCATOR_H

#include <cctype>
#include <map>

class Allocator {
    public:
        enum {
            MMAP_ONLY,
            MALLOC_ONLY,
            AUTO            
        };        
        
        static void* allocate(size_t size, uint32_t flags);
        static void* safe_allocate(size_t size, uint32_t flags);
        static void free(void* ptr);
        
     private:        
        static void* allocate_mmap(size_t);
        static void* allocate_malloc(size_t);
        static void free_mmap(void* ptr);
        static void free_malloc(void* ptr);
        static std::map<void*,uint32_t> allocation_types;
        static std::map<void*,uint32_t> allocation_sizes;
};

#endif
