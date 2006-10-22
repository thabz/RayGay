
#include "allocator.h"
#include "exception.h"
#include <sys/mman.h>
#include <cstdlib>
#include <cstdio>
#include <iostream>

using namespace std;

#define map_iter_type map<void*,Allocator::model_t>::iterator
#define map_iter_size map<void*,size_t>::iterator

std::map<void*,Allocator::model_t> Allocator::allocation_types;
std::map<void*,size_t> Allocator::allocation_sizes;
std::map<void*,int> Allocator::allocation_files;


void* Allocator::allocate(size_t size, model_t type) {
    void* result = NULL;
    switch(type) {
        case MMAP_ONLY:
            result = allocate_mmap(size);
            break;
        case MALLOC_ONLY:
            result = allocate_malloc(size);
            break;
        case AUTO:
            type = MALLOC_ONLY;
            result = allocate_malloc(size);
            if (result == NULL) {
                type = MMAP_ONLY;    
                result = allocate_mmap(size);            
            }
            break;
        default:
            throw_exception("Unknown allocation type");    
    }
    if (result != NULL) {
        allocation_types.insert(make_pair(result, type));
        allocation_sizes.insert(make_pair(result, size));
    }
    return result;
}

void* Allocator::safe_allocate(size_t size, model_t type) {
    void* result = allocate(size, type);
    if (result == NULL) {
        throw_exception("Allocation failed");    
    }
    return result;    
}

void* Allocator::allocate_mmap(size_t size) {
     FILE* file_handle = tmpfile();
     int file = fileno(file_handle);
     lseek(file, size-1, SEEK_SET);
     write(file, "", 1);
     void* result = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, file, 0);
     if (result != NULL) {
         allocation_files.insert(make_pair(result,file));
     }
     return result;
}

void* Allocator::allocate_malloc(size_t size) {
     return malloc(size);        
}

void Allocator::deallocate(void* ptr) {
     if (ptr == NULL) {
         throw_exception("Trying to free a NULL pointer");    
     }
     map_iter_type map_iter = allocation_types.find(ptr);
     if (map_iter == allocation_types.end()) {
         throw_exception("Trying to free non-allocated memory");     
     }
     model_t type = map_iter->second;
     switch (type) {
         case MMAP_ONLY:
             free_mmap(ptr);
             break;
         case MALLOC_ONLY:
             free_malloc(ptr);
             break;
         default:
             throw_exception("Trying to free unknown type");    
     }
     allocation_types.erase(ptr);
     allocation_sizes.erase(ptr);
}

void Allocator::free_malloc(void* ptr) {
     free(ptr);        
}

void Allocator::free_mmap(void* ptr) {
     map_iter_size map_iter = allocation_sizes.find(ptr);
     size_t size = map_iter->second;
     munmap(ptr, size);
     int file = allocation_files.find(ptr)->second;
     close(file);
     allocation_files.erase(ptr);         
}
