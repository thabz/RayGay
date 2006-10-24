
#ifndef TRACER_MMAP_STL_ALLOCATOR
#define TRACER_MMAP_STL_ALLOCATOR

extern "C"
{
#include <sys/types.h>
#include <sys/mman.h>
}

#include <limits>
#include <vector>
#include <iostream>
#include <iterator>

/**
 * An STL allocator that uses mmap'ed tmp-files.
 * 
 * @see http://www.ddj.com/dept/cpp/184403759 for an excellent description of the 
 * allocator interface and allocators in general.
 */
template <typename T>
class mmap_allocator : public std::allocator<T> {
  public:
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef T* pointer;
    typedef T const * const_pointer;
    typedef T& reference;
    typedef T const & const_reference;

    //rebind
    template <typename U>
    struct rebind {
      typedef mmap_allocator <U> other;
    };

    pointer address (reference value ) const {
      return &value;
    }

    const_pointer address (const_reference value) const {
      return &value;
    }

    mmap_allocator () throw () {}
    
    mmap_allocator ( mmap_allocator const  &) throw () {}
    
    template <typename U>
    mmap_allocator (mmap_allocator <U> const &) throw () {} 

    ~mmap_allocator() throw () {}

    size_t max_size () const throw() {
      return ::std::numeric_limits <size_type>::max() / sizeof(T);
    }
    
    pointer allocate (size_type size, void* hint = 0) {
            size_type byte_size = size * sizeof(T);
            FILE* file_handle = ::tmpfile();
            file = ::fileno(file_handle);
            ::lseek(file, byte_size-1, SEEK_SET);
            ::write(file, "", 1);
            pointer result = static_cast<pointer>(::mmap(NULL, byte_size, PROT_READ | PROT_WRITE, MAP_SHARED, file, 0));
            if (int(result) == -1) {
               result = NULL;     
            }
            return result;
    }

    void construct (pointer p, const_reference value) {
      new((void *)p) T(value);  //placement new
    }

    void destroy (pointer p) {
      p->~T();
    }

    void deallocate (pointer p, size_type num) {
      ::munmap((caddr_t) p, sizeof(T)*num);
      ::close(file);
    }
    
    private:
        int file;   
};

  // Againt the norm two mmap_allocator's aren't interchangeable.
  template <typename T1, typename T2>
  bool operator == (mmap_allocator <T1> const &, mmap_allocator <T2> const &) throw () {
    return false;
  }

  // Againt the norm two mmap_allocator's aren't interchangeable.
  template <typename T1, typename T2>
  bool operator != (mmap_allocator <T1> const &, mmap_allocator <T2> const &) throw () {
    return true;
  }

#endif