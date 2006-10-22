// Three different POSIX happy allocators for C++ STL objects and some 
// measurements.


extern "C"
{
#include <sys/types.h>
#include <sys/mman.h>
}

#include <limits>
#include <vector>
#include <iostream>
#include <iterator>

namespace leimyalloc {
  template <typename T>
  class mmap_allocator {
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

    size_type max_size () const throw() {
      return ::std::numeric_limits <size_type>::max() / sizeof(T);
    }
    
    pointer allocate (size_type num, void *  hint = 0) {
      pointer p = (pointer) ::mmap(hint, num, PROT_READ | PROT_WRITE, MAP_ANON, -1, 0);
      int * val = (int *)p;
      if(val && *val == -1)
	p = NULL;
      return p;
    }

    void construct (pointer p, const_reference value) {
      new((void *)p) T(value);  //placement new
    }

    void destroy (pointer p) {
      p->~T();
    }

    void deallocate (pointer p, size_type num) {
      ::munmap((caddr_t) p, num);
    }
    
  };

  template <typename T1, typename T2>
  bool operator == (mmap_allocator <T1> const &, mmap_allocator <T2> const &) throw () {
    return true;
  }

  template <typename T1, typename T2>
  bool operator != (mmap_allocator <T1> const &, mmap_allocator <T2> const &) throw () {
    return false;
  }

}

#include <cstdlib>
#include <ctime>


template <int low, int high>
struct RandomGen {
  RandomGen () { 
    srand(time(0)); 
  }
  int operator () () const { 
    return rand() % high + low; 
  }
};


// vector dumper

template <typename T, template <typename T> class alloc>
std::ostream& operator << (std::ostream & os, std::vector<T, alloc<T> > const & v)
{
  std::copy(v.begin(), v.end(), std::ostream_iterator<T> (os, " "));
  return os;
}

int main () 
{
  std::vector<int> v1(200);
  std::vector<int, leimyalloc::mmap_allocator<int> > v2(200);

  // fill v1
  std::generate(v1.begin(), v1.end(), RandomGen<0,200>());

  // fill v2
  std::generate(v2.begin(), v2.end(), RandomGen<0,200>());

  std::cout << v1 << std::endl;

  std::cout << v2 << std::endl;
}
    
 
