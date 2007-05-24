/*
 * This is a scratchpad for testing different C++ features...
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern "C" {
#include <sys/mman.h>
#include <fcntl.h>
}

#include <cstdlib>
#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

#include "collections/lru_hash.h"
#include "collections/bucket_map.h"
#include "collections/caseinsensitive_map.h"
#include "collections/flat3_map.h"

#include "mmap_allocator.h"
#include "math/vector.h"

#include "testing.h"

using namespace std;

class test_bool : public Test {
        public:
        void run() {
                bool t1 = true;
                bool t2 = true;
                bool f1 = false;
                bool f2 = false;

                assertTrue(t1 && t2 == true);
                assertTrue(t1 && t2);
                assertTrue(t1 || f1);
                assertTrue(f1 || t1);
                assertTrue(f1 || f2 == false);
        }
};


class test_modulo : public Test {
        public:
        void run() {
    assertTrue((0 + 1) % 3 == 1);
    assertTrue((1 + 1) % 3 == 2);
    assertTrue((2 + 1) % 3 == 0);

    assertTrue((0 + 3 - 1) % 3 == 2);
    assertTrue((1 + 3 - 1) % 3 == 0);
    assertTrue((2 + 3 - 1) % 3 == 1);
}
};


class test_lowercase : public Test {
        public:
        void run() {
    string s = "Ray Gay";
    transform(s.begin(),s.end(),s.begin(),(int(*)(int)) tolower);
    assertTrue(s == "ray gay");
}
};


class compareAreaDesc {
    public:
	bool operator()(int const p1, int const p2)
	{
	    return p1 > p2;
	}
};

class compareAreaAsc {
    public:
	bool operator()(int const p1, int const p2)
	{
	    return p1 < p2;
	}
};


class test_sort_array: public Test {
        public:
        void run() {
    int tal[] = { 10, 5, 15, 0 ,20 };
    sort(tal,tal + 5);
    assertTrue(tal[0] == 0);
    assertTrue(tal[1] == 5);
    assertTrue(tal[2] == 10);
}
};


class test_sort : public Test {
        public:
        void run() {

    // Test descending sortering
    vector<int> tal;
    tal.push_back(10);
    tal.push_back(5);
    tal.push_back(15);
    tal.push_back(0);
    tal.push_back(20);
    sort(tal.begin(),tal.end(),compareAreaDesc());
    assertTrue(tal[0] == 20);
    assertTrue(tal[1] == 15);
    assertTrue(tal[2] == 10);
    assertTrue(tal[3] == 5);
    assertTrue(tal[4] == 0);
    tal.clear();
    
    // Test ascending sortering
    tal.push_back(10);
    tal.push_back(5);
    tal.push_back(15);
    tal.push_back(0);
    tal.push_back(20);
    sort(tal.begin(),tal.end(),compareAreaAsc());
    assertTrue(tal[0] == 0);
    assertTrue(tal[1] == 5);
    assertTrue(tal[2] == 10);
    assertTrue(tal[3] == 15);
    assertTrue(tal[4] == 20);

    // Test pointer sagert
    sort((&tal)->begin(),(&tal)->end(),compareAreaAsc());

    // Test sortering af tom vector
    vector<int> tom;
    sort(tom.begin(),tom.end(),compareAreaAsc());
    assertTrue(tom.empty());
    sort((&tom)->begin(),(&tom)->end(),compareAreaAsc());

    tom.push_back(1);
    sort(tom.begin(),tom.end(),compareAreaAsc());
    assertTrue(tom.size() == 1);
}
};


class test_vector_copy : public Test {
        public:
        void run() {
    vector<int>* a = new vector<int>;
    vector<int>* b = new vector<int>;
    a->push_back(1);
    a->push_back(2);
    a->push_back(3);
    (*b) = (*a);
    (*a)[0] = 10;
    assertTrue((*b)[0] == 1);
    a->push_back(4);
    assertTrue(a->size() == 4);
    assertTrue(b->size() == 3);
    delete a;
    delete b;
}
};

class test_vector_ref : public Test {
        public:
        void run() {
    vector<int>* a = new vector<int>;
    a->push_back(1);
    a->push_back(2);
    a->push_back(3);
    vector<int>& b = *a;
    assertTrue(b[0] == 1);
    b.push_back(4);
    assertTrue(a->size() == 4);
    (*a)[0] = 10;
    assertTrue(b[0] == 10);
    b.clear();
    assertTrue(a->size() == 0);
}
};

class test_vector_clear : public Test {
        public:
        void run() {
    vector<int>* a = new vector<int>;
    for(int i = 0; i < 10000; i++) {
	a->push_back(i);
    }
    assertTrue(a->size() == 10000);
    a->clear();
    assertTrue(a->size() == 0);
    //cout << "capacity: " << a->capacity() << endl;
    a->reserve(0);
    //cout << "capacity: " << a->capacity() << endl;
}
};

class test_vector_erase : public Test {
        public:
        void run() {
            vector<int> a;
            for(int i = 0; i < 100; i++) {
       	        a.push_back(i);
             }
             assertTrue(a.size() == 100);
             assertTrue(a[0] == 0);
             a.erase(a.begin());
             assertTrue(a.size() == 99);
             assertTrue(a[0] == 1);
             assertTrue(a[10] == 11);
             a.erase(a.begin()+10);
             assertTrue(a[10] == 12);
             assertTrue(a[0] == 1);
             assertTrue(a.size() == 98);
        }
};


class test_lru_hash : public Test {
        public:
        void run() {
    LRUHash<int,int> lru = LRUHash<int,int>(3);
    lru.insert(1,101);
    lru.insert(2,102);
    lru.insert(3,103);
    assertTrue(*(lru.find(1)) == 101);
    assertTrue(*(lru.find(2)) == 102);
    assertTrue(*(lru.find(3)) == 103);
    lru.insert(1,105);
    assertTrue(*(lru.find(1)) == 105);
    assertTrue(*(lru.find(2)) == 102);
    assertTrue(*(lru.find(3)) == 103);
    lru.insert(4,106);
    lru.insert(5,107);
    assertTrue(lru.find(1) == NULL);
    assertTrue(lru.find(2) == NULL);
    assertTrue(*(lru.find(3)) == 103);
    assertTrue(*(lru.find(4)) == 106);
    assertTrue(*(lru.find(5)) == 107);
}
};

class test_shift : public Test {
        public:
        void run() {

    uint a = 50;
    uint b;

    b = a << 2 | 3;

    assertTrue(b >> 2 == 50);
    assertTrue((b & 3) == 3);
    
    a = 20;
    b = (a << 2) | 1;

    assertTrue(b >> 2 == 20);
    assertTrue((b & 3) == 0x1);
}
};

class test_mmap : public Test {
        public:
        void run() {

    char* filename = "test.bin";        
    int file = open(filename, O_RDWR | O_CREAT | O_TRUNC, 00700);
    if (file == -1) {
        cout << "open failed" << endl;
        exit(EXIT_FAILURE);    
    }
    size_t len = 100000;
    lseek(file, len-1,SEEK_SET);
    write(file, "", 1);
    uint8_t* data = (uint8_t*) mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_SHARED, file, 0);
    if (data == NULL) {
        cout << "mmap failed" << endl;
        exit(EXIT_FAILURE);    
    }
    for(uint32_t i = 0; i < len; i++) {
        data[i] = uint8_t(i & 0xff);    
    }
    munmap(data, len);
    close(file);
    
    int file2 = open(filename, O_RDONLY, 0);
    if (file == -1) {
        cout << "open 2 failed" << endl;
        exit(EXIT_FAILURE);    
    }
    uint8_t* data2 = (uint8_t*) mmap(NULL, len, PROT_READ, MAP_SHARED, file2, 0);
    if (data == NULL) {
        cout << "mmap 2 failed" << endl;
        exit(EXIT_FAILURE);    
    }
    for(uint32_t i = 0; i < len; i++) {
        assertTrue(data2[i] == uint8_t(i & 0xff));    
    }
    munmap(data2,len);
    close(file2);
}
};

class test_stl_mmap_allocator : public Test {
        public:
        void run() {
    vector<Vector> v1;
    vector<Vector,mmap_allocator<Vector> > v2;
    
    for(uint32_t i = 0; i < 500; i++) {
        Vector v = Vector(i,i,i);   
        v1.push_back(v);
        v2.push_back(v);
    }        
    for(uint32_t i = 0; i < 500; i++) {
        assertTrue(v1[i] == v2[i]);    
    }
}
};


class test_swap : public Test {
   public:
       void run() {
           double a = 10.9, b = 2.1;
           swap(a,b);
           assertTrue(a == 2.1);
           assertTrue(b == 10.9);       }
};

class test_bucket_map : public Test {
   public:
       void run() {
           bucket_map<int,string> map;
           map.insert(10, "ten");
           map.insert(20, "twenty");
           map.insert(40, "fourty");
           map.insert(1000, "thousand");
           map.insert(256+10, "other");
           assertTrue(*map.find(10) == "ten");
           assertTrue(*map.find(20) == "twenty");
           assertTrue(*map.find(256+10) == "other");
           assertTrue(*map.find(1000) == "thousand");
           assertTrue(map.find(99) == NULL);
       }
};

class test_icase_map : public Test {
   public:
       void run() {
	   caseinsensitive_map<int> m;
	   m["AaaA"] = 10;
	   m["BBBB"] = 20;
	   assertTrue(m["aaaa"] == 10);
	   assertTrue(m["aAAa"] == 10);
	   assertTrue(m["bbbb"] == 20);
	   assertTrue(m["BbbB"] == 20);
       }
};

/*
class test_flat3_map : public Test {
   public:
       void run() {
	   flat3_map<char,double> m;
	   m.insert('A',10);
	   m.insert('B',20);
	   assertTrue(*map.find('A') == 10);
       }
};
*/

int main(int argc, char *argv[]) {
    TestSuite suite;
    suite.add("Bool",new test_bool());
    suite.add("Modulo",new test_modulo());
    suite.add("Lowercase",new test_lowercase());
    suite.add("Sort",new test_sort());
    suite.add("Sort array",new test_sort_array());
    suite.add("Vector copy",new test_vector_copy());
    suite.add("Vector ref",new test_vector_ref());
    suite.add("Vector clear",new test_vector_clear());
    suite.add("Vector erase",new test_vector_erase());
    suite.add("Shift",new test_shift());
    suite.add("LRU Hash",new test_lru_hash());
    suite.add("Bucket map",new test_bucket_map());
    suite.add("Caseinsensitive map",new test_icase_map());
    suite.add("Flat 3 map",new test_flat3_map());
    suite.add("mmap",new test_mmap());
    suite.add("STL mmap alloc",new test_stl_mmap_allocator());
    suite.add("Swap",new test_swap());
        
    suite.run();
    suite.printStatus();

    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}


