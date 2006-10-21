/*
 * This is a scratchpad for testing different C++ features...
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cassert>
#include <cstdio>
#include <fcntl.h>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <sys/mman.h>

#include "collections/lru_hash.h"

using namespace std;

void test_bool() {
    bool t1 = true;
    bool t2 = true;
    bool f1 = false;
    bool f2 = false;

    assert(t1 && t2 == true);
    assert(t1 && t2);
    assert(t1 || f1);
    assert(f1 || t1);
    assert(f1 || f2 == false);
}

void test_modulo() {
    assert((0 + 1) % 3 == 1);
    assert((1 + 1) % 3 == 2);
    assert((2 + 1) % 3 == 0);

    assert((0 + 3 - 1) % 3 == 2);
    assert((1 + 3 - 1) % 3 == 0);
    assert((2 + 3 - 1) % 3 == 1);
}


void test_lowercase() {
    string s = "Ray Gay";
    transform(s.begin(),s.end(),s.begin(),(int(*)(int)) tolower);
    assert(s == "ray gay");
}

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

void test_sort_array() {
    int tal[] = { 10, 5, 15, 0 ,20 };
    sort(tal,tal + 5);
    assert(tal[0] == 0);
    assert(tal[1] == 5);
    assert(tal[2] == 10);
}

void test_sort() {
    // Test descending sortering
    vector<int> tal;
    tal.push_back(10);
    tal.push_back(5);
    tal.push_back(15);
    tal.push_back(0);
    tal.push_back(20);
    sort(tal.begin(),tal.end(),compareAreaDesc());
    assert(tal[0] == 20);
    assert(tal[1] == 15);
    assert(tal[2] == 10);
    assert(tal[3] == 5);
    assert(tal[4] == 0);
    tal.clear();
    
    // Test ascending sortering
    tal.push_back(10);
    tal.push_back(5);
    tal.push_back(15);
    tal.push_back(0);
    tal.push_back(20);
    sort(tal.begin(),tal.end(),compareAreaAsc());
    assert(tal[0] == 0);
    assert(tal[1] == 5);
    assert(tal[2] == 10);
    assert(tal[3] == 15);
    assert(tal[4] == 20);

    // Test pointer sagert
    sort((&tal)->begin(),(&tal)->end(),compareAreaAsc());

    // Test sortering af tom vector
    vector<int> tom;
    sort(tom.begin(),tom.end(),compareAreaAsc());
    assert(tom.empty());
    sort((&tom)->begin(),(&tom)->end(),compareAreaAsc());

    tom.push_back(1);
    sort(tom.begin(),tom.end(),compareAreaAsc());
    assert(tom.size() == 1);
}

void test_vector_copy() {
    vector<int>* a = new vector<int>;
    vector<int>* b = new vector<int>;
    a->push_back(1);
    a->push_back(2);
    a->push_back(3);
    (*b) = (*a);
    (*a)[0] = 10;
    assert((*b)[0] == 1);
    a->push_back(4);
    assert(a->size() == 4);
    assert(b->size() == 3);
    delete a;
    delete b;
}

void test_fmod() {
//    cout << "fmod(1.2, 1) : " << fmod(1.2,1) << endl;
//    cout << "fmod(2.2, 1) : " << fmod(2.2,1) << endl;
//    cout << "fmod(3.0, 1) : " << fmod(3.0,1) << endl;
//    cout << "fmod(-1.2, 1) : " << fmod(-1.2,1) << endl;
}

void test_vector_ref() {
    vector<int>* a = new vector<int>;
    a->push_back(1);
    a->push_back(2);
    a->push_back(3);
    vector<int>& b = *a;
    assert(b[0] == 1);
    b.push_back(4);
    assert(a->size() == 4);
    (*a)[0] = 10;
    assert(b[0] == 10);
    b.clear();
    assert(a->size() == 0);
}

void test_vector_clear() {
    vector<int>* a = new vector<int>;
    for(int i = 0; i < 10000; i++) {
	a->push_back(i);
    }
    assert(a->size() == 10000);
    a->clear();
    assert(a->size() == 0);
    //cout << "capacity: " << a->capacity() << endl;
    a->reserve(0);
    //cout << "capacity: " << a->capacity() << endl;
}

void test_lru_hash() {
    LRUHash<int,int> lru = LRUHash<int,int>(3);
    lru.insert(1,101);
    lru.insert(2,102);
    lru.insert(3,103);
    assert(*(lru.find(1)) == 101);
    assert(*(lru.find(2)) == 102);
    assert(*(lru.find(3)) == 103);
    lru.insert(1,105);
    assert(*(lru.find(1)) == 105);
    assert(*(lru.find(2)) == 102);
    assert(*(lru.find(3)) == 103);
    lru.insert(4,106);
    lru.insert(5,107);
    assert(lru.find(1) == NULL);
    assert(lru.find(2) == NULL);
    assert(*(lru.find(3)) == 103);
    assert(*(lru.find(4)) == 106);
    assert(*(lru.find(5)) == 107);
}

void test_shift() {
    uint a = 50;
    uint b;

    b = a << 2 | 3;

    assert(b >> 2 == 50);
    assert((b & 3) == 3);
    
    a = 20;
    b = (a << 2) | 1;

    assert(b >> 2 == 20);
    assert((b & 3) == 0x1);
}

void test_mmap() {
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
        assert(data2[i] == uint8_t(i & 0xff));    
    }
    munmap(data2,len);
    close(file2);
}

int main(int argc, char *argv[]) {

    test_bool();
    test_modulo();
    test_lowercase();
    test_sort();
    test_sort_array();
    test_vector_copy();
    test_vector_ref();
    test_fmod();
    test_vector_clear();
    test_shift();
    test_lru_hash();
    test_mmap();
    return EXIT_SUCCESS;
}


