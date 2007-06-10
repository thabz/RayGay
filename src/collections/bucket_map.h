
#ifndef COLLECTIONS_BUCKET_MAP_H
#define COLLECTIONS_BUCKET_MAP_H

#include <string>

template <typename K, typename V> 
struct _bucket_map_node {
    std::pair<K,V> p;        
    _bucket_map_node<K,V>* next;               
};

template <typename K, typename V>  class bucket_map;

template <typename K, typename V> 
struct _bucket_map_iterator {
        
    typedef _bucket_map_iterator<K,V> _self_type;
    typedef pair<K,V>* _pointer;
    typedef pair<K,V>& _reference;
    
    _bucket_map_iterator(uint32_t cur_bucket, _bucket_map_node<K,V> *cur_node, bucket_map<K,V> *bmap) : cur_bucket(cur_bucket), cur_node(cur_node), bmap(bmap) {
    }
    
    _reference operator*() const {
        return cur_node->p;
    }
    
    _pointer operator->() const {
        return &(cur_node->p);
    }
    
    _self_type& operator++() {
        if (cur_bucket >= bmap->num_buckets) {
            return *this;        
        }    
        if (cur_node->next != NULL) {
            cur_node = cur_node->next;
        } else {
            while(cur_bucket < bmap->num_buckets && bmap->buckets[cur_bucket] != NULL) {
                cur_bucket++;
            }
            if (cur_bucket < bmap->num_buckets) {
                cur_node = bmap->buckets[cur_bucket];    
            }
        }   
        return *this;
    }
    
    _self_type operator++(int) {
        _self_type result = *this;
        (*this)++;
        return result;
    }
    
    bool operator==(const _self_type& x) const {
        return cur_bucket == x.cur_bucket && cur_node == x.cur_node;    
    }

    bool operator!=(const _self_type& x) const {
        return cur_bucket != x.cur_bucket || cur_node != x.cur_node;    
    }
    
    uint32_t cur_bucket;
    _bucket_map_node<K,V> *cur_node;
    bucket_map<K,V> *bmap;
};

template <typename K, typename V> 
class bucket_map 
{
    public:
        typedef uint32_t hash_type;
        typedef K key_type;
        typedef V data_type;
        typedef pair<K,V> value_type;
        typedef _bucket_map_iterator<K,V> iterator;
        typedef _bucket_map_node<K,V> node_type;
        typedef V& reference;

    	bucket_map(uint32_t num_buckets = 255);
    	
        ~bucket_map();
        
        iterator begin() {
            if (begin_bucket == num_buckets) {
                return end();   
            } else {
                return iterator(begin_bucket, buckets[begin_bucket], this);    
            }
        }
        
        iterator end() {
            return iterator(num_buckets, NULL, this);    
        }
                
    	int hash(const K &key) const;
    	
        pair<iterator,bool> insert(const value_type& v) {
            hash_type h = hash(v.first);
            node_type* node = buckets[h];
            node_type* prev = NULL;
            for(; node != NULL; node = node->next) {
                if (node->p.first == v.first) {
                    node->p.second = v.second;
                    return pair<iterator,bool>(iterator(h, node, this),false);
                }
                prev = node;
            }
            node_type* new_node = new node_type();
            new_node->next = NULL;
            new_node->p = v;
            if (prev != NULL) {
                // Append to bucket    
                prev->next = new_node;
            } else {
                // Start new bucket    
                buckets[h] = new_node;
                if (h < begin_bucket) begin_bucket = h;
            }
            return pair<iterator,bool>(iterator(h, new_node, this), true);
        }
    	
    	iterator find(const K &key) {
            return find(key, hash(key));
        }
    	
    	iterator find(const K &key, const hash_type &h) {
            node_type* node = buckets[h];
            for(; node != NULL; node = node->next) {
                if (node->p.first == key) {
                    return iterator(h, node, this);
                }
            }
            return end();
        }
    	
        reference operator[](const K& k) {
            hash_type h = hash(k);        
            iterator i = find(k,h);
            if (i != end()) {
                return i->second;
            } else {
                return insert(value_type(k, data_type())).first->second;    
            }
        }
    	
    	// Number of buckets
    	uint32_t num_buckets;
    	
    	// First non-empty bucket
        uint32_t begin_bucket;
    	
    	// Buckets
        node_type** buckets;
};

template <typename K, typename V> 
bucket_map<K,V>::bucket_map(uint32_t num_buckets) : num_buckets(num_buckets)
{
    if (num_buckets < 17) num_buckets = 17;
    buckets = new node_type*[num_buckets];
    for(uint i = 0; i < num_buckets; i++) {
        buckets[i] = NULL;
    }
    begin_bucket = num_buckets;
};

template <typename K, typename V> 
bucket_map<K,V>::~bucket_map() 
{
    for(uint i = 0; i < num_buckets; i++) {
        if (buckets[i] != NULL) {
            node_type* node = buckets[i]->next;
            while(node != NULL) {
                node_type* next = node->next;    
                delete node;
                node = next;
            }        
        }        
    }        
    delete [] buckets;        
}

template <typename K, typename V> 
int bucket_map<K,V>::hash(const K &key) const
{
    int h = int(key);
    h += ~(h << 15);
    h ^= (h >> 10);
    h += (h << 3);
    h ^= (h >> 6);
    h += ~(h << 11);
    h ^= (h >> 16);
    h %= num_buckets;
    return (h < 0) ? h * -1 : h;
};

#endif
