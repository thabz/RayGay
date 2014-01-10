
#ifndef COLLECTIONS_BUCKET_MAP_H
#define COLLECTIONS_BUCKET_MAP_H

#include <stdint.h>
#include <utility>   // For std::pair<A,B>

template <typename K, typename V> 
struct _bucket_map_node {
    std::pair<K,V> p;        
    _bucket_map_node<K,V>* next;
    bool empty;               
};

template <typename K, typename V>  struct bucket_map;

template <typename K, typename V> 
struct _bucket_map_iterator {
        
    typedef _bucket_map_iterator<K,V> _self_type;
    typedef std::pair<K,V>* _pointer;
    typedef std::pair<K,V>& _reference;
    
    _bucket_map_iterator(uint32_t cur_bucket, _bucket_map_node<K,V> *cur_node, bucket_map<K,V> *bmap) : cur_bucket(cur_bucket), cur_node(cur_node), bmap(bmap) {
    }

    _bucket_map_iterator(const _self_type& o) : cur_bucket(o.cur_bucket), cur_node(o.cur_node), bmap(o.bmap) {
    }
    
    _bucket_map_iterator() { 
    }

    
    _reference operator*() const {
        return cur_node->p;
    }
    
    _pointer operator->() const {
        return &(cur_node->p);
    }
    
    // Prefix ++
    _self_type& operator++() {
        if (cur_bucket >= bmap->num_buckets) {
            return *this;        
        }    
        if (cur_node->next != NULL) {
            cur_node = cur_node->next;
        } else {
            cur_bucket++;
            while(cur_bucket < bmap->num_buckets && bmap->buckets[cur_bucket].empty == true) {
                cur_bucket++;
            }
            if (cur_bucket < bmap->num_buckets) {
                cur_node = &(bmap->buckets[cur_bucket]);
            } else {
                cur_node = NULL;
            }
        }   
        return *this;
    }
    
    // Postfix ++
    _self_type operator++(int) {
        _self_type result = *this;
        ++(*this);
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
struct bucket_map 
{
    public:
        typedef uint32_t hash_type;
        typedef K key_type;
        typedef V data_type;
        typedef std::pair<K,V> value_type;
        typedef _bucket_map_iterator<K,V> iterator;
        typedef _bucket_map_node<K,V> node_type;
        typedef V& reference;
        typedef size_t size_type;
        friend struct _bucket_map_iterator<K,V>;
        
        // n_b must be a power of two. No checking is done. TODO: Is there a fast way to check that?
    	bucket_map(uint32_t n_b = 256) : num_buckets(n_b) {
            assert(num_buckets > 0);
            buckets = new node_type[num_buckets];
            for(uint32_t i = 0; i < num_buckets; i++) {
                buckets[i].empty = true;
            }
            begin_bucket = num_buckets;
            map_size = 0;
            end_iterator = iterator(num_buckets, NULL, this);  	        
    	}
    	
        ~bucket_map() {
            for(uint32_t i = begin_bucket; i < num_buckets; i++) {
                if (!buckets[i].empty) {
                    node_type* node = buckets[i].next;
                    while(node != NULL) {
                        node_type* next = node->next;    
                        delete node;
                        node = next;
                    }        
                }        
            }        
            delete [] buckets;        
        }
        
        iterator begin() {
            if (begin_bucket == num_buckets) {
                return end();    
            } else {
                return iterator(begin_bucket, &buckets[begin_bucket], this);    
            }
        }
        
        iterator& end() {
            return end_iterator;
        }
                
        std::pair<iterator,bool> insert(const value_type& v, hash_type h) {
            h = h & (num_buckets-1);
            node_type* node = &buckets[h];
            node_type* bucket_end = NULL;
            // Try to modify value if key already registered
            for(; node != NULL && !node->empty; node = node->next) {
                if (node->p.first == v.first) {
                    node->p.second = v.second;
                    return std::pair<iterator,bool>(iterator(h, node, this),false);
                }
                bucket_end = node;
            }
            
            // No luck, create new node
            map_size++;
            node_type* new_node;
            if (bucket_end == NULL) {
                // Start new bucket    
                new_node = &buckets[h];
                if (h < begin_bucket) begin_bucket = h;
            } else {
                // Append to bucket    
                new_node = new node_type();
                bucket_end->next = new_node;
            }
            new_node->next = NULL;
            new_node->p = v;
            new_node->empty = false;

            return std::pair<iterator,bool>(iterator(h, new_node, this), true);
        }

        std::pair<iterator,bool> insert(const value_type& v) {
            return insert(v, hash(v.first));        
        }
            	
    	iterator find(const K &key) {
            hash_type h = hash(key);     
            return find(key, h);
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
    	
    	size_type size() const {
            return map_size;        
    	}
    	
    	bool empty() const {
            return map_size == 0;        
    	}

    	iterator find(const K &key, const hash_type &h) {
            hash_type hh = h & (num_buckets-1);
            node_type* node = &buckets[hh];
            for(; node != NULL && !node->empty; node = node->next) {
                if (node->p.first == key) {
                    return iterator(hh, node, this);
                }
            }
            return end();
        }
    	
    private:
     
    	// Number of buckets
    	const uint32_t num_buckets;
    	 	
    	// Buckets
        node_type* buckets;

        int hash(const K &key) const;
   
    	// Size of map
        size_t map_size;
    	
    	// First non-empty bucket
        hash_type begin_bucket;
        
        // End iterator
        iterator end_iterator;
};

template <typename K, typename V> 
int bucket_map<K,V>::hash(const K &key) const {
    int h = int(key);
#if 0
        h += ~(h << 15);
        h ^= (h >> 10);
        h += (h << 3);
        h ^= (h >> 6);
        h += ~(h << 11);
        h ^= (h >> 16);
#else        
        h ^= h << 3;
        h += h >> 5;
        h ^= h << 4;
        h += h >> 17;
        h ^= h << 25;
        h += h >> 6;
#endif        
    return (h < 0) ? h * -1 : h;
};

#endif
