
#ifndef COLLECTIONS_BUCKET_MAP_H
#define COLLECTIONS_BUCKET_MAP_H

#include <utility>   // For std::pair<A,B>

template <typename K, typename V> 
struct _bucket_map_node {
    std::pair<K,V> p;        
    _bucket_map_node<K,V>* next;               
};

template <typename K, typename V>  class bucket_map;

template <typename K, typename V> 
struct _bucket_map_iterator {
        
    typedef _bucket_map_iterator<K,V> _self_type;
    typedef std::pair<K,V>* _pointer;
    typedef std::pair<K,V>& _reference;
    
    _bucket_map_iterator(uint32_t cur_bucket, _bucket_map_node<K,V> *cur_node, bucket_map<K,V> *bmap) : cur_bucket(cur_bucket), cur_node(cur_node), bmap(bmap) {
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
            while(cur_bucket < bmap->num_buckets && bmap->buckets[cur_bucket] == NULL) {
                cur_bucket++;
            }
            if (cur_bucket < bmap->num_buckets) {
                cur_node = bmap->buckets[cur_bucket];
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
class bucket_map 
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
        friend class _bucket_map_iterator<K,V>;

    	bucket_map(uint32_t num_buckets = 255) : num_buckets(num_buckets) {
            if (num_buckets < 17) num_buckets = 17;
            buckets = new node_type*[num_buckets];
            for(uint i = 0; i < num_buckets; i++) {
                buckets[i] = NULL;
            }
            begin_bucket = num_buckets;
            map_size = 0;  	        
    	}
    	
        ~bucket_map() {
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
                
        std::pair<iterator,bool> insert(const value_type& v) {
            hash_type h = hash(v.first);
            node_type* node = buckets[h];
            node_type* prev = NULL;
            for(; node != NULL; node = node->next) {
                if (node->p.first == v.first) {
                    node->p.second = v.second;
                    return std::pair<iterator,bool>(iterator(h, node, this),false);
                }
                prev = node;
            }
            map_size++;
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
            return std::pair<iterator,bool>(iterator(h, new_node, this), true);
        }
    	
    	iterator find(const K &key) {
            return find(key, hash(key));
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

    private:
     
    	// Number of buckets
    	uint32_t num_buckets;
    	 	
    	// Buckets
        node_type** buckets;

    	iterator find(const K &key, const hash_type &h) {
            node_type* node = buckets[h];
            for(; node != NULL; node = node->next) {
                if (node->p.first == key) {
                    return iterator(h, node, this);
                }
            }
            return end();
        }
    	
    	int hash(const K &key) const {
            int h = int(key);
            h += ~(h << 15);
            h ^= (h >> 10);
            h += (h << 3);
            h ^= (h >> 6);
            h += ~(h << 11);
            h ^= (h >> 16);
            h %= num_buckets;
            return (h < 0) ? h * -1 : h;
        }
   
    	// Size of map
        size_t map_size;
    	
    	// First non-empty bucket
        uint32_t begin_bucket;
};

#endif
