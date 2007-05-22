
#ifndef COLLECTIONS_BUCKET_MAP_H
#define COLLECTIONS_BUCKET_MAP_H

template <typename K, typename V> 
class bucket_map 
{
    public:
        typedef int hash_type;
            
    	bucket_map(uint32_t num_buckets = 255);
        ~bucket_map();
    	int hash(const K &key) const;
    	void insert(const K &key, const V &value);
    	V* find(const K &key) const;
    	V* find(const K &key, const hash_type &h) const;
    
    private:
    	struct bucket_map_node {
            pair<K,V> p;        
            bucket_map_node* next;               
    	};
    	
    	// Number of buckets
    	uint32_t num_buckets;
    	
    	// Buckets
        bucket_map_node** buckets;
        vector<bool> empty;
};

template <typename K, typename V> 
bucket_map<K,V>::bucket_map(uint32_t num_buckets) : num_buckets(num_buckets)
{
    if (num_buckets < 17) num_buckets = 17;
    buckets = new bucket_map_node*[num_buckets];
    for(uint i = 0; i < num_buckets; i++) {
        buckets[i] = NULL;
    }
};

template <typename K, typename V> 
bucket_map<K,V>::~bucket_map() 
{
    for(uint i = 0; i < num_buckets; i++) {
        if (buckets[i] != NULL) {
            bucket_map_node* node = buckets[i]->next;
            while(node != NULL) {
                bucket_map_node* next = node->next;    
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

template <typename K, typename V> 
V* bucket_map<K,V>::find(const K &key) const
{
    return find(key, hash(key));
};

template <typename K, typename V> 
V* bucket_map<K,V>::find(const K &key, const hash_type& h) const
{
    bucket_map_node* node = buckets[h];
    if (node == NULL) {
        return NULL;    
    } else {
        for(; node != NULL; node = node->next) {
            if (node->p.first == key) {
                return &node->p.second;
            }
        }
        return NULL;
    }
};

template <typename K, typename V> 
void bucket_map<K,V>::insert(const K& key, const V& value) 
{
    hash_type h = hash(key);
    bucket_map_node* node = buckets[h];
    if (node == NULL) {
        bucket_map_node* new_node = new bucket_map_node();
        new_node->p.first = key;
        new_node->p.second = value;
        new_node->next = NULL;
        buckets[h] = new_node;
    } else {
        bucket_map_node* n;    
        for(; node != NULL; node = node->next) {
            n = node;
            if (node->p.first == key) {
                node->p.second = value;    
            }
        }
        bucket_map_node* new_node = new bucket_map_node();
        new_node->p.first = key;
        new_node->p.second = value;
        new_node->next = NULL;
        n->next = new_node;
    }
};

#endif