
/**
 * A map that stores keys and values in fast local fields until the size exceeds 3.
 * For greater sizes the operations are delegated to a stl::map.
 */
 
 template <typename K, typename V> 
 class flat3_map 
 {
     public:
     	flat3_map(uint32_t num_buckets = 255);
         ~bucket_map();
     	int hash(const K &key) const;
     	void insert(const K &key, const V &value);
     	V* find(const K &key) const;
     	V* find(const K &key, const hash_type &h) const;

     private:
        K key1, key2, key3;
        V value1, value2, value3;
        map<K,V> delegate;
        size_t size;
};

template <typename K, typename V> 
V* flat3_map<K,V>::find(const K &key, const hash_type &h) const
{
     if (size > 3) {
        delegate.find(key);
        // Extract pair
     }        
     switch(size) {
         case 3 :
             if (key == key3) return value3;
         case 2 :    
             if (key == key2) return value2;
         case 1 :    
             if (key == key1) return value1;
         default:
             return NULL;    
     }
}
