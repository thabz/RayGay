
#ifndef COLLECTIONS_LRU_HASH_H
#define COLLECTIONS_LRU_HASH_H

#include <list>
#include <map>

/**
 * A least-recently-used map
 */
template <typename key_type, typename value_type> 
class LRUHash 
{
    public:
	insert(const key_type& key, const value_type& value);
	value_type* find(const key_type& key);

    private:
	// The values that we really store includes a pointer into the lru_list
	class internal_value {
	    value_type* value;
	    list<key_type>::iterator list_iter;
	};
	// Max number of keys in the hash at any given time
	uint32_t max_size;
	// A linked list of last recently used keys
	list<key_type> lru_list;
	// The wrapped hashmap
	map<key_type, internal_value> table;

};

template <typename key_type, typename value_type> 
LRUHash<key_type,value_type>::LRUHash(uint32_t max_size) : max_size(max_size)
{
};

template <typename key_type, typename value_type> 
value_type* LRUHash<key_type,value_type>::find(const key_type& key)
{
    map<key_type, internal_value>::iterator map_iter;
    map_iter = table.find(key);
    if (map_iter == table.end()) {
	// Key not found
	return NULL;
    } else {
	// Make key most recently used
	list<key_type>::iterator list_iter = map_iter->second.list_iter;
	lru_list.splice(lru_list.begin(), lru_list, li);
	// Return value
	return &(map_iter->second.value);
    }
};


template <typename key_type, typename value_type> 
void LRUHash<key_type,value_type>::insert(const key_type& key, const value_type& value)
{
    value_type* val = find(key);

    if (val != NULL) {
	// Simple update the already hashed value
	*val = value;
    } else {
	// Insert new key/value
	lru_list.push_front(key);
	internal_value iv;
	iv.value = value;
	iv.list_iter = lru_list.begin();
	table.insert(make_pair(key,iv));
	if (lru_list.size() > max_size) {
	    // Remove least recently used key/value
	    table.erase(lru_list.back());
	    lru_list.pop_back();
	}
    }
};


#endif
