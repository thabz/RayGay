
#ifndef COLLECTIONS_LRU_HASH_H
#define COLLECTIONS_LRU_HASH_H

#include <list>
#include <map>

using namespace std;

/**
 * A least-recently-used map
 *
 * TODO: The list<> is very slow. Use a fixed size vector<> with a circular
 * start-index instead. The template parameters K and V are the key- and
 * valuetype.
 */
template <typename K, typename V> class lru_hash {
public:
  lru_hash(uint32_t max_size);
  void insert(const K &key, const V &value);
  V *find(const K &key);

private:
  typedef typename list<K>::iterator list_iter_type;
  // The values that we really store includes a pointer into the lru_list
  struct internal_value {
    list_iter_type list_iter;
    V value;
  };
  typedef typename map<K, internal_value>::iterator map_iter_type;
  // Max number of keys in the hash at any given time
  uint32_t max_size;
  // A linked list of last recently used keys
  list<K> lru_list;
  // The wrapped hashmap
  map<K, internal_value> table;
};

template <typename K, typename V>
lru_hash<K, V>::lru_hash(uint32_t max_size) : max_size(max_size){};

template <typename K, typename V> V *lru_hash<K, V>::find(const K &key) {
  map_iter_type map_iter = table.find(key);
  if (map_iter == table.end()) {
    // Key not found
    return NULL;
  } else {
    // Make key most recently used
    list_iter_type list_iter = map_iter->second.list_iter;
    lru_list.splice(lru_list.begin(), lru_list, list_iter);
    // Return value
    return &(map_iter->second.value);
  }
};

template <typename K, typename V>
void lru_hash<K, V>::insert(const K &key, const V &value) {
  V *val = find(key);

  if (val != NULL) {
    // Simple update the already hashed value
    *val = value;
  } else {
    // Insert new key/value
    lru_list.push_front(key);
    internal_value iv;
    iv.value = value;
    iv.list_iter = lru_list.begin();
    table.insert(make_pair(key, iv));
    if (lru_list.size() > max_size) {
      // Remove least recently used key/value
      table.erase(lru_list.back());
      lru_list.pop_back();
    }
  }
};

#endif
