
#ifndef COLLECTIONS_FLAT3_MAP
#define COLLECTIONS_FLAT3_MAP

/**
 * A map that stores keys and values in fast local fields until the size
 * exceeds 3. For greater sizes the operations are delegated to a stl::map.
 *
 * FIXME: This doesn't compile at the moment. No idea what's wrong.
 */

#include <map>

template <typename K, typename V> class flat3_map {
public:
  flat3_map();
  void insert(const K &key, const V &value);
  V *find(const K &key) const;
  size_t size() const;

private:
  typedef std::map<K, V> delegate_type;
  void convert();
  K key1;
  K key2;
  K key3;
  V value1;
  V value2;
  V value3;
  delegate_type delegate;
  delegate_type::const_iterator iter;
  size_t _size;
};

template <typename K, typename V> flat3_map<K, V>::flat3_map() { _size = 0; }

template <typename K, typename V> size_t flat3_map<K, V>::size() const {
  return _size > 3 ? delegate.size() : _size;
}

template <typename K, typename V> void flat3_map<K, V>::convert() {
  delegate[key1] = value1;
  delegate[key2] = value2;
  delegate[key3] = value3;
}

template <typename K, typename V> V *flat3_map<K, V>::find(const K &key) const {
  if (_size > 3) {
    iter = delegate.find(key);
    if (iter == delegate.end()) {
      return NULL;
    } else {
      return iter.second;
    }
  }
  switch (_size) {
  // Using fallthrough
  case 3:
    if (key == key3)
      return &value3;
  case 2:
    if (key == key2)
      return &value2;
  case 1:
    if (key == key1)
      return &value1;
  default:
    return NULL;
  }
}

template <typename K, typename V>
void flat3_map<K, V>::insert(const K &key, const V &value) {
  if (_size > 3) {
    delegate[key] = value;
    return;
  }
  // Try updating existing
  switch (_size) {
  case 3:
    if (key3 == key) {
      value3 = value;
      return;
    }
  case 2:
    if (key2 == key) {
      value2 = value;
      return;
    }
  case 1:
    if (key1 == key) {
      value1 = value;
      return;
    }
  }
  // Insert new
  switch (_size) {
  case 3:
    convert();
    delegate[key] = value;
    break;
  case 2:
    key3 = key;
    value3 = value;
    break;
  case 1:
    key2 = key;
    value2 = value;
    break;
  case 0:
    key1 = key;
    value1 = value;
    break;
  }
  _size++;
};

#endif
