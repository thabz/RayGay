#ifndef EDGEKEY_H
#define EDGEKEY_H

class EdgeKey {

public:
  EdgeKey(int iV0 = -1, int iV1 = -1);
  bool operator<(const EdgeKey &rh) const;
  int V[2];
};

inline EdgeKey::EdgeKey(int iV0, int iV1) {
  if (iV0 < iV1) {
    V[0] = iV0;
    V[1] = iV1;
  } else {
    V[0] = iV1;
    V[1] = iV0;
  }
}

inline bool EdgeKey::operator<(const EdgeKey &rkKey) const {
  if (V[1] < rkKey.V[1])
    return true;
  if (V[1] > rkKey.V[1])
    return false;
  return V[0] < rkKey.V[0];
}

#endif /* EDGEKEY_H */
