
#ifndef OBJECTS_BLOB
#define OBJECTS_BLOB

#include "objects/volume.h"

/**
 * A Blob is a collection of spheres with a power.
 * If each sphere \f$ S_i \f$ has the power \f$ P_i(x,y,z) \f$
 * this surface is the points \f$ (x,y,z) \f$ where
 *
 * \f[ \sum_{i} P_i(x,y,z) = k \f]
 *
 * for some constant \f$ k \f$.
 * 
 */
class Blob : public Volume {


};


#endif
