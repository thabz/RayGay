
#include <iostream>
#include <cmath>

#include "math/vector.h"
#include "image/rgb.h"


/**
 * This is the constructor for the photon map.
 * To create the photon map it is necessary to specify the
 * maximum number of photons that will be stored.
 *
 * @param max_phot The maximum number of photons that will be stored.
 * @param max_dist Max distance to look for photons when doing an irradiance estimate
 * @patam estimate_photons Number of photons to use when doing an irradiance estimate
 */
template <class PhotonType>
PhotonMap<PhotonType>::PhotonMap<PhotonType>( const int max_phot, double max_dist, int estimate_photons  )
{
    stored_photons = 0;
    prev_scale = 1;
    max_photons = max_phot;
    this->max_dist = max_dist;
    this->estimate_photons = estimate_photons;

    photons = (PhotonType*)malloc( sizeof( PhotonType ) * ( max_photons+1 ) );

    if (photons == NULL) {
	std::cerr << "Out of memory initializing photon map." << std::endl;
	exit(-1);
    }

    bbox_min[0] = bbox_min[1] = bbox_min[2] = 1e8f;
    bbox_max[0] = bbox_max[1] = bbox_max[2] = -1e8f;

    //----------------------------------------
    // initialize direction conversion tables
    //----------------------------------------

    for (int i=0; i<256; i++) {
	double angle = double(i)*(1.0/256.0)*M_PI;
	costheta[i] = cos( angle );
	sintheta[i] = sin( angle );
	cosphi[i]   = cos( 2.0*angle );
	sinphi[i]   = sin( 2.0*angle );
    }
}


/**
 * Destructor
 */
template <class PhotonType>
PhotonMap<PhotonType>:: ~PhotonMap<PhotonType>() {
    free(photons);
}


/**
 * Returns the direction of a photon
 */
template <class PhotonType>
void PhotonMap<PhotonType>::photon_dir( float *dir, const PhotonType *p ) const
{
    dir[0] = sintheta[p->theta]*cosphi[p->phi];
    dir[1] = sintheta[p->theta]*sinphi[p->phi];
    dir[2] = costheta[p->theta];
}

/**
 * Computes an irradiance estimate
 * at a given surface position
 * @param pos surface position
 * @param normal surface normal at pos
 */
template <class PhotonType>
Vector PhotonMap<PhotonType>::irradiance_estimate(
	const Vector& pos,
	const Vector& normal) const
	
{
    Vector irrad = Vector(0.0,0.0,0.0);

    NearestPhotons<PhotonType> np;
    np.dist2 = (float*)alloca( sizeof(float)*(estimate_photons+1) );
    np.index = (const PhotonType**)alloca( sizeof(PhotonType*)*(estimate_photons+1) );

    np.pos = pos;
    np.max = estimate_photons;
    np.found = 0;
    np.got_heap = 0;
    np.dist2[0] = max_dist*max_dist;

    // locate the nearest photons
    locate_photons( &np, 1 );

    // if less than 8 photons return
    if (np.found<8)
	return Vector(0,0,0);

    float pdir[3];

    // sum irradiance from all photons
    for (int i=1; i<=np.found; i++) {
	const PhotonType *p = np.index[i];
	// the photon_dir call and following if can be omitted (for speed)
	// if the scene does not have any thin surfaces
	photon_dir( pdir, p );
	if ( (pdir[0]*normal[0]+pdir[1]*normal[1]+pdir[2]*normal[2]) < 0.0f ) {
	    irrad[0] += p->power[0];
	    irrad[1] += p->power[1];
	    irrad[2] += p->power[2];
	}
    }

    irrad *= (1.0f/M_PI)/(np.dist2[0]);	// estimate of density;
    return irrad;
}


/**
 * Finds the nearest photons in the
 * photon map given the parameters in np
 */
template <class PhotonType>
void PhotonMap<PhotonType>::locate_photons(
	NearestPhotons<PhotonType> *const np,
	const int index ) const
{
    const PhotonType *p = &photons[index];
    float dist1;

    if (index<half_stored_photons) {
	dist1 = np->pos[ p->plane ] - p->pos[ p->plane ];

	if (dist1>0.0) { // if dist1 is positive search right plane
	    locate_photons( np, 2*index+1 );
	    if ( dist1*dist1 < np->dist2[0] )
		locate_photons( np, 2*index );
	} else {         // dist1 is negative search left first
	    locate_photons( np, 2*index );
	    if ( dist1*dist1 < np->dist2[0] )
		locate_photons( np, 2*index+1 );
	}
    }

    // compute squared distance between current photon and np->pos

    dist1 = p->pos[0] - np->pos[0];
    float dist2 = dist1*dist1;
    dist1 = p->pos[1] - np->pos[1];
    dist2 += dist1*dist1;
    dist1 = p->pos[2] - np->pos[2];
    dist2 += dist1*dist1;

    if ( dist2 < np->dist2[0] ) {
	// we found a photon :) Insert it in the candidate list

	if ( np->found < np->max ) {
	    // heap is not full; use array
	    np->found++;
	    np->dist2[np->found] = dist2;
	    np->index[np->found] = p;
	} else {
	    int j,parent;

	    if (np->got_heap==0) { // Do we need to build the heap?
		// Build heap
		float dst2;
		const PhotonType *phot;
		int half_found = np->found>>1;
		for ( int k=half_found; k>=1; k--) {
		    parent=k;
		    phot = np->index[k];
		    dst2 = np->dist2[k];
		    while ( parent <= half_found ) {
			j = parent+parent;
			if (j<np->found && np->dist2[j]<np->dist2[j+1])
			    j++;
			if (dst2>=np->dist2[j])
			    break;
			np->dist2[parent] = np->dist2[j];
			np->index[parent] = np->index[j];
			parent=j;
		    }
		    np->dist2[parent] = dst2;
		    np->index[parent] = phot;
		}
		np->got_heap = 1;
	    }

	    // insert new photon into max heap
	    // delete largest element, insert new and reorder the heap

	    parent=1;
	    j = 2;
	    while ( j <= np->found ) {
		if ( j < np->found && np->dist2[j] < np->dist2[j+1] )
		    j++;
		if ( dist2 > np->dist2[j] )
		    break;
		np->dist2[parent] = np->dist2[j];
		np->index[parent] = np->index[j];
		parent = j;
		j += j;
	    }
	    np->index[parent] = p;
	    np->dist2[parent] = dist2;

	    np->dist2[0] = np->dist2[1];
	}
    }
}

/**
 * Call this function to store a photon.
 *
 * Puts a photon into the flat array that will form
 * the final kd-tree.
 *
 * @param power The RGB power of the photon
 * @param pos The position of the photon
 * @param dir The direction of the photon
 */
template <class PhotonType>
void PhotonMap<PhotonType>::storeit(const PhotonType& photon) {

    if (isFull())
	return;

    stored_photons++;
    photons[stored_photons] = photon;
    PhotonType *const node = &photons[stored_photons];

    for (int i=0; i<3; i++) {
	if (node->pos[i] < bbox_min[i])
	    bbox_min[i] = node->pos[i];
	if (node->pos[i] > bbox_max[i])
	    bbox_max[i] = node->pos[i];
    }

}


/**
 * Scale the power of all photons once they have been emitted from the light
 * source.
 * Call this function after each light source is processed.
 *
 * @param scale should be 1/(#emitted photons).
 */
template <class PhotonType>
void PhotonMap<PhotonType>::scale_photon_power( const float scale ) {
    for (int i=prev_scale; i<=stored_photons; i++) {
	photons[i].power[0] *= scale;
	photons[i].power[1] *= scale;
	photons[i].power[2] *= scale;
    }
    prev_scale = stored_photons + 1;
}


/**
 * Creates a left balanced kd-tree from the flat photon array.
 *
 * This function should be called before the photon map
 * is used for rendering.
 */
template <class PhotonType>
void PhotonMap<PhotonType>::balance(void)
{
    if (stored_photons>1) {
	// allocate two temporary arrays for the balancing procedure
	PhotonType **pa1 = (PhotonType**)malloc(sizeof(PhotonType*)*(stored_photons+1));
	PhotonType **pa2 = (PhotonType**)malloc(sizeof(PhotonType*)*(stored_photons+1));

	for (int i=0; i<=stored_photons; i++)
	    pa2[i] = &photons[i];

	balance_segment( pa1, pa2, 1, 1, stored_photons );
	free(pa2);

	// reorganize balanced kd-tree (make a heap)
	int d, j=1, foo=1;
	PhotonType foo_photon = photons[j];

	for (int i=1; i<=stored_photons; i++) {
	    d=pa1[j]-photons;
	    pa1[j] = NULL;
	    if (d != foo)
		photons[j] = photons[d];
	    else {
		photons[j] = foo_photon;

		if (i<stored_photons) {
		    for (;foo<=stored_photons; foo++)
			if (pa1[foo] != NULL)
			    break;
		    foo_photon = photons[foo];
		    j = foo;
		}
		continue;
	    }
	    j = d;
	}
	free(pa1);
    }

    half_stored_photons = stored_photons/2-1;
}


//#define swap(ph,a,b) { PhotonType *ph2=ph[a]; ph[a]=ph[b]; ph[b]=ph2; }

template <class PhotonType>
void  PhotonMap<PhotonType>::swap(PhotonType** ph, int a, int b) {
    PhotonType *ph2=ph[a];
    ph[a]=ph[b];
    ph[b]=ph2; 
}


/**
 * Splits the photon array into two separate pieces around the median. 
 * 
 * This with all photons below the
 * the median in the lower half and all photons above
 * than the median in the upper half. The comparison
 * criteria is the axis (indicated by the axis parameter)
 * (inspired by routine in "Algorithms in C++" by Sedgewick)
 */
template <class PhotonType>
void PhotonMap<PhotonType>::median_split(
	PhotonType **p,
	const int start,               // start of photon block in array
	const int end,                 // end of photon block in array
	const int median,              // desired median number
	const int axis )               // axis to split along
{
    int left = start;
    int right = end;

    while ( right > left ) {
	const float v = p[right]->pos[axis];
	int i=left-1;
	int j=right;
	for (;;) {
	    while ( p[++i]->pos[axis] < v )
		;
	    while ( p[--j]->pos[axis] > v && j>left )
		;
	    if ( i >= j )
		break;
	    swap(p,i,j);
	}

	swap(p,i,right);
	if ( i >= median )
	    right=i-1;
	if ( i <= median )
	    left=i+1;
    }
}


// See "Realistic image synthesis using Photon Mapping" chapter 6
// for an explanation of this function
template <class PhotonType>
void PhotonMap<PhotonType>::balance_segment(
	PhotonType **pbal,
	PhotonType **porg,
	const int index,
	const int start,
	const int end )
{
    //--------------------
    // compute new median
    //--------------------

    int median=1;
    while ((4*median) <= (end-start+1))
	median += median;

    if ((3*median) <= (end-start+1)) {
	median += median;
	median += start-1;
    } else	
	median = end-median+1;

    //--------------------------
    // find axis to split along
    //--------------------------

    int axis=2;
    if ((bbox_max[0]-bbox_min[0])>(bbox_max[1]-bbox_min[1]) &&
	    (bbox_max[0]-bbox_min[0])>(bbox_max[2]-bbox_min[2]))
	axis=0;
    else if ((bbox_max[1]-bbox_min[1])>(bbox_max[2]-bbox_min[2]))
	axis=1;

    //------------------------------------------
    // partition photon block around the median
    //------------------------------------------

    median_split( porg, start, end, median, axis );

    pbal[ index ] = porg[ median ];
    pbal[ index ]->plane = axis;

    //----------------------------------------------
    // recursively balance the left and right block
    //----------------------------------------------

    if ( median > start ) {
	// balance left segment
	if ( start < median-1 ) {
	    const float tmp=bbox_max[axis];
	    bbox_max[axis] = pbal[index]->pos[axis];
	    balance_segment( pbal, porg, 2*index, start, median-1 );
	    bbox_max[axis] = tmp;
	} else {
	    pbal[ 2*index ] = porg[start];
	}
    }

    if ( median < end ) {
	// balance right segment
	if ( median+1 < end ) {
	    const float tmp = bbox_min[axis];		
	    bbox_min[axis] = pbal[index]->pos[axis];
	    balance_segment( pbal, porg, 2*index+1, median+1, end );
	    bbox_min[axis] = tmp;
	} else {
	    pbal[ 2*index+1 ] = porg[end];
	}
    }	
}

template <class PhotonType>
Vector PhotonMap<PhotonType>::photon_dir(const PhotonType* p) const {
    float dir[3];
    photon_dir(dir,p);
    return Vector(dir[0],dir[1],dir[2]);
}

template <class PhotonType>
void PhotonMap<PhotonType>::packVector(const Vector& dir, unsigned char* theta_dest, unsigned char* phi_dest) const {
    int theta = int( acos(dir[2])*(256.0/M_PI) );
    if (theta>255)
	*theta_dest = 255;
    else
	*theta_dest = (unsigned char)theta;

    int phi = int( atan2(dir[1],dir[0])*(256.0/(2.0*M_PI)) );
    if (phi>255)
	*phi_dest = 255;
    else if (phi<0)
	*phi_dest = (unsigned char)(phi+256);
    else
	*phi_dest = (unsigned char)phi;
}


