
#ifndef GENERIC_PHOTON_MAP
#define GENERIC_PHOTON_MAP

#include "photon.h"

class Vector;
class RGB;

/**
 * This structure is used only to locate the
 * nearest photons
 */
template<class PhotonType>
class NearestPhotons {
    public:
    int max;
    int found;
    int got_heap;
    Vector pos;
    float *dist2;
    const PhotonType **index;
};

/** 
 * This is the PhotonMap template class 
 *
 * An example implementation of the photon map data structure by Henrik Wann Jensen - February 2001.
 * Interface adapted to RayGay by Jesper Christensen - February 2003
 */
template<class PhotonType>
class PhotonMap {
    friend class Photon;

    public:
	PhotonMap(const int max_phot );
	virtual ~PhotonMap();

	virtual void scale_photon_power(
		const float scale );           // 1/(number of emitted photons)

	virtual void balance(void);              // balance the kd-tree (before use!)

	void storeit(const PhotonType& photon);

	virtual Vector irradiance_estimate(
		const Vector& pos,             // surface position
		const Vector& normal,          // surface normal at pos
		const float max_dist,          // max distance to look for photons
		const int nphotons ) const;    // number of photons to use

	virtual void locate_photons(
		NearestPhotons<PhotonType>* const np,      // np is used to locate the photons
		const int index ) const;       // call with index = 1

	void photon_dir(
		float *dir,                    // direction of photon (returned)
		const PhotonType *p ) const;       // the photon

	Vector photon_dir(const PhotonType* p) const;

	PhotonType* list() const { return photons; };

	bool isFull() const { return stored_photons >= max_photons; };

    protected:
	void packVector(const Vector& vector, unsigned char* theta, unsigned char* phi) const;
	Vector unpackVector(unsigned char* theta, unsigned char* phi) const;
	

    private:

	void swap(PhotonType** ph, int a, int b);

	void balance_segment(
		PhotonType **pbal,
		PhotonType **porg,
		const int index,
		const int start,
		const int end );

	void median_split(
		PhotonType **p,
		const int start,
		const int end,
		const int median,
		const int axis );


	int half_stored_photons;
	int max_photons;
	int prev_scale;

	float costheta[256];
	float sintheta[256];
	float cosphi[256];
	float sinphi[256];

	float bbox_min[3];		// use bbox_min;
	float bbox_max[3];		// use bbox_max;

    protected:
	int stored_photons;
	PhotonType *photons;

};

#include "genericphotonmap.cpp"

#endif
