
#ifndef GENERIC_PHOTON_MAP
#define GENERIC_PHOTON_MAP

class Vector;
class RGB;

/**
 * This is the photon
 * The power is not compressed so the
 * size is 28 bytes
 */
class Photon {
    public:
	float pos[3];                 ///< photon position
	short plane;                  ///< splitting plane for kd-tree
	/// incoming direction
	unsigned char theta, phi;     
	float power[3];               ///< photon power (uncompressed)

	/// Get the direction of this photon
	Vector getDirection() const;
	/// Set the direction of this photon
	void setDirection(const Vector& vector);

	/// Get the power
	RGB getPower() const;
	/// Set the power
	void setPower(const RGB& power);
};

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

	virtual void store(
		const Vector& power,          // photon power
		const Vector& pos,            // photon position
		const Vector& dir );          // photon direction

	virtual void scale_photon_power(
		const float scale );           // 1/(number of emitted photons)

	virtual void balance(void);              // balance the kd-tree (before use!)

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

	PhotonType *photons;

	int stored_photons;
	int half_stored_photons;
	int max_photons;
	int prev_scale;

	float costheta[256];
	float sintheta[256];
	float cosphi[256];
	float sinphi[256];

	float bbox_min[3];		// use bbox_min;
	float bbox_max[3];		// use bbox_max;
};

#include "genericphotonmap.cpp"

#endif
