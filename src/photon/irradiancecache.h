
/**
 * An irradiance cache as suggested by Greg Ward.
 *
 * @see http://radsite.lbl.gov/radiance/papers/erw92/paper.html
 * @see http://radsite.lbl.gov/radiance/papers/sg88/paper.html
 */
class IrradianceCache {

    public:
	IrradianceCache(double tolerance);

	/**
	 * Get an irradiance estimate. 
	 *
	 * This must be multiplied by the materials diffuse color.
	 *
	 * @param point the point we want diffuse irradiance estimate at
	 * @param normal the surface normal at point
	 */
	RGB getEstimate(Vector point, Vector normal);

	/**
	 * Insert an estimate into the list.
	 *
	 * This inserts the result of a final gather into the irrancance cache.
	 *
	 * @param point the point where the final gather was done
	 * @param normal the surface normal at point
	 * @param irrancance the result of the final gather
	 * @param mean_distance the mean distance to the objects that was hit by final gather rays.
	 * 
	 */
	void putEstimate(Vector point, Vector normal, RGB irrandiance, double mean_distance);

    private:
	class CacheElement {

	    Vector point;
	    Vector normal;
	    RGB irradiance;
	    double mean_distance;

	    double getWeight(Vector point, Vector normal) const {

	    }

	    /** 
	     * This is the squared distance from point where this 
	     * CacheElement doesn't matter anymore.
	     */
	    double getRadius() const {

	    }
	}

}

