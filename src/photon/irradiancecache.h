
/**
 * An irradiance cache as suggested by Greg Ward.
 *
 * @see http://radsite.lbl.gov/radiance/papers/erw92/paper.html
 * @see http://radsite.lbl.gov/radiance/papers/sg88/paper.html
 */
class IrradianceCache {

    public:
	/// Constructor
	IrradianceCache(double tolerance);

	/**
	 * Get an irradiance estimate. 
	 *
	 * This must be multiplied by the materials diffuse color.
	 *
	 * @param point the point we want diffuse irradiance estimate at
	 * @param normal the surface normal at point
	 */
	RGB getEstimate(const Vector& point, const Vector& normal) const;

	/**
	 * Insert an estimate into the list.
	 *
	 * This inserts the result of a final gather into the irradiance cache.
	 *
	 * The harmonic mean distance \f$H\f$ is defined as
	 *
	 * \f[ H = \frac{n}{K} \f]
	 *
	 * where 
	 *
	 * \f[ K = \sum_{i=1}^{n}{\frac{1}{n_i}} \f]
	 *
	 * @param point the point where the final gather was done
	 * @param normal the surface normal at point
	 * @param irrancance the result of the final gather
	 * @param hmd the harmonic mean distance to the objects that was hit by final gather rays.
	 * 
	 */
	void putEstimate(const Vector& point, const Vector& normal, const RGB& irrandiance, const double hmd);

    private:
	class CacheNode {

	    Vector point;
	    Vector normal;
	    RGB irradiance;
	    double hmd;

	    double getWeight(const Vector& point, const Vector& normal) const {

	    }

	    /** 
	     * This is the squared distance from point where this 
	     * CacheElement doesn't matter anymore.
	     */
	    double getRadius() const {

	    }
	}

}

