
/**
 * A shape between a sphere and a box.
 *
 * \f[ f(x,y,z) = \left( x^{2/n_2} + y^{2/n_2} \right)^{n_2 / n_1} + z^{2/n_1} \f]
 * The surface is \f$ f(x,y,z) = 1 \f$ and the interior is \f$ f(x,y,z) < 1 \f$
 * 
 * @see http://astronomy.swin.edu.au/~pbourke/surfaces/superellipse/
 */
class SuperElliopsoid : public IsoSurface {

    public SuperElliopsoid(double n1, double n2, const Vector& scale);
}
