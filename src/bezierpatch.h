
#include "mesh.h"

class BezierPatch : public Mesh {
    public:
	BezierPatch(Vector* points, int xResolution, int yResolution, const Material& material);

    private:
	const Vector& getControlPoint(unsigned int i, unsigned int j) const;
	Vector getPoint(double u, double v) const;
	Vector* controlPoints;
};
