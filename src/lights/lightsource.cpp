
#include "math/functions.h"
#include "math/matrix.h"
#include "lights/lightsource.h"

Lightsource::Lightsource(const Vector& position) {
    this->position = position;
    this->fadeEnabled = false;
}

void Lightsource::transform(const Matrix& m) {
    position = m * position;
}

/**
 * This method sets the attenuation parameters. The attenuation
 * is factor [0,1] which describes the intensity of light based
 * of distance from a lightsource.
 * 
 * \f[ A = \frac{1}{1 + \left( \frac{d}{D} \right)^P } \f]
 *
 * Where \f$ D \f$ is fadeDistance and \f$ P \f$ is fadePower.
 *
 * @param fadeDistance Distance from light where fading begins
 * @param fadePower The fading falloff rate.
 */
void Lightsource::setAttenuation(double fadeDistance, double fadePower) {
    this->fadeDistance = fadeDistance;
    this->fadePower = fadePower;
    this->fadeEnabled = true;
}

double Lightsource::getAttenuation(const Vector& point) const {
    if (fadeEnabled) {
	double d = (this->getPosition() - point).length();
	double attenuation = double(2) / (double(1) + pow(d/fadeDistance,fadePower));
	return Math::clamp(attenuation);
    } else {
	return double(1);
    }
}
