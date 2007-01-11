
#ifndef MATH_PHYSICS_H
#define MATH_PHYSICS_H

#include "math/vector.h"
#include <vector>

class Particle 
{
    public:
        Particle(Vector& p, Vector& v, double m);
        Vector getPosition() const { return position; };
        void applyGravity(const Vector& gravity, double time_delta);
        void move(const Vector& v);
  
    private:
        double mass;
        Vector position;
        Vector velocity;
        double age;  
};

class Spring 
{
    public:
        Spring(Particle* p1, Particle* p2, double strength, double damping, double rest_length = -1);
        double currentLength() const;
        void tick(double t);
        
    private:
        Particle* p1;
        Particle* p2;
        double rest_length;
        double strength;
        double damping;
};

/**
 * Attractions or repulsions (negative attraction) act on two particles and either 
 * constantly pull them together or constantly pull them apart by applying a force 
 * to each particle:
 *
 *    G*m1*m2/d2 
 *
 * in other words the force is is much stronger close up than far away.
 */
class Attraction 
{
    public:
        double currentDistance() const;

    private:
        Particle* p1;
        Particle* p2;
        /// The G in the formula above
        double strength;         
        /// Minimum Distance, in practice, we are going to want to place a limit on 
        /// how strong that force can be, otherwise once something gets on top of whatever 
        /// it's attracted to it's not going to let it go. This limits the distance in 
        /// calculating the above force, but the force is always acting at all distances.
        double minimum_distance;
};

class ParticleSystem 
{
    public:
        ParticleSystem(const Vector& gravity, double drag);
        void tick(double t);
        void add(Particle* particle); 
        void add(Spring* spring); 
        void add(Attraction* attration); 
        
    private:
        Vector gravity;
        double drag;
        std::vector<Particle*> particles;
        std::vector<Spring*> springs;
        std::vector<Attraction*> attractions;
};

#endif
