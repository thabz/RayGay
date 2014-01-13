
#include "math/physics.h"

using namespace std;

Particle::Particle(Vector& p, Vector& v, double m) {
    this->position = p;
    this->velocity = v;
    this->age = 0;
    this->mass = m;
}

void Particle::applyGravity(const Vector& gravity, double time_delta)
{
    velocity += gravity * time_delta;
    position += velocity * time_delta;
}

void Particle::move(const Vector& v) {
    position += v;
}

Spring::Spring(Particle* pa1, Particle* pa2, double s, double damping, double rest_length)
{
    /// If no rest_length is supplied the current distance between the particles is used
    if (rest_length == -1) {
        rest_length = (pa1->getPosition() - pa2->getPosition()).length();
    }   
    this->rest_length = rest_length;
    this->strength = s;
    this->damping = damping;
    this->p1 = pa1;
    this->p1 = pa2;
}

double Spring::currentLength() const 
{
    return (p1->getPosition() - p2->getPosition()).length();
}

void Spring::tick(double time_delta) {
    double l = currentLength();
    double F1 = strength * (rest_length - l);
    double F2 = -F1;
    Vector delta = (p2->getPosition()) - (p1->getPosition()) / l;
    p1->move(F1 * delta);
    p2->move(F2 * delta);
}

Attraction::Attraction(Particle* p1, Particle* p2, double strength, double minimum_distance) {
    this->p1 = p1;
    this->p2 = p2;
    this->strength = strength;
    this->minimum_distance = minimum_distance;
}

double Attraction::currentDistance() const 
{
    return (p1->getPosition() - p2->getPosition()).length();
}

ParticleSystem::ParticleSystem(const Vector& g, double d) 
{
    this->gravity = g;
    this->drag = d;
}

void ParticleSystem::add(Particle* particle) 
{
    particles.push_back(particle);
}

void ParticleSystem::add(Spring* spring)
{
    springs.push_back(spring);
} 

void ParticleSystem::add(Attraction* attration)
{
    attractions.push_back(attration);
} 

void ParticleSystem::tick(double time_delta)
{
    /// Apply gravity to all particles
    for(uint32_t i = 0; i < particles.size(); i++) {
        particles[i]->applyGravity(gravity, time_delta);
    }
    
    for(uint32_t i = 0; i < springs.size(); i++) {
        Spring* spring = springs[i];
        spring->tick(time_delta);
    }
    
}
