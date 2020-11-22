#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cstdlib>
#include <iostream>

#include "aabox.h"
#include "materials/material.h"
#include "objects/bound.h"
#include "objects/box.h"
#include "objects/cone.h"
#include "objects/csg.h"
#include "objects/cylinder.h"
#include "objects/ellipsoid.h"
#include "objects/extrusion.h"
#include "objects/halfspace.h"
#include "objects/heightfield.h"
#include "objects/mesh.h"
#include "objects/solidbox.h"
#include "objects/sphere.h"
#include "objects/superellipsoid.h"
//#include "objects/necklace.h"
#include "image/image.h"
#include "image/simpletexture.h"
#include "image/texture.h"
#include "objects/tessalation.h"
#include "objects/tetrahedron.h"
#include "objects/torus.h"
#include "objects/transformedinstance.h"
#include "paths/circle.h"
#include "paths/linesegment.h"
#include "space/kdtree.h"
#include "testing.h"

bool intersects(Object *o, const Ray &ray) { return o->fastIntersect(ray) > 0; }
bool intersects(Object *o, const Vector &origin, const Vector &dir) {
  Ray ray = Ray(origin, dir, 1);
  return intersects(o, ray);
}

Vector iPoint(Object *o, const Ray &ray) {
  double t = o->fastIntersect(ray);
  if (t <= 0) {
    cout << "assert failed: t < 0!" << endl;
    return Vector(-1, -1, -1);
  }
  // assert(t > 0);
  Intersection intersection;
  o->fullIntersect(ray, t, intersection);
  return intersection.getPoint();
}

Vector iPoint(Object *o, const Vector &origin, const Vector &dir) {
  Ray ray = Ray(origin, dir, 1);
  return iPoint(o, ray);
}

Vector iNormal(Object *o, const Ray &ray) {
  double t = o->fastIntersect(ray);
  if (t <= 0) {
    cout << "assert failed: t < 0!" << endl;
    return Vector(-1, -1, -1);
  }
  Intersection intersection;
  o->fullIntersect(ray, t, intersection);
  return intersection.getNormal();
}

Vector iNormal(Object *o, const Vector &origin, const Vector &dir) {
  Ray ray = Ray(origin, dir, 1);
  return iNormal(o, ray);
}

/**
 * This fires a bunch of rays at an object.
 * This assumes that (0,0,0) is inside the object
 */
bool intersectionCheck(Object *object, double radius) {
  int failures = 0;
  int num = 10000;
  int checked = 0;
  for (int i = 0; i < num; i++) {
    Vector v = Vector::randomUnitVector();
    Vector pos = v * radius;
    Vector dir = -1 * v;
    Ray ray = Ray(pos, dir, -1);
    double t = object->fastIntersect(ray);
    if (t <= 0 || t > radius) {
      failures++;
    }
    checked++;
  }
  // cout << "Checked points: " << checked << endl;
  return failures == 0;
}

/**
 * This fires a bunch of rays at an object and checks
 * that the returned normals all have length one.
 */
bool normalCheck(Object *object, double radius) {
  int num = 10000;
  int checked = 0;
  int failures = 0;
  for (int i = 0; i < num; i++) {
    Vector v = Vector::randomUnitVector();
    Vector pos = v * radius;
    Vector dir = -1 * v;
    Ray ray = Ray(pos, dir, -1);
    if (intersects(object, ray)) {
      if (!IS_EQUAL(iNormal(object, ray).length(), 1.0)) {
        failures++;
      };
      checked++;
    }
  }
  // cout << "Checked normals: " << checked << endl;
  return failures == 0;
}

/**
 * This fires a bunch of rays at an object. And from
 * each intersection point it fires another ray with
 * the same direction. An object passing this test
 * should be eligible for using as a transparent object.
 */
bool transparentCheck(Object *object, double radius) {
  int failures = 0;
  int num = 10000;
  int checked = 0;
  for (int i = 0; i < num; i++) {
    Vector v = Vector::randomUnitVector();
    Vector pos = v * radius;
    Vector dir = -1 * v;
    Ray ray = Ray(pos, dir, -1);
    while (intersects(object, ray)) {
      if (iPoint(object, ray) == pos)
        failures++;

      pos = iPoint(object, ray);
      ray = Ray(pos, dir, -1);
      checked++;
    }
  }
  // cout << "Checked points: " << checked << endl;
  return failures == 0;
}

class ray_test : public Test {
public:
  void run() {
    Ray ray = Ray();
    assertTrue(ray.ignore());
    ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    assertFalse(ray.ignore());
  }
};

class sphere_test : public Test {
public:
  void run() {
    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    Sphere s = Sphere(Vector(0, 0, 0), 10.0, m);
    assertTrue(s.inside(Vector(0, 0, 9)));
    assertTrue(s.inside(Vector(0, 0, -9)));
    assertFalse(s.inside(Vector(0, 9, 9)));

    assertEqualF(s.signedDistance(Vector(0, 0, 9)), -1);
    assertEqualF(s.signedDistance(Vector(0, 0, -8)), -2);
    assertEqualF(s.signedDistance(Vector(0, 0, -12)), 2);
    assertEqualF(s.signedDistance(Vector(0, 12, 0)), 2);

    s = Sphere(Vector(0, 0, 0), 60.0, m);

    /* Test intersection(ray) */
    // assertTrue(intersects(&s,Vector(0,0,1000),Vector(0,0,-1)));
    // assertTrue(IS_EQUAL(s.getLastIntersection()->getPoint()[2],60.0));

    assertTrue(intersects(&s, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertEqualF(iPoint(&s, Vector(0, 0, 1000), Vector(0, 0, -1))[2], 60.0);

    // r = Ray(Vector(0,0,0),Vector(0,0,-1),1);
    // assertTrue(s.intersect(r));
    //   assertTrue(IS_EQUAL( s.getLastIntersection()->getPoint()[2], -60.0));

    assertTrue(intersects(&s, Vector(0, 0, 0), Vector(0, 0, -1)));
    assertEqualF(iPoint(&s, Vector(0, 0, 0), Vector(0, 0, -1))[2], -60.0);

    assertTrue(intersects(&s, Vector(0, 0, -1000), Vector(0, 0, 1)));
    assertEqualF(iPoint(&s, Vector(0, 0, -1000), Vector(0, 0, 1))[2], -60.0);

    assertTrue(intersects(&s, Vector(0, 0, -100), Vector(0, 0, -1)) == false);

    assertTrue(intersects(&s, Vector(0, 0, -60), Vector(0, 0, -1)) == false);

    /* Test getBoundingBox() */
    s = Sphere(Vector(0, 0, 0), 20.0, m);
    assertTrue(s.getBoundingBox().inside(
        AABox(Vector(-20, -20, -20), Vector(20, 20, 20))));

    /* Test clone */
    Object *s1 = new Sphere(Vector(0, 0, 0), 20, m);
    Object *s2 = dynamic_cast<Object *>(s1->clone());
    s1->transform(Matrix::matrixTranslate(Vector(0, -100, 0)));
    s2->transform(Matrix::matrixTranslate(Vector(0, 100, 0)));
    assertTrue(intersects(s1, Vector(0, -100, -1000), Vector(0, 0, 1)) == true);
    assertTrue(intersects(s2, Vector(0, -100, -1000), Vector(0, 0, 1)) ==
               false);
    assertTrue(intersects(s1, Vector(0, 100, -1000), Vector(0, 0, 1)) == false);
    assertTrue(intersects(s2, Vector(0, 100, -1000), Vector(0, 0, 1)) == true);

    /* Test AllIntersections */
    s = Sphere(Vector(0, 0, 0), 20.0, m);
    Intersection *result =
        (Intersection *)::alloca(sizeof(Intersection) * s.maxIntersections());
    Ray ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    uint32_t num = s.allIntersections(ray, result);
    assertTrue(num == 2);
    assertEqualV(result[0].getPoint(), Vector(0, 0, 20));
    assertTrue(result[0].isEntering() == true);
    assertEqualV(result[1].getPoint(), Vector(0, 0, -20));
    assertTrue(result[1].isEntering() == false);

    ray = Ray(Vector(0, 0, -100), Vector(0, 0, 1), -1);
    num = s.allIntersections(ray, result);
    assertTrue(num == 2);
    assertTrue(result[0].getPoint() == Vector(0, 0, -20));
    assertTrue(result[0].isEntering() == true);
    assertTrue(result[1].getPoint() == Vector(0, 0, 20));
    assertTrue(result[1].isEntering() == false);

    ray = Ray(Vector(0, 0, 0), Vector(0, 0, 1), -1);
    num = s.allIntersections(ray, result);
    assertTrue(num == 1);
    assertTrue(result[0].getPoint() == Vector(0, 0, 20));
    assertTrue(result[0].isEntering() == false);

    ray = Ray(Vector(0, 0, 0), Vector(0, 0, -1), -1);
    num = s.allIntersections(ray, result);
    assertTrue(num == 1);
    assertTrue(result[0].getPoint() == Vector(0, 0, -20));
    assertTrue(result[0].isEntering() == false);

    // Test returned normals
    s = Sphere(Vector(0, 0, 0), 20.0, NULL);
    assertTrue(intersectionCheck(&s, 100));
    assertTrue(normalCheck(&s, 100));
    assertTrue(transparentCheck(&s, 100));
  }
};

class halfspace_test : public Test {
public:
  void run() {
    AABox b;
    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    Halfspace *s = new Halfspace(Vector(0, 1, 0), 0.0, m);
    assertTrue(s->inside(Vector(0, -0.1, 0)));
    assertTrue(s->inside(Vector(0, -100, 0)));
    assertTrue(s->inside(Vector(10, -100, 10)));
    assertTrue(s->inside(Vector(-10, -10, -10)));
    assertTrue(s->inside(Vector(10, -10, -10)));
    assertTrue(s->inside(Vector(-10, -10, 10)));
    assertFalse(s->inside(Vector(0, 0.1, 0)));
    assertFalse(s->inside(Vector(10, 0.1, 10)));

    assertTrue(intersects(s, Vector(0, 10, 0), Vector(0, -1, 0)));
    assertTrue(intersects(s, Vector(10, 99, 10), Vector(-1, -1, 0)));
    assertTrue(intersects(s, Vector(-1, 10, -1), Vector(0, -1, 0)));
    assertTrue(intersects(s, Vector(1, 10, 1), Vector(0, -1, 0)));
    assertTrue(intersects(s, Vector(1, 10, -1), Vector(0, -1, 0)));
    assertTrue(intersects(s, Vector(-1, 10, 1), Vector(0, -1, 0)));
    assertFalse(intersects(s, Vector(0, 1, 0), Vector(0, 1, 0)));
    assertFalse(intersects(s, Vector(0, 1, 0), Vector(1, 0, 0)));

    KdTree *bsp = new KdTree();
    s->addSelf(bsp);
    bsp->prepare();
    Intersection inter;
    Ray r = Ray(Vector(0, 10, 0), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0, 0, 0));
    r = Ray(Vector(10, 10, 10), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(10, 0, 10));
    r = Ray(Vector(-10, 10, -10), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(-10, 0, -10));

    assertTrue(normalCheck(s, 100));

    s = new Halfspace(Vector(0, -1, 0), 0.0, m);
    assertTrue(s->inside(Vector(0, 1, 0)));
    assertFalse(s->inside(Vector(0, -1, 0)));

    // Halfspace is below the plane y = -1
    s = new Halfspace(Vector(0, 1, 0), 1.0, m);
    assertTrue(s->inside(Vector(0, -2, 0)));
    assertFalse(s->inside(Vector(0, 0, 0)));
    assertFalse(s->inside(Vector(0, 2, 0)));
    s = new Halfspace(Vector(0, 1, 0), Vector(0, -1, 0), m);
    assertTrue(s->inside(Vector(0, -2, 0)));
    assertFalse(s->inside(Vector(0, 0, 0)));
    assertFalse(s->inside(Vector(0, 2, 0)));
#ifdef HALFSPACE_OPTIMIZED_AABOX
    b = s->getBoundingBox();
    assertTrue(b.inside(Vector(0, -2, 0)));
    assertTrue(b.inside(Vector(0, -1, 0)));
    assertFalse(b.inside(Vector(0, 0, 0)));
    assertFalse(b.inside(Vector(0, 2, 0)));
#endif

    // Halfspace is below the plane y = 1
    s = new Halfspace(Vector(0, 1, 0), -1.0, m);
    assertTrue(s->inside(Vector(0, -2, 0)));
    assertTrue(s->inside(Vector(0, 0, 0)));
    assertFalse(s->inside(Vector(0, 2, 0)));
    s = new Halfspace(Vector(0, 1, 0), Vector(0, 1, 0), m);
    assertTrue(s->inside(Vector(0, -2, 0)));
    assertTrue(s->inside(Vector(0, 0, 0)));
    assertFalse(s->inside(Vector(0, 2, 0)));
#ifdef HALFSPACE_OPTIMIZED_AABOX
    b = s->getBoundingBox();
    assertTrue(b.inside(Vector(0, -2, 0)));
    assertTrue(b.inside(Vector(0, 0, 0)));
    assertTrue(b.inside(Vector(0, 1, 0)));
    assertFalse(b.inside(Vector(0, 2, 0)));
#endif

    // Halfspace is over the plane y = -1
    s = new Halfspace(Vector(0, -1, 0), -1.0, m);
    assertFalse(s->inside(Vector(0, -2, 0)));
    assertTrue(s->inside(Vector(0, 0, 0)));
    assertTrue(s->inside(Vector(0, 2, 0)));
    s = new Halfspace(Vector(0, -1, 0), Vector(0, -1, 0), m);
    assertFalse(s->inside(Vector(0, -2, 0)));
    assertTrue(s->inside(Vector(0, 0, 0)));
    assertTrue(s->inside(Vector(0, 2, 0)));
#ifdef HALFSPACE_OPTIMIZED_AABOX
    b = s->getBoundingBox();
    assertFalse(b.inside(Vector(0, -2, 0)));
    assertTrue(b.inside(Vector(0, -1, 0)));
    assertTrue(b.inside(Vector(0, 0, 0)));
    assertTrue(b.inside(Vector(0, 2, 0)));
#endif

    // Halfspace is over the plane y = 1
    s = new Halfspace(Vector(0, -1, 0), 1.0, m);
    assertFalse(s->inside(Vector(0, -2, 0)));
    assertFalse(s->inside(Vector(0, 0, 0)));
    assertTrue(s->inside(Vector(0, 2, 0)));
    s = new Halfspace(Vector(0, -1, 0), Vector(0, 1, 0), m);
    assertFalse(s->inside(Vector(0, -2, 0)));
    assertFalse(s->inside(Vector(0, 0, 0)));
    assertTrue(s->inside(Vector(0, 2, 0)));
#ifdef HALFSPACE_OPTIMIZED_AABOX
    b = s->getBoundingBox();
    assertFalse(b.inside(Vector(0, -2, 0)));
    assertFalse(b.inside(Vector(0, 0, 0)));
    assertTrue(b.inside(Vector(0, 1, 0)));
    assertTrue(b.inside(Vector(0, 2, 0)));
#endif

    // Test transformation by creating a halfspace and
    // one point inside it and one point outside it.
    // Rotate and translate the hell out of all three and check that
    // the inside point is still inside and that the outside point is
    // still outside
    // Also shoot a ray from the outside point towards the inside
    // point and check that a correct intersection occurs.
    // Halfspace is y <= 0
    s = new Halfspace(Vector(0, 1, 0), 0.0, m);
    Vector in = Vector(0, -2, 0);
    Vector out = Vector(0, 2, 0);
    for (int i = 0; i < 1000; i++) {
      Vector angles =
          Vector(RANDOM(0, M_2PI), RANDOM(0, M_2PI), RANDOM(0, M_2PI));
      Vector trans = Vector(RANDOM(-10, 10), RANDOM(-10, 10), RANDOM(-10, 10));
      Matrix m = Matrix::matrixRotate(angles);
      m *= Matrix::matrixTranslate(trans);
      in = m * in;
      out = m * out;
      s->transform(m);
      assertFalse(s->inside(out));
      assertTrue(s->inside(in));

      Vector dir = (in - out).normalized();
      Ray ray = Ray(out, dir, 1);
      Intersection inter;
      double t = s->fastIntersect(ray);
      assertTrue(t > 0);
      s->fullIntersect(ray, t, inter);
      // cout << inter.getPoint() << " == " << 0.5*(in+out) << endl;
      assertTrue(inter.isIntersected());
      assertEqualV(inter.getPoint(), 0.5 * (in + out));
    }
  }
};

class box_test : public Test {
public:
  void run() {
    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    Box *b = new Box(Vector(-1, -1, -1), Vector(1, 1, 1), m);
    b->prepare();
    assertTrue(b->getVertices()->size() == 8);

    KdTree *bsp = new KdTree();
    b->addSelf(bsp);
    bsp->prepare();
    Intersection inter;
    Ray r = Ray(Vector(0, 0, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0, 0, 1));
    assertTrue(inter.getUV() == Vector2(0.5, 0.5));

    r = Ray(Vector(0, -100, 0), Vector(0, 1, 0), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0, -1, 0));

    /* Test second constructor */
    b = new Box(Vector(0, 0, 0), 2, 2, 2, m);
    b->prepare();
    assertTrue(b->getVertices()->size() == 8);

    bsp = new KdTree();
    b->addSelf(bsp);
    bsp->prepare();

    r = Ray(Vector(0, 0, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertTrue(inter.getPoint() == Vector(0, 0, 1));

    r = Ray(Vector(0, -100, 0), Vector(0, 1, 0), 1);
    assertTrue(bsp->intersect(r, inter));
    assertTrue(inter.getPoint() == Vector(0, -1, 0));

    r = Ray(Vector(0, -100, 1.5), Vector(0, 1, 0), 1);
    assertTrue(bsp->intersect(r, inter) == false);

    /* test clone() */
    b = new Box(Vector(-1, -1, -1), Vector(1, 1, 1), m);
    SceneObject *b2 = b->clone();
    assertTrue(b2 != NULL);
    b->transform(Matrix::matrixTranslate(Vector(0, 10, 0)));
    b->prepare();
    b2->prepare();
    bsp = new KdTree();
    b->addSelf(bsp);
    b2->addSelf(bsp);
    bsp->prepare();

    Intersection i;
    r = Ray(Vector(0, 0, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, i));
    r = Ray(Vector(0, 10, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, i));
    r = Ray(Vector(0, 5, 100), Vector(0, 0, -1), 1);
    assertTrue(!bsp->intersect(r, i));
  }
};

class mesh_test : public Test {
public:
  void run() {
    ///////////////////////////////////////////////////////////////
    // Test Mesh::clone()
    ///////////////////////////////////////////////////////////////
    Box *b1 = new Box(Vector(-1, -1, -1), Vector(1, 1, 1), NULL);
    Mesh *b2 = dynamic_cast<Mesh *>(b1->clone());
    assertTrue(b2 != NULL);

    b1->prepare();
    b2->prepare();

    b1->transform(Matrix::matrixTranslate(Vector(10, 10, 0)));

    KdTree *bsp = new KdTree();
    b1->addSelf(bsp);
    b2->addSelf(bsp);
    bsp->prepare();

    Intersection inter;
    Ray r;

    r = Ray(Vector(0, 0, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0, 0, 1));
    assertTrue(inter.getUV() == Vector2(0.5, 0.5));

    r = Ray(Vector(0.5, -0.5, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0.5, -0.5, 1));

    r = Ray(Vector(10, 10, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(10, 10, 1));
    assertTrue(inter.getUV() == Vector2(0.5, 0.5));

    r = Ray(Vector(10.5, 10.5, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(10.5, 10.5, 1));

#ifndef TRIANGLE_BACKFACE_CULLING
    // Test ray from inside.
    r = Ray(Vector(0.5, 0.8, 0.5), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
    assertEqualV(inter.getPoint(), Vector(0.5, 0.8, -1));
#endif
  }
};

class tetrahedron_test : public Test {
public:
  void run() {
    Material *mat = new Material(RGB(.0, .0, .0), RGB(.0, .0, .0));
    Tetrahedron t = Tetrahedron(Vector(0, 0, 0), 100, mat);
    assertTrue(t.getEdges()->size() == 6);
    assertTrue(t.getVertices()->size() == 4);
  }
};

class tesselation_test : public Test {
public:
  void run() {
    Material *mat = new Material(RGB(.0, .0, .0), RGB(.0, .0, .0));

    // 4 triangles
    Tessalation *t = new Tessalation(Vector(0, 0, 0), 100, 0, mat);
    assertTrue(t->getEdges()->size() == 6);
    assertTrue(t->getVertices()->size() == 4);

    // 12 triangles
    t = new Tessalation(Vector(0, 0, 0), 100, 1, mat);
    assertTrue(t->getVertices()->size() == 8);
    // cout << t->getEdges()->size() << endl;
    //  assertTrue(t->getEdges()->size() == 4 * 3);

    // 36 triangles
    t = new Tessalation(Vector(0, 0, 0), 100, 2, mat);
    assertTrue(t->getVertices()->size() == 20);

    // 108 triangles
    t = new Tessalation(Vector(0, 0, 0), 100, 3, mat);
    assertTrue(t->getVertices()->size() == 56);
  }
};

class extrusion_test : public Test {
public:
  void run() {

    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    // Check bounds
    Vector o = Vector(0, 0, 0);
    Vector top = Vector(10, 0, 0);
    AABox b = AABox(Vector(-1, -10, -10), Vector(11, 10, 10));

    Extrusion *c = new Extrusion(o, top, 9.0, 5, m);

    c = new Extrusion(top, o, 9.0, 5, m);

    top = Vector(0, 10, 0);
    b = AABox(Vector(-10, -1, -10), Vector(10, 11, 10));
    c = new Extrusion(o, top, 5.0, 5, m);

    // Check intersection
    c = new Extrusion(Vector(0, 0, 0), Vector(0, 0, -10), 5.0, 3, m);
    c->prepare();
    KdTree *bsp = new KdTree();
    c->addSelf(bsp);
    bsp->prepare();
    Intersection i;
    Ray r = Ray(Vector(0.5, 0.5, 100), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, i));

    // Check generated mesh
    c = new Extrusion(Vector(0, 0, 0), Vector(0, 0, -10), 2.0, 5, m);
    c->prepare();
    assertTrue(c->getVertices()->size() == 5 * 2 + 2);

    Circle circle1 = Circle(Vector(0, 75, 0), 200, Vector(0, 1, 0));
    Extrusion torus = Extrusion(
        circle1, 100, 16, 10,
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.20, 30));
    torus.prepare();

    assertTrue(torus.getVertices()->size() == 16 * 10);
  }
};

class cylinder_test : public Test {
public:
  void run() {
    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    Cylinder *cyl =
        new Cylinder(Vector(0, 0, 0), Vector(0, 0, 10), 10, false, m);

    /* Test intersection() of z-axis aligned cylinder */

    assertTrue(intersects(cyl, Vector(0, -1000, 5), Vector(0, 1, 0)));
    assertTrue(iPoint(cyl, Vector(0, -1000, 5), Vector(0, 1, 0)) ==
               Vector(0, -10, 5));
    assertTrue(iNormal(cyl, Vector(0, -1000, 5), Vector(0, 1, 0)) ==
               Vector(0, -1, 0));

    assertTrue(intersects(cyl, Vector(0, 1000, 5), Vector(0, -1, 0)));
    assertTrue(iPoint(cyl, Vector(0, 1000, 5), Vector(0, -1, 0)) ==
               Vector(0, 10, 5));
    assertTrue(iNormal(cyl, Vector(0, 1000, 5), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));

    // Test inside(p)
    assertTrue(cyl->inside(Vector(0, 0, 1)));
    assertTrue(cyl->inside(Vector(0, 9, 9)));
    assertFalse(cyl->inside(Vector(9, 9, 9)));
    assertFalse(cyl->inside(Vector(0, 0, 11)));
    delete cyl;

    // Test a cylinder translated along the z-axis
    cyl = new Cylinder(Vector(0, 0, 2), Vector(0, 0, 10), 10, false, m);

    // Test inside(p)
    assertFalse(cyl->inside(Vector(0, 0, 2)));
    assertTrue(cyl->inside(Vector(0, 0, 3)));
    assertTrue(cyl->inside(Vector(0, 9, 9)));
    assertFalse(cyl->inside(Vector(9, 9, 9)));
    assertFalse(cyl->inside(Vector(0, 0, 11)));

    delete cyl;

    // Test an x-axis aligned cylinder
    cyl = new Cylinder(Vector(2, 0, 0), Vector(10, 0, 0), 10, false, m);
    assertTrue(intersects(cyl, Vector(3, 0, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(cyl, Vector(3, 0, 1000), Vector(0, 0, -1)) ==
               Vector(3, 0, 10));
    assertTrue(iNormal(cyl, Vector(3, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));

    delete cyl;

    // Test an y-axis aligned cylinder
    cyl = new Cylinder(Vector(0, 2, 0), Vector(0, 10, 0), 10, false, m);
    assertTrue(intersects(cyl, Vector(0, 3, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(cyl, Vector(0, 3, 1000), Vector(0, 0, -1)) ==
               Vector(0, 3, 10));
    assertTrue(iNormal(cyl, Vector(0, 3, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));

    assertTrue(intersects(cyl, Vector(0, 0, 100), Vector(0, 0, -1)) == false);
    delete cyl;

    // test allIntersections
    cyl = new Cylinder(Vector(0, 2, 0), Vector(0, 10, 0), 10, true, m);
    Intersection *result = (Intersection *)::alloca(sizeof(Intersection) *
                                                    cyl->maxIntersections());
    Ray ray = Ray(Vector(0, 5, 1000), Vector(0, 0, -1), -1);
    uint32_t num = cyl->allIntersections(ray, result);
    assertTrue(num == 2);
    assertTrue(result[0].getPoint() == Vector(0, 5, 10));
    assertTrue(result[1].getPoint() == Vector(0, 5, -10));

    // Intersect with caps (z-axis aligned)
    cyl = new Cylinder(Vector(0, 0, 2), Vector(0, 0, 10), 10, true, m);
    ray = Ray(Vector(0, 0, 50), Vector(0, 0, -1), -1);
    num = cyl->allIntersections(ray, result);
    assertTrue(num == 2);
    assertTrue(result[0].getPoint() == Vector(0, 0, 10));
    assertTrue(result[0].getNormal() == Vector(0, 0, 1));
    assertTrue(result[1].getPoint() == Vector(0, 0, 2));
    assertTrue(result[1].getNormal() == Vector(0, 0, -1));

    // Intersect with caps (y-axis aligned)
    cyl = new Cylinder(Vector(0, 2, 0), Vector(0, 10, 0), 10, true, m);
    ray = Ray(Vector(0, 50, 1), Vector(0, -1, 0), -1);
    num = cyl->allIntersections(ray, result);
    assertTrue(num == 2);
    assertTrue(result[0].getPoint() == Vector(0, 10, 1));
    assertTrue(result[0].getNormal() == Vector(0, 1, 0));
    assertTrue(result[1].getPoint() == Vector(0, 2, 1));
    assertTrue(result[1].getNormal() == Vector(0, -1, 0));
    assertTrue(iPoint(cyl, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 10, 0));
    assertTrue(iNormal(cyl, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iPoint(cyl, Vector(1, 1000, 1), Vector(0, -1, 0)) ==
               Vector(1, 10, 1));
    assertTrue(iNormal(cyl, Vector(1, 1000, 1), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));

    // Test clone()
    Object *s1 = new Cylinder(Vector(2, 0, 0), Vector(10, 0, 0), 10, true, m);
    Object *s2 = dynamic_cast<Object *>(s1->clone());
    s1->transform(Matrix::matrixTranslate(Vector(0, -100, 0)));
    s2->transform(Matrix::matrixTranslate(Vector(0, 100, 0)));
    assertTrue(intersects(s1, Vector(5, -100, -1000), Vector(0, 0, 1)) == true);
    assertTrue(intersects(s2, Vector(5, -100, -1000), Vector(0, 0, 1)) ==
               false);
    assertTrue(intersects(s1, Vector(5, 100, -1000), Vector(0, 0, 1)) == false);
    assertTrue(intersects(s2, Vector(5, 100, -1000), Vector(0, 0, 1)) == true);

    // Rays with origin on surface
    cyl = new Cylinder(Vector(0, 2, 0), Vector(0, 10, 0), 10, true, m);
    assertTrue(!intersects(cyl, Vector(0, 10, 0), Vector(0, 1, 0)));
    assertTrue(intersects(cyl, Vector(0, 10, 0), Vector(0, -1, 0)));
    assertTrue(iPoint(cyl, Vector(0, 10, 0), Vector(0, -1, 0)) ==
               Vector(0, 2, 0));
    assertTrue(iPoint(cyl, Vector(0, 2, 0), Vector(0, 1, 0)) ==
               Vector(0, 10, 0));
    delete cyl;

    // Ray with origin inside cylinder
    cyl = new Cylinder(Vector(0, 0, 2), Vector(0, 0, 10), 10, true, m);
    assertTrue(intersects(cyl, Vector(0, 0, 5), Vector(0, 0, 1)));
    assertTrue(iPoint(cyl, Vector(0, 0, 5), Vector(0, 0, 1)) ==
               Vector(0, 0, 10));
    assertTrue(iNormal(cyl, Vector(0, 0, 5), Vector(0, 0, 1)) ==
               Vector(0, 0, 1));
    assertTrue(intersects(cyl, Vector(0, 0, 5), Vector(0, 1, 0)));
    assertTrue(iPoint(cyl, Vector(0, 0, 5), Vector(0, 1, 0)) ==
               Vector(0, 10, 5));
    assertTrue(iNormal(cyl, Vector(0, 0, 5), Vector(0, 1, 0)) ==
               Vector(0, 1, 0));

    cyl = new Cylinder(Vector(0, 2, 0), Vector(0, 10, 0), 10, true, m);
    assertTrue(intersects(cyl, Vector(0, 5, 0), Vector(0, 1, 0)));
    assertTrue(iPoint(cyl, Vector(0, 5, 0), Vector(0, 1, 0)) ==
               Vector(0, 10, 0));
    assertTrue(iNormal(cyl, Vector(0, 5, 0), Vector(0, 1, 0)) ==
               Vector(0, 1, 0));

    // Scaled cylinder
    cyl = new Cylinder(Vector(0, 0, 0), Vector(0, 1, 0), 1, true, m);
    cyl->transform(Matrix::matrixScale(Vector(3, 10, 2)));
    assertTrue(intersects(cyl, Vector(0, 5, 100), Vector(0, 0, -1)));
    assertTrue(iPoint(cyl, Vector(0, 5, 100), Vector(0, 0, -1)) ==
               Vector(0, 5, 2));
    assertTrue(iNormal(cyl, Vector(0, 5, 100), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(cyl, Vector(1000, 5, 0), Vector(-1, 0, 0)) ==
               Vector(3, 5, 0));
    assertTrue(iNormal(cyl, Vector(1000, 5, 0), Vector(-1, 0, 0)) ==
               Vector(1, 0, 0));

    cyl = new Cylinder(Vector(-5, -10, -5), Vector(5, 10, 3), 10, true, m);
    assertTrue(intersectionCheck(cyl, 1000));
    cyl->transform(Matrix::matrixRotate(Vector(0.1, 1.2, 2.5)));
    assertTrue(intersectionCheck(cyl, 1000));
    cyl->transform(Matrix::matrixScale(Vector(0.5, 0.2, 0.5)));
    assertTrue(intersectionCheck(cyl, 1000));

    cyl = new Cylinder(Vector(-3, -10, -4), Vector(1, 10, 3), 1, true, m);
    cyl->transform(Matrix::matrixScale(Vector(3, 10, 2)));
    assertTrue(normalCheck(cyl, 1000));
    assertTrue(transparentCheck(cyl, 1000));
  }
};

class objectgroup_test : public Test {
public:
  void run() {
    /*
    // Test clone()
    Material* m = new Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30);
    Linesegment segment = Linesegment(Vector(0,0,0),Vector(0,0,100));
    Necklace* g1 = new Necklace(&segment,10,10,m);
    SceneObject* g2 = g1->clone();
    g1->transform(Matrix::matrixTranslate(Vector(0,-100,0)));
    g2->transform(Matrix::matrixTranslate(Vector(0,100,0)));

    KdTree* bsp = new KdTree();
    g1->addSelf(bsp);
    g2->addSelf(bsp);
    bsp->prepare();

    Intersection i;
    Ray r = Ray(Vector(0,100,1000),Vector(0,0,-1),1);
    assertTrue(bsp->intersect(r,i));
    r = Ray(Vector(0,-100,1000),Vector(0,0,-1),1);
    assertTrue(bsp->intersect(r,i));
    r = Ray(Vector(0,0,1000),Vector(0,0,-1),1);
    assertTrue(!bsp->intersect(r,i));
    */
  }
};

class torus_test : public Test {
public:
  void run() {

    Material *m =
        new Material(RGB(1.0, 0.2, 0.2), 0.75, RGB(1.0, 1.0, 1.0), 0.75, 30);
    Torus *t = new Torus(10, 1, m);

    // intersect
    Intersection i;
    assertTrue(intersects(t, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(t, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 11));
    assertTrue(iNormal(t, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));

    Ray ray = Ray(Vector(0, 0, -1000), Vector(0, 0, 1), 1);
    assertTrue(intersects(t, ray));
    assertTrue(iPoint(t, ray) == Vector(0, 0, -11));
    assertTrue(iNormal(t, ray) == Vector(0, 0, -1));

    ray = Ray(Vector(0, 1000, 0), Vector(0, -1, 0), 1); // In middle
    assertTrue(!intersects(t, ray));
    ray = Ray(Vector(0, 1000, 11.1), Vector(0, -1, 0), 1); // Close by
    assertTrue(!intersects(t, ray));
    ray = Ray(Vector(1000, 11.1, 0), Vector(-1, 0, 0), 1); // Close by
    assertTrue(!intersects(t, ray));

    // inside()
    assertFalse(t->inside(Vector(0, 0, 0)));
    assertFalse(t->inside(Vector(0, 0, 8)));
    assertFalse(t->inside(Vector(0, 0, 12)));
    assertFalse(t->inside(Vector(8, 0, 0)));
    assertFalse(t->inside(Vector(12, 0, 0)));
    assertTrue(t->inside(Vector(0, 0, 10)));
    assertTrue(t->inside(Vector(10, 0, 0)));
    assertFalse(t->inside(Vector(10, 0, 10)));
    delete t;

    // Test allIntersections()
    t = new Torus(10, 1, m);
    ray = Ray(Vector(1000, 0, 0), Vector(-1, 0, 0), 1);
    Intersection *all =
        (Intersection *)::alloca(sizeof(Intersection) * t->maxIntersections());
    uint32_t num = t->allIntersections(ray, all);
    assertTrue(num == 4);
    assertTrue(all[0].getPoint() == Vector(11, 0, 0));
    assertTrue(all[0].isEntering() == true);
    assertTrue(all[1].getPoint() == Vector(9, 0, 0));
    assertTrue(all[1].isEntering() == false);
    assertTrue(all[2].getPoint() == Vector(-9, 0, 0));
    assertTrue(all[2].isEntering() == true);
    assertTrue(all[3].getPoint() == Vector(-11, 0, 0));
    assertTrue(all[3].isEntering() == false);

    ray = Ray(Vector(0, 1000, 0), Vector(0, -1, 0), 1);
    num = t->allIntersections(ray, all);
    assertTrue(num == 0);

    // Test scaled torus
    t = new Torus(10, 1, m);
    t->transform(Matrix::matrixScale(Vector(2, 3, 4)));
    assertTrue(iPoint(t, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 44));
    assertTrue(iNormal(t, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(t, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -44));
    assertTrue(iNormal(t, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(t, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(22, 0, 0));
    assertTrue(iNormal(t, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(1, 0, 0));
    assertTrue(iPoint(t, Vector(-1000, 0, 0), Vector(1, 0, 0)) ==
               Vector(-22, 0, 0));
    assertTrue(iNormal(t, Vector(-1000, 0, 0), Vector(1, 0, 0)) ==
               Vector(-1, 0, 0));

    // Test normals
    t = new Torus(10, 1, m);
    t->transform(Matrix::matrixScale(Vector(2, 3, 4)));
    assertTrue(normalCheck(t, 100));
    assertTrue(transparentCheck(t, 100));
  }
};

class transformed_instance_test : public Test {
public:
  void run() {
    Sphere *s = new Sphere(Vector(0, 0, 0), 10.0, NULL);
    TransformedInstance *t1 = new TransformedInstance(s);
    t1->transform(Matrix::matrixTranslate(Vector(100, 0, 0)));
    TransformedInstance *t2 = new TransformedInstance(s);
    t2->transform(Matrix::matrixTranslate(Vector(-100, 0, 0)));
    assertTrue(intersects(t1, Vector(100, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(t1, Vector(0, 0, 1000), Vector(0, 0, -1)));

    assertTrue(intersects(t2, Vector(-100, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(t2, Vector(0, 0, 1000), Vector(0, 0, -1)));
  }
};

class csg_test : public Test {
public:
  void run() {
    ///////////////////////////////////////////////////////////////
    // Union
    ///////////////////////////////////////////////////////////////
    // Ray from outside
    Sphere *s1 = new Sphere(Vector(0, 0, 10), 15, NULL);
    Sphere *s2 = new Sphere(Vector(0, 0, -10), 15, NULL);
    Sphere *s3 = new Sphere(Vector(0, 0, 0), 15, NULL);
    Solid *csg = new CSGUnion(s1, s2, NULL);
    Ray ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    Intersection all[5000];

    uint32_t num = csg->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 25));
    assertTrue(all[1].getPoint() == Vector(0, 0, -25));
    CSGUnion *csg2 = new CSGUnion(csg, s3, NULL);

    num = csg2->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 25));
    assertTrue(all[1].getPoint() == Vector(0, 0, -25));
    Sphere *s4 = new Sphere(Vector(0, 0, -50), 10, NULL);
    CSGUnion *csg3 = new CSGUnion(csg2, s4, NULL);

    num = csg3->allIntersections(ray, all);
    assertTrue(num == 4);
    assertTrue(all[0].getPoint() == Vector(0, 0, 25));
    assertTrue(all[1].getPoint() == Vector(0, 0, -25));
    assertTrue(all[2].getPoint() == Vector(0, 0, -40));
    assertTrue(all[3].getPoint() == Vector(0, 0, -60));

    // Ray from inside
    ray = Ray(Vector(0, 0, 0), Vector(0, 0, -1), -1);
    num = csg3->allIntersections(ray, all);
    assertTrue(num == 3);
    assertTrue(all[0].getPoint() == Vector(0, 0, -25));
    assertTrue(all[1].getPoint() == Vector(0, 0, -40));
    assertTrue(all[2].getPoint() == Vector(0, 0, -60));

    assertTrue(normalCheck(csg3, 1000));
    assertTrue(transparentCheck(csg3, 1000));

    s1 = new Sphere(Vector(0, 0, 10), 5, NULL);
    s2 = new Sphere(Vector(0, 0, -10), 5, NULL);
    csg = new CSGUnion(s1, s2, NULL);

    assertTrue(iPoint(csg, Vector(0, 0, 0), Vector(0, 0, 1)) ==
               Vector(0, 0, 5));
    assertTrue(iNormal(csg, Vector(0, 0, 0), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 0), Vector(0, 0, -1)) ==
               Vector(0, 0, -5));
    assertTrue(iNormal(csg, Vector(0, 0, 0), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));

    s1 = new Sphere(Vector(0, 0, 10), 15, NULL);
    s2 = new Sphere(Vector(0, 0, -10), 15, NULL);
    csg = new CSGUnion(s1, s2, NULL);
    assertTrue(iPoint(csg, Vector(0, 0, 25), Vector(0, 0, -1)) ==
               Vector(0, 0, -25));
    assertTrue(iNormal(csg, Vector(0, 0, 25), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 5), Vector(0, 0, -1)) ==
               Vector(0, 0, -25));
    assertTrue(iNormal(csg, Vector(0, 0, 5), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, -25), Vector(0, 0, 1)) ==
               Vector(0, 0, 25));
    assertTrue(iNormal(csg, Vector(0, 0, -25), Vector(0, 0, 1)) ==
               Vector(0, 0, 1));

    // Testing inside()
    assertTrue(csg->inside(Vector(0, 0, 0)));
    assertTrue(csg->inside(Vector(0, 0, 10)));
    assertTrue(csg->inside(Vector(0, 0, -10)));
    assertFalse(csg->inside(Vector(0, 0, -26)));
    assertFalse(csg->inside(Vector(0, 0, 26)));

    // Testing the constructor taking a vector<Solid*>
    vector<Solid *> solids;

    solids.push_back(s1);
    solids.push_back(s2);
    csg = new CSGUnion(&solids, NULL);
    assertTrue(iPoint(csg, Vector(0, 0, 25), Vector(0, 0, -1)) ==
               Vector(0, 0, -25));
    assertTrue(iNormal(csg, Vector(0, 0, 25), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 5), Vector(0, 0, -1)) ==
               Vector(0, 0, -25));
    assertTrue(iNormal(csg, Vector(0, 0, 5), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, -25), Vector(0, 0, 1)) ==
               Vector(0, 0, 25));
    assertTrue(iNormal(csg, Vector(0, 0, -25), Vector(0, 0, 1)) ==
               Vector(0, 0, 1));

    solids.clear();
    // A group of ball that all intersect
    for (int i = 0; i <= 100; i++) {
      solids.push_back(new Sphere(Vector(0, 0, i), 2, NULL));
    }
    csg = new CSGUnion(&solids, NULL);
    assertTrue(iPoint(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 102));
    assertTrue(iNormal(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -2));
    assertTrue(iNormal(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(intersects(csg, Vector(0, 0, 50), Vector(0, 0, -1)));

    assertTrue(iPoint(csg, Vector(0, 0, 50), Vector(0, 0, 1)) ==
               Vector(0, 0, 102));
    assertTrue(iNormal(csg, Vector(0, 0, 50), Vector(0, 0, 1)) ==
               Vector(0, 0, 1));

    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 102));
    assertTrue(all[1].getPoint() == Vector(0, 0, -2));

    solids.clear();
    // A group that doesn't intersect
    for (int i = 0; i <= 100; i++) {
      solids.push_back(new Sphere(Vector(0, 0, i), 0.25, NULL));
    }
    csg = new CSGUnion(&solids, NULL);
    assertTrue(iPoint(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 100.25));
    assertTrue(iNormal(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -0.25));
    assertTrue(iNormal(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 2 * 101);
    assertTrue(all[0].getPoint() == Vector(0, 0, 100.25));
    assertTrue(all[1].getPoint() == Vector(0, 0, 99.75));

    ///////////////////////////////////////////////////////////////
    // Intersection
    ///////////////////////////////////////////////////////////////
    s1 = new Sphere(Vector(0, 0, 10), 15, NULL);
    s2 = new Sphere(Vector(0, 0, -10), 15, NULL);
    s3 = new Sphere(Vector(0, 0, 0), 15, NULL);
    csg = new CSGIntersection(s1, s3, NULL);

    assertTrue(normalCheck(csg, 1000));
    assertTrue(transparentCheck(csg, 1000));

    ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 15));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getPoint() == Vector(0, 0, -5));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 15));
    assertTrue(iNormal(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -5));
    assertTrue(iNormal(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));

    // Ray that misses
    ray = Ray(Vector(0, 1000, 20), Vector(0, -1, 0), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 0);
    assertTrue(!intersects(csg, Vector(0, 1000, 20), Vector(0, -1, 0)));
    assertTrue(intersects(csg, Vector(0, 1000, 14), Vector(0, -1, 0)));

    // Ray that misses
    ray = Ray(Vector(0, 1000, -10), Vector(0, -1, 0), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 0);
    assertTrue(!intersects(csg, Vector(0, 1000, -10), Vector(0, -1, 0)));
    assertTrue(intersects(csg, Vector(0, 1000, -3), Vector(0, -1, 0)));

    // inside()
    assertFalse(csg->inside(Vector(0, 0, 16)));
    assertTrue(csg->inside(Vector(0, 0, 14)));
    assertTrue(csg->inside(Vector(0, 0, -4)));
    assertFalse(csg->inside(Vector(0, 0, -6)));

    // Void intersection
    s1 = new Sphere(Vector(0, 0, 10), 5, NULL);
    s2 = new Sphere(Vector(0, 0, -10), 5, NULL);
    csg = new CSGIntersection(s1, s2, NULL);
    assertTrue(!intersects(csg, Vector(0, 0, 1000), Vector(0, 0, -1)));
    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    csg->allIntersections(ray, all);
    assertTrue(num == 0);

    // inside()
    assertFalse(csg->inside(Vector(0, 0, 0)));
    assertFalse(csg->inside(Vector(0, 0, 10)));
    assertFalse(csg->inside(Vector(0, 0, -10)));
    assertFalse(csg->inside(Vector(0, 0, -26)));
    assertFalse(csg->inside(Vector(0, 0, 26)));

    ///////////////////////////////////////////////////////////////
    // Difference
    ///////////////////////////////////////////////////////////////
    s1 = new Sphere(Vector(0, 0, 10), 15, NULL);
    s2 = new Sphere(Vector(0, 0, 0), 15, NULL);
    csg = new CSGDifference(s1, s2, NULL);
    ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 25));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getPoint() == Vector(0, 0, 15));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 25));
    assertTrue(iNormal(csg, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, 15));
    assertTrue(iNormal(csg, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(!intersects(csg, Vector(0, 0, 25.1), Vector(0, 0, 1)));
    assertTrue(!intersects(csg, Vector(0, 0, 25), Vector(0, 0, 1)));
    assertTrue(!intersects(csg, Vector(0, 0, 14), Vector(0, 0, -1)));
    assertTrue(!intersects(csg, Vector(0, 0, 14.99), Vector(0, 0, -1)));
    assertTrue(!intersects(csg, Vector(0, 0, 15), Vector(0, 0, -1)));

    // inside()
    assertFalse(csg->inside(Vector(0, 0, 26)));
    assertTrue(csg->inside(Vector(0, 0, 24)));
    assertTrue(csg->inside(Vector(0, 0, 16)));
    assertFalse(csg->inside(Vector(0, 0, 14)));
    assertFalse(csg->inside(Vector(0, 0, 0)));

    // Test a sphere with three other spheres subtracted from its middle,
    // front and back, so that the resulting object is hollow along the z-axis.

    s1 = new Sphere(Vector(0, 0, 0), 200.0, NULL);
    s2 = new Sphere(Vector(0, 0, 0), 180.0, NULL);
    Solid *s = new CSGDifference(s1, s2, NULL); // Make it hollow
    s3 = new Sphere(Vector(0, 0, 200), 100.0, NULL);
    Solid *b4 = new CSGDifference(s, s3, NULL); // Cut front
    s4 = new Sphere(Vector(0, 0, -200), 100.0, NULL);
    Solid *b5 = new CSGDifference(b4, s4, NULL); // Cut back

    Ray r = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), 1);
    assertTrue(intersects(s, r));
    assertTrue(intersects(s3, r));
    assertTrue(intersects(s4, r));
    assertTrue(intersects(b4, r));
    r = Ray(Vector(0, 0, -1000), Vector(0, 0, 1), 1);
    assertTrue(!intersects(b5, r));
    r = Ray(Vector(0, 0, 0), Vector(0, 0, 1), 1);
    assertTrue(!intersects(b5, r));

    assertTrue(normalCheck(b5, 1000));
    assertTrue(transparentCheck(b5, 1000));

    // Test a hollow sphere
    s1 = new Sphere(Vector(0, 0, 0), 30, NULL);
    s2 = new Sphere(Vector(0, 0, 0), 29, NULL);
    csg = new CSGDifference(s1, s2, NULL); // Make it hollow
    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 4);
    assertTrue(all[0].getPoint() == Vector(0, 0, 30));
    assertTrue(all[1].getPoint() == Vector(0, 0, 29));
    assertTrue(all[2].getPoint() == Vector(0, 0, -29));
    assertTrue(all[3].getPoint() == Vector(0, 0, -30));
    assertTrue(all[0].isEntering() == true);
    assertTrue(all[1].isEntering() == false);
    assertTrue(all[2].isEntering() == true);
    assertTrue(all[3].isEntering() == false);
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(all[2].getNormal() == Vector(0, 0, 1));
    assertTrue(all[3].getNormal() == Vector(0, 0, -1));

    // Test a hollow ellipsoid
    Ellipsoid *e1 = new Ellipsoid(Vector(0, 0, 0), Vector(10, 20, 30), NULL);
    Ellipsoid *e2 = new Ellipsoid(Vector(0, 0, 0), Vector(9, 19, 29), NULL);
    csg = new CSGDifference(e1, e2, NULL);
    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    num = csg->allIntersections(ray, all);
    assertTrue(num == 4);
    assertTrue(all[0].getPoint() == Vector(0, 0, 30));
    assertTrue(all[1].getPoint() == Vector(0, 0, 29));
    assertTrue(all[2].getPoint() == Vector(0, 0, -29));
    assertTrue(all[3].getPoint() == Vector(0, 0, -30));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(all[2].getNormal() == Vector(0, 0, 1));
    assertTrue(all[3].getNormal() == Vector(0, 0, -1));
    assertTrue(all[0].isEntering() == true);
    assertTrue(all[1].isEntering() == false);
    assertTrue(all[2].isEntering() == true);
    assertTrue(all[3].isEntering() == false);
    // Rays with origin on edges
    assertTrue(iPoint(csg, Vector(0, 0, 30), Vector(0, 0, -1)) ==
               Vector(0, 0, 29));
    assertTrue(iNormal(csg, Vector(0, 0, 30), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(csg, Vector(0, 0, 29), Vector(0, 0, -1)) ==
               Vector(0, 0, -29));
    assertTrue(iNormal(csg, Vector(0, 0, 29), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(csg, Vector(0, 0, -29), Vector(0, 0, -1)) ==
               Vector(0, 0, -30));
    assertTrue(iNormal(csg, Vector(0, 0, -29), Vector(0, 0, -1)) ==
               Vector(0, 0, -1));
    assertTrue(!intersects(csg, Vector(0, 0, -30), Vector(0, 0, -1)));

    // Test a hollow ellipsoid with top cut off
    SolidBox *box =
        new SolidBox(Vector(-100, 5, -100), Vector(100, 50, 100), NULL);
    Solid *cup = new CSGDifference(csg, box, NULL);
    ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    num = cup->allIntersections(ray, all);
    assertTrue(num == 4);
    assertTrue(all[0].getPoint() == Vector(0, 0, 30));
    assertTrue(all[1].getPoint() == Vector(0, 0, 29));
    assertTrue(all[2].getPoint() == Vector(0, 0, -29));
    assertTrue(all[3].getPoint() == Vector(0, 0, -30));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(all[2].getNormal() == Vector(0, 0, 1));
    assertTrue(all[3].getNormal() == Vector(0, 0, -1));
    ray = Ray(Vector(0, 1000, 0), Vector(0, -1, 0), -1);

    num = cup->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, -19, 0));
    assertTrue(all[1].getPoint() == Vector(0, -20, 0));
    assertTrue(all[0].getNormal() == Vector(0, 1, 0));
    assertTrue(all[1].getNormal() == Vector(0, -1, 0));
    assertTrue(iPoint(cup, Vector(0, 200, 0), Vector(0, -1, 0)) ==
               Vector(0, -19, 0));
    assertTrue(iNormal(cup, Vector(0, 200, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iPoint(cup, Vector(0, -19, 0), Vector(0, -1, 0)) ==
               Vector(0, -20, 0));
    assertTrue(iNormal(cup, Vector(0, -19, 0), Vector(0, -1, 0)) ==
               Vector(0, -1, 0));
    assertTrue(!intersects(csg, Vector(0, -20, 0), Vector(0, -1, 0)));

    assertTrue(normalCheck(csg, 1000));
    assertTrue(transparentCheck(csg, 1000));

    // Test boundingbox shrinkage with CSG difference
    s1 = new Sphere(Vector(0, 0, 0), 800, NULL);
    s2 = new Sphere(Vector(1000, 0, 0), 990, NULL);
    csg = new CSGDifference(s1, s1, NULL);

    assertTrue(s2->getBoundingBox().area() > 10 + s2->getContainedBox().area());
    assertTrue(s2->getContainedBox().area() > 10);

    assertTrue(csg->getBoundingBox().area() < 10 + s1->getBoundingBox().area());
  }
};

class solidbox_test : public Test {
public:
  void run() {
    Ray ray;
    Intersection all[100];

    SolidBox *b = new SolidBox(Vector(-10, -10, -10), Vector(10, 10, 10), NULL);
    assertTrue(intersects(b, Vector(20, 0, 0), Vector(-1, 0, 0)));
    assertTrue(iPoint(b, Vector(20, 0, 0), Vector(-1, 0, 0)) ==
               Vector(10, 0, 0));
    assertTrue(iNormal(b, Vector(20, 0, 0), Vector(-1, 0, 0)) ==
               Vector(1, 0, 0));
    assertTrue(iPoint(b, Vector(-20, 0, 0), Vector(1, 0, 0)) ==
               Vector(-10, 0, 0));
    assertTrue(iNormal(b, Vector(-20, 0, 0), Vector(1, 0, 0)) ==
               Vector(-1, 0, 0));
    assertTrue(iPoint(b, Vector(0, 0, 0), Vector(1, 0, 0)) == Vector(10, 0, 0));
    assertTrue(iNormal(b, Vector(0, 0, 0), Vector(1, 0, 0)) == Vector(1, 0, 0));

    // Ray from outside
    ray = Ray(Vector(0, 0, 100), Vector(0, 0, -1), -1);
    uint32_t num = b->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 10));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[0].isEntering());
    assertTrue(all[1].getPoint() == Vector(0, 0, -10));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(!all[1].isEntering());

    // Ray from inside
    ray = Ray(Vector(0, 0, 0), Vector(0, -1, 0), -1);
    num = b->allIntersections(ray, all);
    assertTrue(num == 1);
    assertTrue(all[0].getPoint() == Vector(0, -10, 0));
    assertTrue(all[0].getNormal() == Vector(0, -1, 0));
    assertTrue(!all[0].isEntering());

    // Ray that misses
    ray = Ray(Vector(0, 0, 20), Vector(0, -1, 0), -1);
    num = b->allIntersections(ray, all);
    assertTrue(num == 0);

    // Outwards ray with origin on edge of box
    ray = Ray(Vector(0, 0, 10), Vector(0, 0, 1), -1);
    num = b->allIntersections(ray, all);
    assertTrue(num == 0);
    assertTrue(intersects(b, ray) == false);

    // Inwards ray with origin on edge of box
    ray = Ray(Vector(0, 0, 10), Vector(0, 0, -1), -1);
    num = b->allIntersections(ray, all);
    assertTrue(num == 1);
    assertTrue(all[0].getPoint() == Vector(0, 0, -10));
    assertTrue(all[0].getNormal() == Vector(0, 0, -1));
    assertTrue(!all[0].isEntering());
    assertTrue(intersects(b, ray) == true);

    // Scaled box
    b = new SolidBox(Vector(-1, -1, -1), Vector(1, 1, 1), NULL);
    b->transform(Matrix::matrixScale(Vector(10, 20, 30)));
    assertTrue(intersects(b, Vector(9, 19, 100), Vector(0, 0, -1)));
    assertTrue(iPoint(b, Vector(9, 19, 100), Vector(0, 0, -1)) ==
               Vector(9, 19, 30));
    assertTrue(iNormal(b, Vector(9, 19, 100), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(intersects(b, Vector(-9, -19, -100), Vector(0, 0, 1)));
    assertTrue(iPoint(b, Vector(-9, -19, -100), Vector(0, 0, 1)) ==
               Vector(-9, -19, -30));
    assertTrue(iNormal(b, Vector(-9, -19, -100), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));

    // Solidbox in CSG difference
    SolidBox *b1 =
        new SolidBox(Vector(-20, 350, -20), Vector(20, 400, 20), NULL);
    SolidBox *b2 =
        new SolidBox(Vector(-15, 395, -15), Vector(15, 405, 15), NULL);
    Solid *csg = new CSGDifference(b1, b2, NULL);
    assertTrue(iPoint(csg, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 395, 0));
    assertTrue(iNormal(csg, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));

    b = new SolidBox(Vector(-10, -20, -30), Vector(40, 50, 60), NULL);
    assertTrue(normalCheck(b, 200));
    assertTrue(transparentCheck(b, 200));
  }
};

class ellipsoid_test : public Test {
public:
  void run() {
    Ellipsoid *e = new Ellipsoid(Vector(0, 0, 0), Vector(10, 20, 30), NULL);
    assertTrue(intersects(e, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(intersects(e, Vector(0, 0, 0), Vector(0, 0, -1)));
    assertTrue(intersects(e, Vector(0, 0, 0), Vector(0, 0, 1)));
    assertTrue(iPoint(e, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 30));
    assertTrue(iNormal(e, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(e, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 20, 0));
    assertTrue(iNormal(e, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iPoint(e, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(10, 0, 0));
    assertTrue(iNormal(e, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(1, 0, 0));

    assertTrue(e->inside(Vector(2, -5, -1)));
    assertTrue(e->inside(Vector(0, 0, 25)));
    assertFalse(e->inside(Vector(0, 0, 35)));
    assertTrue(e->inside(Vector(9, 0, 0)));
    assertFalse(e->inside(Vector(11, 0, 0)));
    assertTrue(e->inside(Vector(0, -19, 0)));
    assertFalse(e->inside(Vector(0, -21, 0)));

    e = new Ellipsoid(Vector(0, 0, 0), Vector(2, 5, 6), NULL);
    e->transform(Matrix::matrixScale(Vector(5, 4, 5)));
    assertTrue(intersects(e, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(e, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 30));
    assertTrue(iNormal(e, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(e, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 20, 0));
    assertTrue(iNormal(e, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iPoint(e, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(10, 0, 0));
    assertTrue(iNormal(e, Vector(1000, 0, 0), Vector(-1, 0, 0)) ==
               Vector(1, 0, 0));

    // Translated
    e = new Ellipsoid(Vector(0, 0, 0), Vector(10, 20, 30), NULL);
    e->transform(Matrix::matrixTranslate(Vector(100, 200, 300)));
    assertTrue(e->inside(Vector(100, 200, 300)));
    assertFalse(e->inside(Vector(111, 200, 300)));
    assertTrue(e->inside(Vector(100, 200, 329)));
    assertFalse(e->inside(Vector(100, 200, 331)));

    // Scaled and translated
    e = new Ellipsoid(Vector(0, 0, 0), Vector(10, 20, 30), NULL);
    e->transform(Matrix::matrixScale(Vector(2, 3, 4)));
    e->transform(Matrix::matrixTranslate(Vector(30, 20, 10)));
    assertTrue(intersects(e, Vector(30, 20, 10), Vector(0, 0, -1)));
    assertTrue(intersects(e, Vector(30, 20, 10), Vector(0, 0, 1)));
    assertTrue(e->inside(Vector(49, 20, 10)));
    assertFalse(e->inside(Vector(51, 20, 10)));
    assertTrue(e->inside(Vector(30, 20, 129)));
    assertFalse(e->inside(Vector(30, 20, 131)));

    // All intersections
    e = new Ellipsoid(Vector(0, 0, 0), Vector(2, 5, 6), NULL);
    e->transform(Matrix::matrixScale(Vector(5, 4, 5)));
    Ray ray = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), -1);
    Intersection *all =
        (Intersection *)::alloca(sizeof(Intersection) * e->maxIntersections());
    uint32_t num = e->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 30));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[1].getPoint() == Vector(0, 0, -30));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(all[0].isEntering());
    assertTrue(!all[1].isEntering());

    e = new Ellipsoid(Vector(0, 0, 0), Vector(10, 20, 30), NULL);
    assertTrue(normalCheck(e, 100));
    assertTrue(transparentCheck(e, 100));
  }
};

class cone_test : public Test {
public:
  void run() {
    Ray ray;
    Intersection all[100];
    Cone *c;
    Vector n;

    // Cone along z-axis
    c = new Cone(Vector(0, 0, 0), Vector(0, 0, 1), 1, 0, true, NULL);
    assertTrue(intersects(c, Vector(0, 1000, 0.5), Vector(0, -1, 0)));
    assertTrue(iPoint(c, Vector(0, 1000, 0.5), Vector(0, -1, 0)) ==
               Vector(0, 0.5, 0.5));
    n = Vector(0, 1, 1);
    n.normalize();
    assertTrue(iNormal(c, Vector(0, 1000, 0.5), Vector(0, -1, 0)) == n);
    assertTrue(!intersects(c, Vector(1, 1000, 0.5), Vector(0, -1, 0)));
    assertTrue(!intersects(c, Vector(0, 1000, -0.1), Vector(0, -1, 0)));
    assertTrue(!intersects(c, Vector(0, 1000, 1.1), Vector(0, -1, 0)));

    c = new Cone(Vector(0, 0, 0), Vector(0, 0, 1), 4, 2, true, NULL);
    assertTrue(intersects(c, Vector(0, 1000, 0.5), Vector(0, -1, 0)));
    assertTrue(iPoint(c, Vector(0, 1000, 0.5), Vector(0, -1, 0)) ==
               Vector(0, 3, 0.5));
    assertFalse(c->inside(Vector(0, 0, -1)));
    assertFalse(c->inside(Vector(0, 0, 2)));
    assertFalse(c->inside(Vector(0, 3, 1)));
    assertTrue(c->inside(Vector(0, 0, 0.5)));
    assertTrue(c->inside(Vector(1, 1, 0.5)));

    c = new Cone(Vector(0, 0, -1), Vector(0, 0, 1), 4, 2, true, NULL);
    assertTrue(intersects(c, Vector(0, 1000, 0), Vector(0, -1, 0)));
    assertTrue(iPoint(c, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 3, 0));
    assertTrue(intersects(c, Vector(0, 1000, -0.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0, 1000, 0.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0.01, 1000, -0.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0.01, 1000, 0.9), Vector(0, -1, 0)));

    c = new Cone(Vector(0, 0, 0), Vector(0, 0, 10), 500, 10, true, NULL);
    assertTrue(intersects(c, Vector(0, 1000, 0.1), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0, 1000, 9.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0.01, 1000, 9.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0.01, 1000, 9.9), Vector(0, -1, 0)));
    assertTrue(intersects(c, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(c, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 10));
    assertTrue(iNormal(c, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(c, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, 0));
    assertTrue(iNormal(c, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));

    // Cone along y-axis
    c = new Cone(Vector(0, 0, 0), Vector(0, 1, 0), 2, 4, true, NULL);
    assertTrue(intersects(c, Vector(0, 0.5, 100), Vector(0, 0, -1)));
    assertTrue(iPoint(c, Vector(0, 0.5, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0.5, 3));
    assertTrue(c->inside(Vector(0, 0.5, 0)));
    assertTrue(c->inside(Vector(1, 0.5, 1)));
    assertFalse(c->inside(Vector(0, -1, 0)));
    assertFalse(c->inside(Vector(0, 2, 0)));
    assertFalse(c->inside(Vector(0, 0.2, 3)));

    c = new Cone(Vector(0, 0, 0), Vector(0, 10, 0), 500, 10, true, NULL);
    assertTrue(intersects(c, Vector(1000, 0.1, 0), Vector(-1, 0, 0)));
    assertTrue(intersects(c, Vector(1000, 9.9, 0), Vector(-1, 0, 0)));
    assertTrue(intersects(c, Vector(1000, 9.9, 10), Vector(-1, 0, 0)));
    assertTrue(intersects(c, Vector(1000, 0.1, 400), Vector(-1, 0, 0)));
    assertTrue(!intersects(c, Vector(1000, 0.1, 500), Vector(-1, 0, 0)));
    assertTrue(iPoint(c, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 10, 0));
    assertTrue(iNormal(c, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iPoint(c, Vector(0, -1000, 0), Vector(0, 1, 0)) ==
               Vector(0, 0, 0));
    assertTrue(iNormal(c, Vector(0, -1000, 0), Vector(0, 1, 0)) ==
               Vector(0, -1, 0));

    // Cone along y-axis
    c = new Cone(Vector(0, -1, 0), Vector(0, 1, 0), 2, 4, true, NULL);
    assertTrue(intersects(c, Vector(0, 0, 100), Vector(0, 0, -1)));
    assertTrue(iPoint(c, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 3));

    // Cone along x-axis
    c = new Cone(Vector(-1, 0, 0), Vector(1, 0, 0), 2, 4, true, NULL);
    assertTrue(intersects(c, Vector(0, 1000, 0), Vector(0, -1, 0)));
    assertTrue(iPoint(c, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 3, 0));

    // Intersection with caps (cone along z-axis)
    c = new Cone(Vector(0, 0, 0), Vector(0, 0, 1), 1, 0, true, NULL);
    assertTrue(iPoint(c, Vector(0, 0, -100), Vector(0, 0, 1)) ==
               Vector(0, 0, 0));
    c = new Cone(Vector(0, 0, -1), Vector(0, 0, 1), 2, 1, true, NULL);
    assertTrue(iPoint(c, Vector(0, 0, -100), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iPoint(c, Vector(0, 0, 100), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));

    // All intersections with caps (cone along z-axis)
    c = new Cone(Vector(0, 0, -2), Vector(0, 0, 3), 2, 1, true, NULL);
    ray = Ray(Vector(0, 0, 10), Vector(0, 0, -1), -1);
    uint32_t num = c->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(0, 0, 3));
    assertTrue(all[0].getNormal() == Vector(0, 0, 1));
    assertTrue(all[0].isEntering());
    assertTrue(all[1].getPoint() == Vector(0, 0, -2));
    assertTrue(all[1].getNormal() == Vector(0, 0, -1));
    assertTrue(!all[1].isEntering());

    // All intersections with caps (cone along x-axis)
    c = new Cone(Vector(-2, 0, 0), Vector(3, 0, 0), 2, 1, true, NULL);
    ray = Ray(Vector(-10, 0, 0), Vector(1, 0, 0), -1);
    num = c->allIntersections(ray, all);
    assertTrue(num == 2);
    assertTrue(all[0].getPoint() == Vector(-2, 0, 0));
    assertTrue(all[0].getNormal() == Vector(-1, 0, 0));
    assertTrue(all[0].isEntering());
    assertTrue(all[1].getPoint() == Vector(3, 0, 0));
    assertTrue(all[1].getNormal() == Vector(1, 0, 0));
    assertTrue(!all[1].isEntering());

    // Checking normals
    c = new Cone(Vector(0, 0, 0), Vector(0, 0, 10), 500, 10, true, NULL);
    assertTrue(normalCheck(c, 1000));
    assertTrue(transparentCheck(c, 1000));
  }
};

class superellipsoid_test : public Test {
public:
  void run() {
    // Unscaled instance at origin
    SuperEllipsoid *s = new SuperEllipsoid(0.2, 0.2, 100, 0.0001, NULL);
    assertTrue(intersects(s, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(2, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(0, 2, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() < 1.001);
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() > 0.999);
    assertTrue(iNormal(s, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iNormal(s, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iNormal(s, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iNormal(s, Vector(0, -1000, 0), Vector(0, 1, 0)) ==
               Vector(0, -1, 0));

    // Rays starting inside object
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, 1)));
    assertTrue(intersects(s, Vector(0.1, 0.2, 0.3), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(0.1, 0.2, 0.3), Vector(0, 0, 1)));
    delete s;

    // Translated instance
    s = new SuperEllipsoid(0.2, 0.2, 100, 0.0001, NULL);
    s->transform(Matrix::matrixTranslate(Vector(100, 300, 400)));
    assertTrue(intersects(s, Vector(100, 300, 400), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(100, 300, 400), Vector(0, 0, 1)));
    delete s;

    // Scaled instance at origin
    s = new SuperEllipsoid(0.2, 0.2, 100, 0.0001, NULL);
    s->transform(Matrix::matrixScale(Vector(10, 10, 10)));
    assertTrue(intersects(s, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(9, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(11, 0, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() < 10.001);
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() > 9.999);
    assertTrue(iNormal(s, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iNormal(s, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iNormal(s, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, 1)));
    assertTrue(intersects(s, Vector(1, -2, 3), Vector(0, 1, 0)));
    assertTrue(intersects(s, Vector(1, -2, 3), Vector(0, -1, 0)));
    delete s;

    // Scaled and translated instance
    s = new SuperEllipsoid(0.2, 0.2, 100, 0.0001, NULL);
    s->transform(Matrix::matrixScale(Vector(10, 10, 10)));
    s->transform(Matrix::matrixTranslate(Vector(100, 300, 400)));
    assertTrue(intersects(s, Vector(100, 300, 1000), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(109, 300, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(111, 300, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(111, 300, -1000), Vector(0, 0, 1)));
    assertTrue(!intersects(s, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(0, 0, -1000), Vector(0, 0, 1)));
    assertTrue(iPoint(s, Vector(100, 300, 1000), Vector(0, 0, -1)).z() <
               410.01);
    assertTrue(iPoint(s, Vector(100, 300, 1000), Vector(0, 0, -1)).z() >
               409.99);
    assertTrue(iNormal(s, Vector(100, 300, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iPoint(s, Vector(100, 300, -1000), Vector(0, 0, 1)).z() <
               390.01);
    assertTrue(iPoint(s, Vector(100, 300, -1000), Vector(0, 0, 1)).z() >
               389.99);
    assertTrue(iNormal(s, Vector(100, 300, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    // Rays from the inside
    assertTrue(intersects(s, Vector(100, 300, 400), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(100, 300, 401), Vector(0, 0, 1)));
    assertTrue(intersects(s, Vector(101, 300, 401), Vector(0, -1, 0)));
    assertTrue(intersects(s, Vector(101, 300, 401), Vector(0, 1, 0)));
    //   assertTrue(intersects(s,Vector(100,300,399),Vector(0,0,-1)));
    //   assertTrue(intersects(s,Vector(100,300,401),Vector(0,0,-1)));
    delete s;

    // Test a superellipsoid with other n1 and n2 values
    s = new SuperEllipsoid(0.2, 3.0, 100, 0.0001, NULL);
    assertTrue(intersects(s, Vector(0, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(2, 0, 1000), Vector(0, 0, -1)));
    assertTrue(!intersects(s, Vector(0, 2, 1000), Vector(0, 0, -1)));
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() < 1.001);
    assertTrue(iPoint(s, Vector(0, 0, 1000), Vector(0, 0, -1)).z() > 0.999);
    assertTrue(iNormal(s, Vector(0, 0, 1000), Vector(0, 0, -1)) ==
               Vector(0, 0, 1));
    assertTrue(iNormal(s, Vector(0, 0, -1000), Vector(0, 0, 1)) ==
               Vector(0, 0, -1));
    assertTrue(iNormal(s, Vector(0, 1000, 0), Vector(0, -1, 0)) ==
               Vector(0, 1, 0));
    assertTrue(iNormal(s, Vector(0, -1000, 0), Vector(0, 1, 0)) ==
               Vector(0, -1, 0));
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, -1)));
    assertTrue(intersects(s, Vector(0, 0, 0), Vector(0, 0, 1)));
    delete s;

    // Check normals
    s = new SuperEllipsoid(0.2, 0.2, 100, 0.0001, NULL);
    s->transform(Matrix::matrixScale(Vector(10, 30, 40)));
    assertTrue(normalCheck(s, 1000));
    assertTrue(transparentCheck(s, 1000));
  }
};

class bounded_mesh_test : public Test {
public:
  void run() {
    // Basic intersections
    Tessalation *t = new Tessalation(Vector(0, 0, 0), 100, 5, NULL);
    Bound *b = new Bound(t);
    assertTrue(intersects(b, Vector(1, 0, 1000), Vector(0, 0, -1)));
    assertTrue(intersects(b, Vector(0, 1, -1000), Vector(0, 0, 1)));
    assertTrue(intersects(b, Vector(1000, 1, 0), Vector(-1, 0, 0)));
    assertTrue(intersects(b, Vector(-1000, 1, 1), Vector(1, 0, 0)));
    assertTrue(intersects(b, Vector(0, 1000, 1), Vector(0, -1, 0)));
    assertTrue(intersects(b, Vector(0, -1000, 1), Vector(0, 1, 0)));

    // Added to a Kd-tree.
    KdTree *bsp = new KdTree();
    b->addSelf(bsp);
    bsp->prepare();
    Intersection inter;
    Ray r = Ray(Vector(0, 0, 1000), Vector(0, 0, -1), 1);
    assertTrue(bsp->intersect(r, inter));
  }
};

class heightfield_test : public Test {
public:
  void run() {
    Image *img = Image::load(getLoadPrefix() + "/gfx/water.tga");
    Texture *texture =
        new SimpleTexture(img, Vector2(2, 2), Texture::INTERPOLATION_BILINEAR);
    HeightField *h = new HeightField(texture, 10, 200, 200, 10, 10, NULL);
    h->prepare();

    KdTree *bsp = new KdTree();
    h->addSelf(bsp);
    bsp->prepare();
    Intersection inter;
    Ray ray;
    ray = Ray(Vector(0, 1000, 0), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(ray, inter));

    ray = Ray(Vector(95, 1000, 95), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(ray, inter));
    ray = Ray(Vector(-95, 1000, -95), Vector(0, -1, 0), 1);
    assertTrue(bsp->intersect(ray, inter));

    ray = Ray(Vector(105, 1000, 0), Vector(0, -1, 0), 1);
    assertFalse(bsp->intersect(ray, inter));

    // Fire lots of rays that should hit
    for (int i = 0; i < 3000; i++) {
      double r1 = RANDOM(-99, 99);
      double r2 = RANDOM(30, 4000);
      double r3 = RANDOM(-99, 99);

      ray = Ray(Vector(r1, r2, r3), Vector(0, -1, 0), 1);
      assertTrue(bsp->intersect(ray, inter));
    }

    // Fire lots of rays that should miss
    int fired = 0;
    while (fired < 3000) {
      double r1 = RANDOM(-300, 300);
      double r2 = RANDOM(30, 4000);
      double r3 = RANDOM(-300, 300);

      if (abs(r1) > 100.1 && abs(r3) < 100.1) {
        ray = Ray(Vector(r1, r2, r3), Vector(0, -1, 0), 1);
        assertFalse(bsp->intersect(ray, inter));
        fired++;
      }
    }
  }
};

int main(int argc, char *argv[]) {

  TestSuite suite;

  suite.add("Ray", new ray_test());
  suite.add("Sphere", new sphere_test());
  suite.add("Halfspace", new halfspace_test());
  suite.add("Cylinder", new cylinder_test());
  suite.add("Torus", new torus_test());
  suite.add("Solid box", new solidbox_test());
  suite.add("Ellipsoid", new ellipsoid_test());
  suite.add("Cone", new cone_test());
  suite.add("Box", new box_test());
  suite.add("Object group", new objectgroup_test());
  suite.add("Superellipsoid", new superellipsoid_test());
  suite.add("CSG", new csg_test());
  suite.add("Tetrahedron", new tetrahedron_test());
  suite.add("Tessalation", new tesselation_test());
  suite.add("Extrusion", new extrusion_test());
  suite.add("Mesh", new mesh_test());
  suite.add("Heightfield", new heightfield_test());
  suite.add("Transformed instance", new transformed_instance_test());
  suite.add("Bounded mesh", new bounded_mesh_test());
  suite.run();
  suite.printStatus();
  if (suite.hasFailures()) {
    return EXIT_FAILURE;
  } else {
    return EXIT_SUCCESS;
  }
}
