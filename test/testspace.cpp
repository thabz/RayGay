#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>

#include "stats.h"
#include "boundingbox.h"
#include "intersection.h"
#include "ray.h"
#include "space/kdtree.h"
#include "objects/sphere.h"
#include "math/vector2.h"
#include "materials/material.h"
#include "testing.h"

class boundingbox_test : public Test {

    public: 
	void run() {


	    BoundingBox b;
	    ////////////////////////////////////////////////////////
	    // Test normal() 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.normal(Vector(1,0,0))[0] = 1);
	    assertTrue(b.normal(Vector(-1,0,0))[0] = -1);
	    assertTrue(b.normal(Vector(0,1,0))[1] = 1);
	    assertTrue(b.normal(Vector(0,-1,0))[1] = -1);
	    assertTrue(b.normal(Vector(0.5,0.2,1))[2] = 1);
	    assertTrue(b.normal(Vector(0,0,-1))[2] = -1);

	    b = BoundingBox(Vector(-100,-10,-100),Vector(100,10,100));
	    assertTrue(b.normal(Vector(-100,0,-100))[0] = -1);

	    ////////////////////////////////////////////////////////
	    // Test onEdge() 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.onEdge(Vector(-1,-1,-1)));
	    assertTrue(b.onEdge(Vector(-1,0,0)));
	    assertTrue(b.onEdge(Vector(1,0,0)));
	    assertTrue(b.onEdge(Vector(-1,0,1)));
	    assertTrue(b.onEdge(Vector(1,0.5,0.5)));
	    assertTrue(!b.onEdge(Vector(0.5,0.5,0.5)));
	    assertTrue(!b.onEdge(Vector(0,0,0.5)));
	    assertTrue(!b.onEdge(Vector(0,0,0)));
	    assertTrue(!b.onEdge(Vector(10,0,0)));

	    ////////////////////////////////////////////////////////
	    // Test inside() 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.inside(Vector(0,0,0)));
	    assertTrue(b.inside(Vector(0.5,0.2,-0.3)));
	    assertTrue(!b.inside(Vector(1,0.2,-0.3)));
	    assertTrue(!b.inside(Vector(0,-1,-0.3)));
	    assertTrue(!b.inside(Vector(0,-1,1)));
	    assertTrue(!b.inside(Vector(0,-10,12)));

	    ////////////////////////////////////////////////////////
	    // Test intersection 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    Ray r;

	    r = Ray(Vector(0.5,0.5,60),Vector(0,0,-1),1);
	    assertTrue(b.checkIntersect(r));
	    Vector2 v = b.intersect(r);
	    assertTrue(r.getPoint(v[0]) == Vector(0.5,0.5,1));
	    assertTrue(r.getPoint(v[1]) == Vector(0.5,0.5,-1));

	    r = Ray(Vector(0.5,-50,0),Vector(0,1,0),1);
	    assertTrue(b.checkIntersect(r));
	    v = b.intersect(r);
	    assertTrue(r.getPoint(v[0]) == Vector(0.5,-1,0));
	    assertTrue(r.getPoint(v[1]) == Vector(0.5,1,0));

	    r = Ray(Vector(2,2,60),Vector(0,0,-1),1);
	    assertTrue(!b.checkIntersect(r));

	    r = Ray(Vector(100,100,0),Vector(-1,-1,0),1);
	    assertTrue(b.checkIntersect(r));

	    r = Ray(Vector(-100,-100,-100),Vector(1,1,1),1);
	    v = b.intersect(r);
	    assertTrue(r.getPoint(v[0]) == Vector(-1,-1,-1));

	    ////////////////////////////////////////////////////////
	    // Test doUnion 
	    ////////////////////////////////////////////////////////
	    BoundingBox b1 = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    BoundingBox b2 = BoundingBox(Vector(-2,-2,-2),Vector(0,0,0));
	    BoundingBox bu = BoundingBox::doUnion(b1,b2);
	    assertTrue(BoundingBox(Vector(-2,-2,-2),Vector(1,1,1)) == bu);

	    BoundingBox bi = BoundingBox::doIntersection(b1,b2);
	    assertTrue(BoundingBox(Vector(-1,-1,-1),Vector(0,0,0)) == bi);

	    ////////////////////////////////////////////////////////
	    // Test cutByPlane 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.cutByPlane(0,10) == -1);
	    assertTrue(b.cutByPlane(1,10) == -1);
	    assertTrue(b.cutByPlane(2,10) == -1);
	    assertTrue(b.cutByPlane(0,-2) == 1);
	    assertTrue(b.cutByPlane(1,-3) == 1);
	    assertTrue(b.cutByPlane(2,-4) == 1);
	    assertTrue(b.cutByPlane(0,0) == 0);
	    assertTrue(b.cutByPlane(1,0.5) == 0);
	    assertTrue(b.cutByPlane(2,-0.5) == 0);

	    ////////////////////////////////////////////////////////
	    // Test center() 
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.center() == Vector(0,0,0));
	    b = BoundingBox(Vector(0,0,0),Vector(2,2,2));
	    assertTrue(b.center() == Vector(1,1,1));
	    b = BoundingBox(Vector(0,0,0),Vector(4,2,2));
	    assertTrue(b.center() == Vector(2,1,1));
	    b = BoundingBox(Vector(10,10,10),Vector(20,20,20));
	    assertTrue(b.center() == Vector(15,15,15));

	    ////////////////////////////////////////////////////////
	    // Test intersectSphere()
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.intersectSphere(Vector(0,0,0),10*10));
	    assertTrue(b.intersectSphere(Vector(0,0,0),2*2));
	    assertTrue(b.intersectSphere(Vector(0,0,0),0.5*0.5));
	    assertTrue(b.intersectSphere(Vector(-2,0,0),2*2));
	    assertTrue(!b.intersectSphere(Vector(0,4,0),2*2));
	    assertTrue(b.intersectSphere(Vector(2,0,0),1*1));
	    assertTrue(b.intersectSphere(Vector(2,0,0),10*10));
	    assertTrue(b.intersectSphere(Vector(2,0,-5),10*10));
	    assertTrue(b.intersectSphere(Vector(2,0,0),2*2));
	    assertTrue(!b.intersectSphere(Vector(2,0,0),0.5*0.5));
	    assertTrue(!b.intersectSphere(Vector(0,0,-2),0.5*0.5));

	    ////////////////////////////////////////////////////////
	    // Test area()
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(0,0,0),Vector(1,1,1));
	    assertTrue(b.area() == 6*1);
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    assertTrue(b.area() == 6*4);
	    b = BoundingBox(Vector(0,0,0),Vector(3,3,3));
	    assertTrue(b.area() == 6*9);

	    ////////////////////////////////////////////////////////
	    // Test split()
	    ////////////////////////////////////////////////////////
	    BoundingBox b_left;
	    BoundingBox b_right;
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    b.split(&b_left, &b_right, 0, 0.0);
	    assertTrue(b_left.minimum() == Vector(-1,-1,-1));
	    assertTrue(b_left.maximum() == Vector(0,1,1));
	    assertTrue(b_right.minimum() == Vector(0,-1,-1));
	    assertTrue(b_right.maximum() == Vector(1,1,1));

	    b.split(&b_left, &b_right, 1, 0.0);
	    assertTrue(b_left.minimum() == Vector(-1,-1,-1));
	    assertTrue(b_left.maximum() == Vector(1,0,1));
	    assertTrue(b_right.minimum() == Vector(-1,0,-1));
	    assertTrue(b_right.maximum() == Vector(1,1,1));

	    b.split(&b_left, &b_right, 2, 0.0);
	    assertTrue(b_left.minimum() == Vector(-1,-1,-1));
	    assertTrue(b_left.maximum() == Vector(1,1,0));
	    assertTrue(b_right.minimum() == Vector(-1,-1,0));
	    assertTrue(b_right.maximum() == Vector(1,1,1));

	    b = BoundingBox(Vector(0,0,0),Vector(2,2,2));
	    b.split(&b_left, &b_right, 0, 0.5);
	    assertTrue(b_left.minimum() == Vector(0,0,0));
	    assertTrue(b_left.maximum() == Vector(0.5,2,2));
	    assertTrue(b_right.minimum() == Vector(0.5,0,0));
	    assertTrue(b_right.maximum() == Vector(2,2,2));

	    ////////////////////////////////////////////////////////
	    // Test copy constructor
	    ////////////////////////////////////////////////////////
	    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
	    BoundingBox a = b;
	    assertTrue(a.maximum() == b.maximum());
	}
};

class kdtree_test : public Test {
    public:
	void run() {
	    KdTree bsp;
	    for(int x = -10; x <= 10; x++) {
		for(int y = -10; y <= 10; y++) {
		    for(int z = -10; z <= 10; z++) {
			Sphere* sx = new Sphere(Vector(x*20,y*20+50,z*20),10,new Material(RGB(0.8,0.8,0.8),0.7,RGB(1.0,1.0,1.0),0.80,40));
			bsp.addObject(sx);
		    }
		}
	    }


	    bsp.addObject(new Sphere(Vector(0,-500,0),10,new Material(RGB(0.8,0.8,0.8),0.7,RGB(1.0,1.0,1.0),0.80,40)));

	    bsp.prepare();

	    // Test intersection
	    Intersection i;
	    Ray r = Ray(Vector(200,250,1000),Vector(0,0,-1),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) != NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == true);
	    assertTrue(i.getPoint() == Vector(200,250,210));

	    r = Ray(Vector(200,250,-1000),Vector(0,0,1),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) != NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == true);
	    assertTrue(i.getPoint() == Vector(200,250,-210));

	    r = Ray(Vector(-200,-150,1000),Vector(0,0,-1),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) != NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == true);
	    assertTrue(i.getPoint() == Vector(-200,-150,210));

	    r = Ray(Vector(0,1000,0),Vector(0,-1,0),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) != NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == true);
	    assertTrue(i.getPoint() == Vector(0,260,0));

	    r = Ray(Vector(0,-1000,0),Vector(0,1,0),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) != NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == true);
	    assertTrue(i.getPoint() == Vector(0,-510,0));

	    r = Ray(Vector(300,250,-1000),Vector(0,0,1),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) == NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == false);

	    r = Ray(Vector(200,250,-1000),Vector(0,0,-1),1);
	    assertTrue(bsp.intersectForShadow(r,HUGE_DOUBLE) == NULL);
	    assertTrue(bsp.intersectForShadow(r,10.0) == NULL);
	    assertTrue(bsp.intersect(r,&i) == false);
	}
};

int main(int argc, char *argv[]) {
    Stats::getUniqueInstance()->clear();

    TestSuite suite;

    suite.add("Bounding box",new boundingbox_test());
    suite.add("Kd-tree",new kdtree_test());
    suite.run();
    suite.printStatus();

    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}

