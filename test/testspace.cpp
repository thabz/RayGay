#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <cassert>
#include <iostream>

#include "boundingbox.h"
#include "intersection.h"
#include "ray.h"
#include "space/bsp.h"
#include "space/kdtree.h"
#include "objects/sphere.h"
#include "math/vector2.h"
#include "materials/material.h"

void boundingbox_test() {
    BoundingBox b;
    /* Test normal() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.normal(Vector(1,0,0))[0] = 1);
    assert(b.normal(Vector(-1,0,0))[0] = -1);
    assert(b.normal(Vector(0,1,0))[1] = 1);
    assert(b.normal(Vector(0,-1,0))[1] = -1);
    assert(b.normal(Vector(0.5,0.2,1))[2] = 1);
    assert(b.normal(Vector(0,0,-1))[2] = -1);

    b = BoundingBox(Vector(-100,-10,-100),Vector(100,10,100));
    assert(b.normal(Vector(-100,0,-100))[0] = -1);


    /* Test onEdge() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.onEdge(Vector(-1,-1,-1)));
    assert(b.onEdge(Vector(-1,0,0)));
    assert(b.onEdge(Vector(1,0,0)));
    assert(b.onEdge(Vector(-1,0,1)));
    assert(b.onEdge(Vector(1,0.5,0.5)));
    assert(!b.onEdge(Vector(0.5,0.5,0.5)));
    assert(!b.onEdge(Vector(0,0,0.5)));
    assert(!b.onEdge(Vector(0,0,0)));
    assert(!b.onEdge(Vector(10,0,0)));

    /* Test inside() */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.inside(Vector(0,0,0)));
    assert(b.inside(Vector(0.5,0.2,-0.3)));
    assert(!b.inside(Vector(1,0.2,-0.3)));
    assert(!b.inside(Vector(0,-1,-0.3)));
    assert(!b.inside(Vector(0,-1,1)));
    assert(!b.inside(Vector(0,-10,12)));

    /* Test intersection */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    Ray r;
    
    r = Ray(Vector(0.5,0.5,60),Vector(0,0,-1),1);
    assert(b.checkIntersect(r));
    Vector2 v = b.intersect(r);
    assert(r.getPoint(v[0]) == Vector(0.5,0.5,1));
    assert(r.getPoint(v[1]) == Vector(0.5,0.5,-1));

    r = Ray(Vector(0.5,-50,0),Vector(0,1,0),1);
    assert(b.checkIntersect(r));
    v = b.intersect(r);
    assert(r.getPoint(v[0]) == Vector(0.5,-1,0));
    assert(r.getPoint(v[1]) == Vector(0.5,1,0));
    
    r = Ray(Vector(2,2,60),Vector(0,0,-1),1);
    assert(!b.checkIntersect(r));

    r = Ray(Vector(100,100,0),Vector(-1,-1,0),1);
    assert(b.checkIntersect(r));

    r = Ray(Vector(-100,-100,-100),Vector(1,1,1),1);
    v = b.intersect(r);
    assert(r.getPoint(v[0]) == Vector(-1,-1,-1));

    /* Test doUnion */
    BoundingBox b1 = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    BoundingBox b2 = BoundingBox(Vector(-2,-2,-2),Vector(0,0,0));
    BoundingBox bu = BoundingBox::doUnion(b1,b2);
    assert(BoundingBox(Vector(-2,-2,-2),Vector(1,1,1)) == bu);

    BoundingBox bi = BoundingBox::doIntersection(b1,b2);
    assert(BoundingBox(Vector(-1,-1,-1),Vector(0,0,0)) == bi);

    /* Test cutByPlane */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.cutByPlane(0,10) == -1);
    assert(b.cutByPlane(1,10) == -1);
    assert(b.cutByPlane(2,10) == -1);
    assert(b.cutByPlane(0,-2) == 1);
    assert(b.cutByPlane(1,-3) == 1);
    assert(b.cutByPlane(2,-4) == 1);
    assert(b.cutByPlane(0,0) == 0);
    assert(b.cutByPlane(1,0.5) == 0);
    assert(b.cutByPlane(2,-0.5) == 0);

    /* Test center */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.center() == Vector(0,0,0));
    b = BoundingBox(Vector(0,0,0),Vector(2,2,2));
    assert(b.center() == Vector(1,1,1));
    b = BoundingBox(Vector(0,0,0),Vector(4,2,2));
    assert(b.center() == Vector(2,1,1));
    b = BoundingBox(Vector(10,10,10),Vector(20,20,20));
    assert(b.center() == Vector(15,15,15));

    /* Test intersectSphere */
    b = BoundingBox(Vector(-1,-1,-1),Vector(1,1,1));
    assert(b.intersectSphere(Vector(0,0,0),10*10));
    assert(b.intersectSphere(Vector(0,0,0),2*2));
    assert(b.intersectSphere(Vector(0,0,0),0.5*0.5));
    assert(b.intersectSphere(Vector(-2,0,0),2*2));
    assert(!b.intersectSphere(Vector(0,4,0),2*2));
    assert(b.intersectSphere(Vector(2,0,0),1*1));
    assert(b.intersectSphere(Vector(2,0,0),10*10));
    assert(b.intersectSphere(Vector(2,0,-5),10*10));
    assert(b.intersectSphere(Vector(2,0,0),2*2));
    assert(!b.intersectSphere(Vector(2,0,0),0.5*0.5));
    assert(!b.intersectSphere(Vector(0,0,-2),0.5*0.5));

}

void bsp_test() {
    BSP bsp;
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
    Ray r = Ray(Vector(200,250,1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],250));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],210));

    r = Ray(Vector(200,250,-1000),Vector(0,0,1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,20.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],250));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],-210));

    r = Ray(Vector(-200,-150,1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,20.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],-200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],-150));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],210));

    r = Ray(Vector(0,1000,0),Vector(0,-1,0),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,20.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],0));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],260));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],0));

    r = Ray(Vector(0,-1000,0),Vector(0,1,0),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,(const Object*)NULL,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,(const Object*)NULL,20.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],0));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],-510));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],0));

    r = Ray(Vector(300,250,-1000),Vector(0,0,1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == false);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == false);

    r = Ray(Vector(200,250,-1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == false);
    assert(bsp.intersect(r) == false);

    assert(bsp.intersectForShadow(r,(const Object*)NULL,HUGE_DOUBLE) == false);
}

void kdtree_test() {
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
    Ray r = Ray(Vector(200,250,1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],250));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],210));

    r = Ray(Vector(200,250,-1000),Vector(0,0,1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],250));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],-210));

    r = Ray(Vector(-200,-150,1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],-200));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],-150));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],210));

    r = Ray(Vector(0,1000,0),Vector(0,-1,0),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],0));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],260));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],0));

    r = Ray(Vector(0,-1000,0),Vector(0,1,0),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersectForShadow(r,(const Object*)NULL,HUGE_DOUBLE) == true);
    assert(bsp.intersectForShadow(r,(const Object*)NULL,10.0) == false);
    assert(bsp.intersect(r) == true);
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[0],0));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[1],-510));
    assert(IS_EQUAL(bsp.getLastIntersection()->getPoint()[2],0));

    r = Ray(Vector(300,250,-1000),Vector(0,0,1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == false);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == false);

    r = Ray(Vector(200,250,-1000),Vector(0,0,-1),1);
    assert(bsp.intersectForShadow(r,HUGE_DOUBLE) == false);
    assert(bsp.intersectForShadow(r,10.0) == false);
    assert(bsp.intersect(r) == false);

    assert(bsp.intersectForShadow(r,(const Object*)NULL,HUGE_DOUBLE) == false);
}

int main(int argc, char *argv[]) {

    boundingbox_test();
    bsp_test();
    kdtree_test();
    BSP::test();
    return EXIT_SUCCESS;
}

