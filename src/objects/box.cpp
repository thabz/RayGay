
#include "box.h"

#include <iostream>
#include <cassert>

#include "ray.h"
#include "intersection.h"
#include "math/matrix.h"
#include "image/rgb.h"
#include "mesh.h"

Box::Box(const Vector a, const Vector b, const Material* mat) : Mesh(Mesh::MESH_FLAT,mat) {
    prepareBox(a,b);
}

Box::Box(const Vector c, double width, double height, double depth, Material* m): Mesh(Mesh::MESH_FLAT,m) {
    Vector extend = Vector(width,height,depth) / 2;
    prepareBox(c - extend, c + extend);
}

void Box::prepareBox(const Vector a, const Vector b) {
    // Back left bottom
    Vector c2 = Vector(min(a[0],b[0]),min(a[1],b[1]),min(a[2],b[2]));
    // Front right top
    Vector c7 = Vector(max(a[0],b[0]),max(a[1],b[1]),max(a[2],b[2]));
    
    // Back right bottom
    Vector c1 = Vector(c7[0],c2[1],c2[2]);
    // Back left top
    Vector c4 = Vector(c2[0],c7[1],c2[2]);
    // Back right top
    Vector c3 = Vector(c7[0],c7[1],c2[2]);
    // Front right bottom
    Vector c5 = Vector(c7[0],c2[1],c7[2]);
    // Front left bottom
    Vector c6 = Vector(c2[0],c2[1],c7[2]);
    // Front left top
    Vector c8 = Vector(c2[0],c7[1],c7[2]);

    // Bottom left
    Vector2 uv00 = Vector2(0.0,0.0);
    // Top left
    Vector2 uv01 = Vector2(0.0,1.0);
    // Bottom right
    Vector2 uv10 = Vector2(1.0,0.0);
    // Top right
    Vector2 uv11 = Vector2(1.0,1.0);

    // Back face
    Mesh::addTriangle(c1,c2,c3,uv00,uv10,uv01);
    Mesh::addTriangle(c2,c4,c3,uv10,uv11,uv01);
    // Left face
    Mesh::addTriangle(c2,c6,c4,uv00,uv10,uv01); 
    Mesh::addTriangle(c6,c8,c4,uv10,uv11,uv01);
    // Bottom face
    Mesh::addTriangle(c5,c6,c2,uv11,uv01,uv00); 
    Mesh::addTriangle(c1,c5,c2,uv10,uv11,uv00);
    // Front face
    Mesh::addTriangle(c6,c5,c7,uv00,uv10,uv11); 
    Mesh::addTriangle(c6,c7,c8,uv00,uv11,uv01);
    // Right face
    Mesh::addTriangle(c5,c1,c7,uv00,uv10,uv01); 
    Mesh::addTriangle(c1,c3,c7,uv10,uv11,uv01);
    // Top face
    Mesh::addTriangle(c3,c4,c7,uv11,uv01,uv10); 
    Mesh::addTriangle(c4,c8,c7,uv01,uv00,uv10);
}

Box::~Box() {
    // Do we leak stuff inside Mesh here?
}

