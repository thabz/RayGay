
#include "objects/parametrizedsurface.h"
#include <cassert>

ParametrizedSurface::ParametrizedSurface(
	uint32_t uRes, 
	uint32_t vRes, 
	bool uClose, 
	bool vClose, 
	const Material* m) : Mesh(Mesh::MESH_PHONG, m) 
{
    this->uRes = uRes;
    this->vRes = vRes;
    this->uClose = uClose;
    this->vClose = vClose;
}

void ParametrizedSurface::prepare() {

    uint32_t uVerticesNum = uRes + (uClose ? 0 : 1);
    uint32_t vVerticesNum = vRes + (vClose ? 0 : 1);

    hintVertexNum(uVerticesNum * vVerticesNum);

    for(uint32_t ui = 0; ui < uVerticesNum; ui++) {
	for(uint32_t vi = 0; vi < vVerticesNum; vi++) {
	    double u = double(ui) / uRes;
	    double v = double(vi) / vRes;
	    Vector vertex = eval(u, v);
	    addVertex(vertex);
	}
    }

    hintFaceNum(uRes * vRes);

    uint32_t c[4];
    Vector2 uvs[4];
    double uStep = 1.0 / double(uRes);
    double vStep = 1.0 / double(vRes);

    for(uint32_t u = 0; u < uRes; u++) {
	for(uint32_t v = 0; v < vRes; v++) {
	    double u1 = (0 + u) * uStep;
	    double u2 = (1 + u) * uStep;
	    double v1 = (0 + v) * vStep;
	    double v2 = (1 + v) * vStep;

	    uvs[0] = Vector2(u1,v1);
	    uvs[1] = Vector2(u1,v2);
	    uvs[2] = Vector2(u2,v2);
	    uvs[3] = Vector2(u2,v1);

	    /*
	    c[0] = (u + 0) + (v + 0) * uVerticesNum;
	    c[1] = (u + 1) + (v + 0) * uVerticesNum;
	    c[2] = (u + 1) + (v + 1) * uVerticesNum;
	    c[3] = (u + 0) + (v + 1) * uVerticesNum;
	    */
	    c[0] = (v + 0) + (u + 0) * vVerticesNum;
	    c[1] = (v + 1) + (u + 0) * vVerticesNum;
	    c[2] = (v + 1) + (u + 1) * vVerticesNum;
	    c[3] = (v + 0) + (u + 1) * vVerticesNum;

	    addQuad(c,uvs);
	}
    }

    Mesh::prepare();
}

