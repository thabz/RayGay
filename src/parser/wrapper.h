
#ifndef PARSER_WRAPPER_H
#define PARSER_WRAPPER_H

#include <cassert>
#include <libguile.h>
#include <guile/gh.h>
#include <iostream>

using namespace std;

class SceneObject;
class Path;
class Material;
class Texture;
class Lightsource;
class Camera;

enum wrapped_type {
    SCENEOBJECT = 2,
    PATH	= 3,
    MATERIAL	= 4,
    TEXTURE	= 5,
    LIGHTSOURCE = 6,
    CAMERA	= 7
};

struct wrapped_object {
    union {
	SceneObject* sceneobject;
	Path* path;
	Material* material;
	Texture* texture;
	Lightsource* lightsource;
	Camera* camera;
	void* obj;
    };
    wrapped_type type;
};

SCM path2scm(Path* path);

static Path* scm2path(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != PATH) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->path;
}

static SceneObject* scm2sceneobject(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != SCENEOBJECT) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->sceneobject;
}

SCM sceneobject2scm(SceneObject* sceneobject);

static Camera* scm2camera(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != CAMERA) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->camera;
}

SCM camera2scm(Camera* camera);

static Texture* scm2texture(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != TEXTURE) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->texture;
}

SCM texture2scm(Texture* texture);

SCM material2scm(Material* material);

static Material* scm2material(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != MATERIAL) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->material;
}

static Lightsource* scm2lightsource(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != LIGHTSOURCE) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->lightsource;
}

SCM lightsource2scm(Lightsource* lightsource);

void init_wrapper_type();

bool isLightsource(SCM object_smob);

bool isSceneObject(SCM object_smob);

bool isWrappedObject(SCM obj);

#endif
