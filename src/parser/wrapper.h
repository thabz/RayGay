
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

static scm_t_bits wrapped_object_tag;

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

static SCM path2scm(Path* path) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->path = path;
    object->type = PATH;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

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

static SCM sceneobject2scm(SceneObject* sceneobject) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->sceneobject = sceneobject;
    object->type = SCENEOBJECT;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

static Camera* scm2camera(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != CAMERA) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->camera;
}

static SCM camera2scm(Camera* camera) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->camera = camera;
    object->type = CAMERA;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

static Texture* scm2texture(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    if (o->type != TEXTURE) {
	scm_wrong_type_arg(subr, pos, object_smob);
    }
    return o->texture;
}

static SCM texture2scm(Texture* texture) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->type = TEXTURE; 
    object->texture = texture;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

static SCM material2scm(Material* material) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->material = material;
    object->type = MATERIAL;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

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

static SCM lightsource2scm(Lightsource* lightsource) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->lightsource = lightsource;
    object->type = LIGHTSOURCE;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

void init_wrapper_type();

static bool isLightsource(SCM object_smob) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    return o->type == LIGHTSOURCE;
}

static bool isSceneObject(SCM object_smob) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    return o->type == SCENEOBJECT;
}

static bool isWrappedObject(SCM obj) {
    return (SCM_NFALSEP(SCM_SMOB_PREDICATE (wrapped_object_tag, obj)));
}

#endif
