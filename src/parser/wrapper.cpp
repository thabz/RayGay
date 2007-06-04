
#include "parser/wrapper.h"

#include <iostream>

using namespace std;

static int wrapped_object_tag;

struct wrapped_object* scm2wrappedobj(SCM s_smob, char* subr, int pos) {
    assert_arg_wrapped_type(subr, pos, s_smob, wrapped_object_tag)
    return (struct wrapped_object*) s_smob->wrapped_data;
}

bool isWrappedObject(SCM obj) {
    return i_wrapped_object_p(arg,subtype) == S_TRUE;        
}

bool isLightsource(SCM object_smob) {
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = object_smob->getWrappedCObject();
	return o->type == LIGHTSOURCE;
    } else {
	return false;
    }
}

bool isMaterial(SCM object_smob) {
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = object_smob->getWrappedCObject();
	return o->type == MATERIAL;
    } else {
	return false;
    }
}

bool isSampler(SCM object_smob) 
{
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = object_smob->getWrappedCObject();
	return o->type == SAMPLER;
    } else {
	return false;
    }
}


bool isSceneObject(SCM object_smob) 
{
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = object_smob->getWrappedCObject();
	return o->type == SCENEOBJECT;
    } else {
	return false;
    }
}

bool isTexture(SCM object_smob) {
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = object_smob->getWrappedCObject();
	return o->type == TEXTURE;
    } else {
	return false;
    }
}

SCM path2scm(Path* path) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->path = path;
    object->type = PATH;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Path* scm2path(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != PATH) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->path;
}


SCM sceneobject2scm(SceneObject* sceneobject) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->sceneobject = sceneobject;
    object->type = SCENEOBJECT;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

SceneObject* scm2sceneobject(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != SCENEOBJECT) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->sceneobject;
}


SCM sampler2scm(SamplerFactory* sampler) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->sampler = sampler;
    object->type = SAMPLER;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

SamplerFactory* scm2sampler(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != SAMPLER) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->sampler;
}

SCM camera2scm(Camera* camera) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->camera = camera;
    object->type = CAMERA;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Camera* scm2camera(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != CAMERA) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->camera;
}

SCM texture2scm(Texture* texture) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->type = TEXTURE; 
    object->texture = texture;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Texture* scm2texture(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != TEXTURE) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->texture;
}

SCM material2scm(Material* material) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->material = material;
    object->type = MATERIAL;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Material* scm2material(SCM object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != MATERIAL) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->material;
}

SCM lightsource2scm(Lightsource* lightsource) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->lightsource = lightsource;
    object->type = LIGHTSOURCE;
    SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Lightsource* scm2lightsource(SCM object_smob, char* subr, int pos)
{
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != LIGHTSOURCE) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->lightsource;
}

void init_wrapper_type() {
    wrapped_object_tag = SceneObject::registerWrappedObject();
}
