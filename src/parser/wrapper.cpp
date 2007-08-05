
#include "parser/wrapper.h"

#include <iostream>

using namespace std;

static int wrapped_object_tag;

struct wrapped_object* scm2wrappedobj(SchemeObject* s_smob, char* subr, int pos) {
    assert_arg_wrapped_type(subr, pos, s_smob, wrapped_object_tag)
    return (struct wrapped_object*) s_smob->getWrappedCObject();
}

bool isWrappedObject(SchemeObject* obj) {
    return i_wrapped_object_p(obj, wrapped_object_tag) == S_TRUE;        
}

bool isLightsource(SchemeObject* object_smob) {
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == LIGHTSOURCE;
    } else {
	return false;
    }
}

bool isMaterial(SchemeObject* object_smob) {
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == MATERIAL;
    } else {
	return false;
    }
}

bool isSampler(SchemeObject* object_smob) 
{
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == SAMPLER;
    } else {
	return false;
    }
}


bool isSceneObject(SchemeObject* object_smob) 
{
    if (isWrappedObject(object_smob)) {
    	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == SCENEOBJECT;
    } else {
	return false;
    }
}

bool isTexture(SchemeObject* object_smob) {
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == TEXTURE;
    } else {
	return false;
    }
}

bool isImage(SchemeObject* object_smob) {
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = (struct wrapped_object*) object_smob->getWrappedCObject();
	return o->type == IMAGE;
    } else {
	return false;
    }
}

SchemeObject* path2scm(Path* path) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->path = path;
    object->type = PATH;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Path* scm2path(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != PATH) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->path;
}


SchemeObject* sceneobject2scm(SceneObject* sceneobject) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->sceneobject = sceneobject;
    object->type = SCENEOBJECT;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

SceneObject* scm2sceneobject(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != SCENEOBJECT) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->sceneobject;
}


SchemeObject* sampler2scm(SamplerFactory* sampler) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->sampler = sampler;
    object->type = SAMPLER;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

SamplerFactory* scm2sampler(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != SAMPLER) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->sampler;
}

SchemeObject* camera2scm(Camera* camera) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->camera = camera;
    object->type = CAMERA;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Camera* scm2camera(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != CAMERA) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->camera;
}

SchemeObject* texture2scm(Texture* texture) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->type = TEXTURE; 
    object->texture = texture;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Texture* scm2texture(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != TEXTURE) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->texture;
}

SchemeObject* image2scm(Image* image) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->type = IMAGE; 
    object->image = image;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Image* scm2image(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != IMAGE) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->image;
}

SchemeObject* material2scm(Material* material) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->material = material;
    object->type = MATERIAL;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Material* scm2material(SchemeObject* object_smob, char* subr, int pos) {
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != MATERIAL) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->material;
}

SchemeObject* lightsource2scm(Lightsource* lightsource) {
    struct wrapped_object* object;
    object = new wrapped_object();
    object->lightsource = lightsource;
    object->type = LIGHTSOURCE;
    return SchemeObject::createWrappedCObject(wrapped_object_tag, object);
}

Lightsource* scm2lightsource(SchemeObject* object_smob, char* subr, int pos)
{
    struct wrapped_object* o = scm2wrappedobj(object_smob, subr, pos);
    if (o->type != LIGHTSOURCE) {
	wrong_type_arg(subr, pos, object_smob);
    }
    return o->lightsource;
}

void init_wrapper_type() {
    wrapped_object_tag = SchemeObject::registerWrappedObject();
}
