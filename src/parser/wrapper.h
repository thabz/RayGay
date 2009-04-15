
#ifndef PARSER_WRAPPER_H
#define PARSER_WRAPPER_H

#include <cassert>
#include <iostream>

#include <scheme/scheme.h>

using namespace std;


class SceneObject;
class Path;
class Material;
class Texture;
class Image;
class Lightsource;
class Camera;
class SamplerFactory;

enum wrapped_type {
    SCENEOBJECT = 2,
    PATH	= 3,
    MATERIAL	= 4,
    TEXTURE	= 5,
    LIGHTSOURCE = 6,
    CAMERA	= 7,
    SAMPLER	= 8,
    IMAGE       = 9
};

struct wrapped_object : public SchemeWrappedCObject {
    union {
	SceneObject* sceneobject;
	Path* path;
	Material* material;
	Texture* texture;
        Image* image;
	Lightsource* lightsource;
	Camera* camera;
	SamplerFactory* sampler;
	void* obj;
    };
    wrapped_type type;
};

struct wrapped_object* scm2wrappedobj(SchemeObject* s_smod, const wchar_t* subr, int pos);

void assert_type(struct wrapped_object* obj, wrapped_type type);

Path* scm2path(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* path2scm(Path* path);

SceneObject* scm2sceneobject(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* sceneobject2scm(SceneObject* sceneobject);

Camera* scm2camera(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* camera2scm(Camera* camera);

SamplerFactory* scm2sampler(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* sampler2scm(SamplerFactory* sampler_factory);

Image* scm2image(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* image2scm(Image* image);

Texture* scm2texture(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* texture2scm(Texture* texture);

SchemeObject* material2scm(Material* material);
Material* scm2material(SchemeObject* object_smob, const wchar_t* subr, int pos);

Lightsource* scm2lightsource(SchemeObject* object_smob, const wchar_t* subr, int pos);
SchemeObject* lightsource2scm(Lightsource* lightsource);

void init_wrapper_type();

SchemeObject* isWrappedObjectType(SchemeObject* obj, wrapped_type type);

#endif
