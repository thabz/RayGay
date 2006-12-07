
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
class SamplerFactory;

enum wrapped_type {
    SCENEOBJECT = 2,
    PATH	= 3,
    MATERIAL	= 4,
    TEXTURE	= 5,
    LIGHTSOURCE = 6,
    CAMERA	= 7,
    SAMPLER	= 8
};

struct wrapped_object {
    union {
	SceneObject* sceneobject;
	Path* path;
	Material* material;
	Texture* texture;
	Lightsource* lightsource;
	Camera* camera;
	SamplerFactory* sampler;
	void* obj;
    };
    wrapped_type type;
};

struct wrapped_object* scm2wrappedobj(SCM s_smod, char* subr, int pos);

void assert_type(struct wrapped_object* obj, wrapped_type type);

Path* scm2path(SCM object_smob, char* subr, int pos);
SCM path2scm(Path* path);

SceneObject* scm2sceneobject(SCM object_smob, char* subr, int pos);
SCM sceneobject2scm(SceneObject* sceneobject);

Camera* scm2camera(SCM object_smob, char* subr, int pos);
SCM camera2scm(Camera* camera);

SamplerFactory* scm2sampler(SCM object_smob, char* subr, int pos);
SCM sampler2scm(SamplerFactory* sampler_factory);

Texture* scm2texture(SCM object_smob, char* subr, int pos);
SCM texture2scm(Texture* texture);

SCM material2scm(Material* material);
Material* scm2material(SCM object_smob, char* subr, int pos);

Lightsource* scm2lightsource(SCM object_smob, char* subr, int pos);
SCM lightsource2scm(Lightsource* lightsource);

void init_wrapper_type();

bool isLightsource(SCM object_smob);

bool isMaterial(SCM object_smob);

bool isSceneObject(SCM object_smob);

bool isTexture(SCM object_smob);

bool isWrappedObject(SCM obj);

#endif
