
#include "parser/wrapper.h"

#include <iostream>

using namespace std;

static scm_t_bits wrapped_object_tag;

struct wrapped_object* scm2wrappedobj(SCM s_smob, char* subr, int pos) {
    SCM_ASSERT (SCM_SMOB_PREDICATE (wrapped_object_tag, s_smob),
	    s_smob, pos, subr);
    return (struct wrapped_object*) SCM_SMOB_DATA(s_smob);
}

bool isWrappedObject(SCM obj) {
    return (SCM_SMOB_PREDICATE (wrapped_object_tag, obj));
}

bool isLightsource(SCM object_smob) {
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
	return o->type == LIGHTSOURCE;
    } else {
	return false;
    }
}

bool isSceneObject(SCM object_smob) 
{
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
	return o->type == SCENEOBJECT;
    } else {
	return false;
    }
}

bool isTexture(SCM object_smob) {
    if (isWrappedObject(object_smob)) {
	struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
	return o->type == TEXTURE;
    } else {
	return false;
    }
}

SCM path2scm(Path* path) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->path = path;
    object->type = PATH;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM sceneobject2scm(SceneObject* sceneobject) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->sceneobject = sceneobject;
    object->type = SCENEOBJECT;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM camera2scm(Camera* camera) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->camera = camera;
    object->type = CAMERA;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM texture2scm(Texture* texture) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->type = TEXTURE; 
    object->texture = texture;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM material2scm(Material* material) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->material = material;
    object->type = MATERIAL;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM lightsource2scm(Lightsource* lightsource) {
    struct wrapped_object* object;
    object = (struct wrapped_object*) scm_must_malloc (sizeof (struct wrapped_object), "wrappedobject");
    object->lightsource = lightsource;
    object->type = LIGHTSOURCE;
    SCM_RETURN_NEWSMOB(wrapped_object_tag, object);
}

SCM mark_wrapper (SCM image_smob) {
    //cout << "mark_wrapper() called." << endl;
    return SCM_BOOL_F;
}

size_t free_wrapper(SCM image_smob) {
    //cout << "free_wrapper() called." << endl;
    return 0;
}

int print_wrapper(SCM object_smob, SCM port, scm_print_state *pstate) {
    struct wrapped_object* o = (struct wrapped_object*) SCM_SMOB_DATA(object_smob);
    char* type_s[] = { "","", "pathobject", "path", "material", "texture", "lightsource", "camera" };
    cout << "#<" << type_s[o->type] << ">" << endl;
    return 1;
}

void init_wrapper_type() {
    wrapped_object_tag = scm_make_smob_type("wrappedobject", sizeof(struct wrapped_object));
    scm_set_smob_mark(wrapped_object_tag, mark_wrapper);
    scm_set_smob_free(wrapped_object_tag, free_wrapper);
    scm_set_smob_print(wrapped_object_tag, print_wrapper);
}

