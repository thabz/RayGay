
#ifndef PHOTON_MAP_DUMP
#define PHOTON_MAP_DUMP

class Scene;
class Image;
class PhotonMap;

class PhotonMapDump {

    public:
	void render(Scene* scene, Image* img, PhotonMap* photonmap, int num);

};

#endif

