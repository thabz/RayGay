
#ifndef PHOTON_MAP_DUMP
#define PHOTON_MAP_DUMP

class Scene;
class Image;
class GlobalPhotonMap;

class PhotonMapDump {

    public:
	void render(Scene* scene, Image* img, GlobalPhotonMap* photonmap, int num);

};

#endif

