
#ifndef IMAGE_MULTI_TEXTURE_H
#define IMAGE_MULTI_TEXTURE_H

#include "image/texture.h"
#include "collections/lru_hash.h"
#include <vector>
#include <string>

class Image;

/**
 * A multitexture is one that is composed of a grid of images.
 */
class MultiTexture : public Texture {
    public:
	    MultiTexture(std::vector<std::string> filenames, uint32_t tiles_per_row, uint32_t memory_cached, const Vector2& repeat_uv, Texture::InterpolationType it);

    private:
	    RGBA getRGB(int x, int y) const;
        Image* getTile(uint32_t index) const;

    	mutable pthread_mutex_t mutex_loader;
	    std::vector<std::string> filenames;
	    mutable lru_hash<int,Image*>* images;
	    uint32_t tiles_per_row;
	    uint32_t memory_cached;
	    uint32_t tile_width;
	    uint32_t tile_height;
};

#endif
