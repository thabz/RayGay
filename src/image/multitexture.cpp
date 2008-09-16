
#include "image/multitexture.h"
#include "image/image.h"
#include "exception.h"

using namespace std;

MultiTexture::MultiTexture(vector<string> filenames, uint32_t tiles_per_row, uint32_t memory_cached, const Vector2& repeat_uv, Texture::InterpolationType it) : Texture(repeat_uv, it) {
    assert(memory_cached > 0);
    assert(tiles_per_row > 0);
    assert(filenames.size() > 0);
    this->memory_cached = memory_cached;
    this->filenames = filenames;
    this->tiles_per_row = tiles_per_row;
    pthread_mutex_init(&mutex_loader,NULL);
    this->images = new lru_hash<int,Image*>(memory_cached);

    Image* first_tile = getTile(0);
    this->tile_width = first_tile->getWidth();
    this->tile_height = first_tile->getHeight();
    this->width = tiles_per_row * tile_width;
    this->height = (filenames.size() / tiles_per_row) * tile_height;
}

RGBA MultiTexture::getRGB(int x, int y) const {
    int tile_x = x / tile_width;
    int tile_y = y / tile_height;
    int index = tile_x + tile_y * tiles_per_row;
    Image* image = getTile(index);
    return image->getRGBA(x % tile_width, y % tile_height);
}

Image* MultiTexture::getTile(uint32_t index) const {
    Image** image_ptr = images->find(index);
    if (image_ptr != NULL) return *image_ptr;
    
    // Not found. We'll need to load it.
    if (index >= filenames.size()) {
        throw_exception("index out of bounds");
    }
    
    pthread_mutex_lock(&mutex_loader);
    //cout << "Loading " << filenames[index] << endl;
    Image* image = Image::load(filenames[index],Allocator::MALLOC_ONLY);
    images->insert(index,image);
    pthread_mutex_unlock(&mutex_loader);
    return image;
}
