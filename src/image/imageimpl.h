
#ifndef TRACER_IMAGE_IMPL_H
#define TRACER_IMAGE_IMPL_H

#include "allocator.h"
#include "image/image.h"
#include "image/rgba.h"

// TODO: Set block_size = 16 as compiletime constant. Then the divides and
// modulus below can be done using shifts and and.

#define blockno(x, y) (((y) / block_size) * blocks_w + ((x) / block_size))

template <class T, int N> class ImageImpl : public Image {
public:
  ImageImpl(int w, int h, Allocator::model_t alloc_model = Allocator::AUTO);

  ImageImpl(const Image &image,
            Allocator::model_t alloc_model = Allocator::AUTO);

  // Get a pixel
  RGBA getRGBA(int x, int y) const;
  // Set a pixel
  void setRGBA(int x, int y, const RGBA &color);

  ~ImageImpl();

  // Components per pixel (4,3 or 1)
  int cpp() const { return N; };

private:
  uint32_t offset(int x, int y) const;

  T *data;

  int block_size;
  int blocks_w;
  int blocks_h;

  // Size of image in number of pixels
  uint32_t alloc_size_pixels;

  // Size of image in number of T
  uint32_t alloc_size;

protected:
  uint32_t height;
  uint32_t width;
};

/*
template<>
class ImageImpl<uint8_t,4> : public Image {
    public:
        ImageImpl(int w, int h, Allocator::model_t alloc_model =
Allocator::AUTO);

        ImageImpl(const Image& image, Allocator::model_t alloc_model =
Allocator::AUTO);

        // Get a pixel
        RGBA getRGBA(int x, int y) const;
        // Set a pixel
        void setRGBA(int x, int y, const RGBA& color);

        ~ImageImpl();

    private:
        uint32_t offset(int x, int y) const;

        uint8_t* data;

        int block_size;
        int blocks_w;
        int blocks_h;

        // Size of image in number of pixels
        uint32_t alloc_size_pixels;

        // Size of image in number of T
        uint32_t alloc_size;
};
*/

template <class T, int N>
inline ImageImpl<T, N>::ImageImpl(int w, int h, Allocator::model_t alloc_model)
    : Image(w, h) {
  block_size = 16;
  blocks_w = w / block_size;
  if (w % block_size != 0)
    blocks_w++;
  blocks_h = h / block_size;
  if (h % block_size != 0)
    blocks_h++;
  alloc_size_pixels = block_size * block_size * blocks_w * blocks_h;
  data = (T *)Allocator::safe_allocate(alloc_size_pixels * N * sizeof(T),
                                       alloc_model);
}

template <class T, int N>
inline ImageImpl<T, N>::ImageImpl(const Image &other,
                                  Allocator::model_t alloc_model) {
  ImageImpl(other.getWidth(), other.getHeight(), alloc_model);
  Image::copy(other);
}

template <class T, int N> inline ImageImpl<T, N>::~ImageImpl() {
  Allocator::free(data);
}

template <class T, int N>
inline uint32_t ImageImpl<T, N>::offset(int x, int y) const {
  return (blockno(x, y) * block_size * block_size +
          (y % block_size) * block_size + (x % block_size)) *
         N;
}

template <> inline RGBA ImageImpl<double, 3>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0], data[o + 1], data[o + 2], 1);
}

template <> inline RGBA ImageImpl<double, 4>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0], data[o + 1], data[o + 2], data[o + 3]);
}

template <> inline RGBA ImageImpl<float, 3>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0], data[o + 1], data[o + 2], 1);
}

template <> inline RGBA ImageImpl<float, 4>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0], data[o + 1], data[o + 2], data[o + 3]);
}

template <> inline RGBA ImageImpl<uint8_t, 3>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0] / 255.0, data[o + 1] / 255.0, data[o + 2] / 255.0, 1);
}

template <> inline RGBA ImageImpl<uint8_t, 4>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  return RGBA(data[o + 0] / 255.0, data[o + 1] / 255.0, data[o + 2] / 255.0,
              data[o + 3] / 255.0);
}

template <> inline RGBA ImageImpl<uint8_t, 1>::getRGBA(int x, int y) const {
  uint32_t o = offset(x, y);
  double v = data[o] / 255.0;
  return RGBA(v, v, v, 1);
}

template <>
inline void ImageImpl<double, 3>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = c.r();
  data[o + 1] = c.g();
  data[o + 2] = c.b();
}

template <>
inline void ImageImpl<double, 4>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = c.r();
  data[o + 1] = c.g();
  data[o + 2] = c.b();
  data[o + 3] = c.a();
}

template <>
inline void ImageImpl<float, 3>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = float(c.r());
  data[o + 1] = float(c.g());
  data[o + 2] = float(c.b());
}

template <>
inline void ImageImpl<float, 4>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = float(c.r());
  data[o + 1] = float(c.g());
  data[o + 2] = float(c.b());
  data[o + 3] = float(c.a());
}

template <>
inline void ImageImpl<uint8_t, 3>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = uint32_t(c.r() * 255);
  data[o + 1] = uint32_t(c.g() * 255);
  data[o + 2] = uint32_t(c.b() * 255);
}

template <>
inline void ImageImpl<uint8_t, 4>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o + 0] = uint8_t(c.r() * 255);
  data[o + 1] = uint8_t(c.g() * 255);
  data[o + 2] = uint8_t(c.b() * 255);
  data[o + 3] = uint8_t(c.a() * 255);
}

template <>
inline void ImageImpl<uint8_t, 1>::setRGBA(int x, int y, const RGBA &c) {
  uint32_t o = offset(x, y);
  data[o] = uint8_t(c.r() * 255);
}

#endif
