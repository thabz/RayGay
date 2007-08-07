
#ifndef RAYGAY_IMAGE_DRAWING_H
#define RAYGAY_IMAGE_DRAWING_H

class Image;
class RGBA;

class ImageDrawing {

    public:
	static void line(Image* image, int x1, int y1, int x2, int y2, const RGBA& c);

};

#endif
