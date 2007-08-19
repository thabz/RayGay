
#ifndef RAYGAY_IMAGE_DRAWING_H
#define RAYGAY_IMAGE_DRAWING_H

class Image;
class RGBA;
class TrueTypeFont;

#include <string>

class ImageDrawing {

    public:
	static void line(Image* image, int x1, int y1, int x2, int y2, const RGBA& c);
	static void circle(Image* image, int x1, int y1, int r, const RGBA& c);
        static void string(Image* image, int x, int y, std::wstring text, TrueTypeFont* font, int size, const RGBA& c);
        static void quadraticBezierCurve(Image* image, float x0, float y0, float x1, float y1, float x2, float y2, const RGBA& c);

};

#endif
