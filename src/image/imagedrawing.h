
#ifndef RAYGAY_IMAGE_DRAWING_H
#define RAYGAY_IMAGE_DRAWING_H

class Image;
class RGBA;

#include <string>
#include "ttf.h"

class ImageDrawing 
{
    public:
        /// See http://panda3d.org/wiki/index.php/Texture_Blend_Modes for some blend modes.    
        enum AlphaCombineMode {
            REPLACE,
            ADD,        
            MODULATE,
            DECAL
        };
            
	    static void line(Image* image, int x1, int y1, int x2, int y2, const RGBA& c, AlphaCombineMode am = REPLACE);
	    static void line_slow(Image* image, float x1, float y1, float x2, float y2, const RGBA& c, AlphaCombineMode am = REPLACE);
	    static void circle(Image* image, int x1, int y1, int r, const RGBA& c, AlphaCombineMode am = REPLACE);
        static void string(Image* image, int x, int y, std::wstring text, TrueTypeFont* font, int size, const RGBA& c, AlphaCombineMode am = REPLACE);
        static void quadraticBezierCurve(Image* image, float x0, float y0, float x1, float y1, float x2, float y2, const RGBA& c, AlphaCombineMode am = REPLACE);
        static void pixel(Image* image, float x, float y, const RGBA& c, AlphaCombineMode am = REPLACE);
	    static void filledbox(Image* image, int x, int y, int w, int h, const RGBA& c, AlphaCombineMode am = REPLACE);
        

    private:
        static void fillGlyph(Image* image, int x, int y, TrueTypeFont::Glyph* glyph, int size, const RGBA& color, bool aa, ImageDrawing::AlphaCombineMode am);
        static void strokeGlyph(Image* image, int x, int y, TrueTypeFont::Glyph* glyph, int size, const RGBA& color, ImageDrawing::AlphaCombineMode am);
};

#endif

