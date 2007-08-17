
#include "image/imagedrawing.h"
#include "image/image.h"
#include <algorithm>
#include "ttf.h"

/**
 * Bresenham's algorithm.
 *
 * Lookup Gupta-Sproull if you want to do an antialiased version too.
 * 
 */	     
void ImageDrawing::line(Image* image, int x0, int y0, int x1, int y1, const RGBA& c) {
    bool steep = abs(y1-y0) > abs(x1-x0);
    if (steep) {
	std::swap(x0,y0);
	std::swap(x1,y1);
    }
    if (x0 > x1) {
	std::swap(x0,x1);
	std::swap(y0,y1);
    }
    int deltax = x1 - x0;
    int deltay = abs(y1-y0);
    int error = -deltax / 2;
    int ystep = y0 < y1 ? 1 : -1;
    int y = y0;
    for(int x = x0; x <= x1; x++) {
	if (steep) { 
	    image->safeSetRGBA(y,x,c);
	} else {
	    image->safeSetRGBA(x,y,c);
	}
	error += deltay;
	if (error > 0) {
	    y += ystep;
	    error -= deltax;
	}
    }
};

void ImageDrawing::circle(Image* image, int x0, int y0, int r, const RGBA& c) {
    int f = 1 - r;
    int ddF_x = 0;
    int ddF_y = -2 * r;
    int x = 0;
    int y = r;

    image->safeSetRGBA(x0,y0+r,c);
    image->safeSetRGBA(x0,y0-r,c);
    image->safeSetRGBA(x0+r,y0,c);
    image->safeSetRGBA(x0-r,y0,c);

    while(x < y) {
        if(f > 0) {
            y--;
            ddF_y += 2;
            f += ddF_y;
        }
        x++;
        ddF_x += 2;
        f += ddF_x + 1;    
        image->safeSetRGBA(x0+x,y0+y,c);
        image->safeSetRGBA(x0-x,y0+y,c);
        image->safeSetRGBA(x0+x,y0-y,c);
        image->safeSetRGBA(x0-x,y0-y,c);
        image->safeSetRGBA(x0+y,y0+x,c);
        image->safeSetRGBA(x0-y,y0+x,c);
        image->safeSetRGBA(x0+y,y0-x,c);
        image->safeSetRGBA(x0-y,y0-x,c);
    }
}

void ImageDrawing::text(Image* image, int x, int y, std::string text, TrueTypeFont* font, int size, const RGBA& color) {
    vector<TrueTypeFont::Glyph*> glyphs = font->getGlyphs(text, size);   
    
    for(uint32_t i = 0; i < glyphs.size(); i++) {
        TrueTypeFont::Glyph* glyph = glyphs[i];
        for(uint32_t j = 0; j < glyph->contours.size(); j++) {
            TrueTypeFont::Contour contour = glyph->contours[j];
            for(uint32_t k = 0; k < contour.coords.size(); k++) {
                if (k > 0) {
                    TrueTypeFont::Coord c0 = contour.coords[k-1];
                    TrueTypeFont::Coord c1 = contour.coords[k];
                    line(image, c0.x*size+x, y-c0.y*size, c1.x*size+x, y-c1.y*size, color);        
                }    
            }
            TrueTypeFont::Coord c0 = contour.coords[contour.coords.size() - 1];
            TrueTypeFont::Coord c1 = contour.coords[0];
            line(image, c0.x*size+x, y-c0.y*size, c1.x*size+x, y-c1.y*size, color);        
        }
        
    }
    
    
    
    
};


