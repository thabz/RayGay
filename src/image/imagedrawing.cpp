
#include "image/imagedrawing.h"
#include "image/image.h"
#include "image/rgba.h"
#include <algorithm>
#include "ttf.h"

void ImageDrawing::pixel(Image* image, float xf, float yf, const RGBA& color, ImageDrawing::AlphaCombineMode am) {
    int x = int(xf);
    int y = int(yf);
    if (x >= 0 && x < image->getWidth() && y >= 0 && y < image->getHeight()) {
        RGBA c = color;
        double a = c.a();
        if (a < 1.0) {
            RGBA existing = image->getRGBA(x,y);    
            if (am == ImageDrawing::ADD) {
                c = existing + c * a;
            } else if (am == ImageDrawing::DECAL) {
                c = c * a + existing * (1 - a);
            } 
        }        
        image->setRGBA(x,y,c); 
    }    
}


/**
 * Bresenham's algorithm.
 *
 * Lookup Gupta-Sproull if you want to do an antialiased version too.
 */	     
void ImageDrawing::line(Image* image, int x0, int y0, int x1, int y1, const RGBA& c, ImageDrawing::AlphaCombineMode am) {
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
	    pixel(image,y,x,c,am);
	} else {
	    pixel(image,x,y,c,am);
	}
	error += deltay;
	if (error > 0) {
	    y += ystep;
	    error -= deltax;
	}
    }
};

/**
 * Slow but using float and thus more precise
 */	     
void ImageDrawing::line_slow(Image* image, float x0, float y0, float x1, float y1, const RGBA& c, ImageDrawing::AlphaCombineMode am) {

    float xdelta = x0 - x1;
    float ydelta = y0 - y1;
    int xl = 0, yl = 0;
    for(uint32_t i = 0; i < 1000; i++) {
        float t = float(i) / 1000;
        int xt = x1 + t*xdelta;
        int yt = y1 + t*ydelta;
        if (i == 0 || xt != xl || yt != yl) {
            pixel(image, xt, yt, c, am);
        }
        xl = xt;
        yl = yt;
    }
};


void ImageDrawing::circle(Image* image, int x0, int y0, int r, const RGBA& c, ImageDrawing::AlphaCombineMode am) {
    int f = 1 - r;
    int ddF_x = 0;
    int ddF_y = -2 * r;
    int x = 0;
    int y = r;

    pixel(image, x0,y0+r,c,am);
    pixel(image, x0,y0-r,c,am);
    pixel(image, x0+r,y0,c,am);
    pixel(image, x0-r,y0,c,am);

    while(x < y) {
        if(f > 0) {
            y--;
            ddF_y += 2;
            f += ddF_y;
        }
        x++;
        ddF_x += 2;
        f += ddF_x + 1;    
        pixel(image, x0+x,y0+y,c,am);
        pixel(image, x0-x,y0+y,c,am);
        pixel(image, x0+x,y0-y,c,am);
        pixel(image, x0-x,y0-y,c,am);
        pixel(image, x0+y,y0+x,c,am);
        pixel(image, x0-y,y0+x,c,am);
        pixel(image, x0+y,y0-x,c,am);
        pixel(image, x0-y,y0-x,c,am);
    }
}

void ImageDrawing::quadraticBezierCurve(Image* image, float x0, float y0, float x1, float y1, float x2, float y2, const RGBA& c, ImageDrawing::AlphaCombineMode am) {
    int xl = 0, yl = 0;
    for(uint32_t i = 0 ; i < 1000; i++) {
        float t = float(i) / 1000;
        int xt = (1-t)*(1-t)*float(x0) + 2*t*(1-t)*float(x1) + t*t*float(x2);
        int yt = (1-t)*(1-t)*float(y0) + 2*t*(1-t)*float(y1) + t*t*float(y2);
        
        if (i == 0 || xt != xl || yt != yl) {
            pixel(image, xt, yt, c, am);
        }
        xl = xt;
        yl = yt;
    }        
}

void ImageDrawing::string(Image* image, int x, int y, std::wstring text, TrueTypeFont* font, int size, const RGBA& color, ImageDrawing::AlphaCombineMode am) {
    vector<TrueTypeFont::Glyph*> glyphs = font->getGlyphs(text);   
    
    for(uint32_t i = 0; i < glyphs.size(); i++) {
        TrueTypeFont::Glyph* glyph = glyphs[i];
        // In Arial ' ' points to the glyph '!'. We don't draw the '!' but just uses it's advance width. I couldn't find 
        // docs that says how to handle space, but this method seems quite sane.
        if (!font->isWhitespace(text[i])) {
            for(uint32_t j = 0; j < glyph->contours.size(); j++) {
                TrueTypeFont::Contour contour = glyph->contours[j];
                vector<Vector2> coords = contour.coords;
                coords.push_back(coords[0]);
                coords.push_back(coords[1]);
                vector<bool> onCurve = contour.onCurve;
                onCurve.push_back(onCurve[0]);
                onCurve.push_back(onCurve[1]);
                Vector2 c0 = coords[0];
                for(uint32_t k = 1; k < coords.size()-1; k++) {
                    Vector2 c1 = coords[k];
                    if (onCurve[k]) {
                        line_slow(image, c0[0]*size+x, y-c0[1]*size, c1[0]*size+x, y-c1[1]*size, color, am);        
                        c0 = c1;
                    } else {
                        Vector2 c2 = coords[k+1];
                        if (!onCurve[k+1]) {
                            // Reconstruct a new c2 that is on curve    
                            c2 = (c1 + c2) * 0.5;
                        }
                        quadraticBezierCurve(image, c0[0]*size+x, y-c0[1]*size, c1[0]*size+x, y-c1[1]*size, c2[0]*size+x, y-c2[1]*size, color, am);
                        c0 = c2;
                    }
                }
            }
        }
        if (i != glyphs.size() - 1) {
            float kerning = font->getKerning(text[i], text[i+1]); 
            x += (glyph->advanceWidth + kerning)* size;
        }
    }
}


