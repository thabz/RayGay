
#include "image/imagedrawing.h"
#include "image/image.h"
#include <algorithm>

/*
 function line(x0, x1, y0, y1)
     boolean steep := abs(y1 - y0) > abs(x1 - x0)
     if steep then
         swap(x0, y0)
         swap(x1, y1)
     if x0 > x1 then
         swap(x0, x1)
         swap(y0, y1)
     int deltax := x1 - x0
     int deltay := abs(y1 - y0)
     int error := -deltax / 2
     int ystep
     int y := y0
     if y0 < y1 then ystep := 1 else ystep := -1
     for x from x0 to x1
         if steep then plot(y,x) else plot(x,y)
         error := error + deltay
         if error > 0 then
             y := y + ystep
             error := error - deltax

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

