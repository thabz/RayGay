/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Copyright 2003 by Jesper Christensen <jesper@kalliope.org>
 *
 * Added Wed Jun 18 2003
 */

#ifndef PIXELSTORE_H
#define PIXELSTORE_H

#include "rgb.h"
class Pixel;

/// A cache for adaptive antialiasing

class PixelStore {
    
    public:
	/// Constructor
	PixelStore(int width, int height, int depth);

	/// Destructor
	~PixelStore();
	void switchRow(double y);
	void setColor(double x, double y, const RGB color);
	RGB getColor(double x, double y) const;
	Pixel getPixel(double x, double y, int depth) const;
	
	/// Internal test
	static void test();

    private:
	RGB* store;
	double topRow;
	double depth;
	int width;
	int stride;  // Stored pixels per line
	int getIndex(double x, double y) const;
};


#endif
