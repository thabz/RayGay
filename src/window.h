
#ifndef PREVIEW_WINDOW_H
#define PREVIEW_WINDOW_H

#include "window.h"

class Image;

class PreviewWindow {

    public:
	virtual void drawBlock(int x, int y, int w, int h) = 0;
	virtual void setImage(Image* img) = 0;
	virtual void run() = 0;
	virtual void stop() = 0;
	virtual void setProgress(double progress) = 0;
};

#endif /* PREVIEW_WINDOW_H */

