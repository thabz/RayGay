
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_GTK

#ifndef PREVIEW_WINDOW_H
#define PREVIEW_WINDOW_H

#undef MIN
#include <gtk/gtk.h>

class Image;

class PreviewWindow {

    public:
	PreviewWindow(int w, int h);
	void drawBlock(int x, int y, int w, int h);
	void setImage(Image* img) { this->image = img; };
	void run();
	void stop();
	void setProgress(double progress);

    private:
	pthread_t window_main_loop;
	GtkWidget *window;
	GtkWidget *darea;
	
	Image* image;
	int width;
	int height;
};

#endif /* PREVIEW_WINDOW_H */
#endif /* HAVE_GTK */

