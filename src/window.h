
#ifdef HAS_GTK

#ifndef PREVIEW_WINDOW_H
#define PREVIEW_WINDOW_H

#include <gtk/gtk.h>

class Image;

class PreviewWindow {

    public:
	PreviewWindow(int argc, char* argv[]);
	void setSize(int width, int height);
	void drawBlock(Image* img, int x, int y, int w, int h);
	void run();
	void stop();

    private:
	pthread_t window_main_loop;
	GtkWidget *window;
	GtkWidget *drawing_area;
	GdkPixmap *pixmap;
};

#endif /* PREVIEW_WINDOW_H */
#endif /* HAS_GTK */

