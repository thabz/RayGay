
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
	PreviewWindow();
	void setSize(int width, int height);
	void drawBlock(Image* img, int x, int y, int w, int h);
	void run();
	void stop();
	void setProgress(double progress);

    private:
	pthread_t window_main_loop;
	GtkWidget *window;
	GtkWidget *drawing_area;
	GdkPixbuf *pixbuf;
};

#endif /* PREVIEW_WINDOW_H */
#endif /* HAVE_GTK */

