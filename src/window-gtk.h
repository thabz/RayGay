
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_GTK

#ifndef PREVIEW_WINDOW_GTK_H
#define PREVIEW_WINDOW_GTK_H

#undef MIN
#include <gtk/gtk.h>
#include <gdk/gdk.h>

class PreviewWindowGTK : public PreviewWindow {

    public:
	PreviewWindowGTK(int w, int h, void (*abortRenderingCB)(void));
	void drawBlock(int x, int y, int w, int h);
	void setImage(Image* img) { this->image = img; };
	void run();
	void stop();
	void setProgress(double progress);

    private:
	pthread_t window_main_loop;
	GtkWidget *window;
	GtkWidget *darea;
	GtkProgressBar* progress_bar;
	
	Image* image;
	int width;
	int height;
};

#endif /* PREVIEW_WINDOW_GTK_H */

#endif /* HAVE_GTK */
