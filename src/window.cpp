
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_GTK

#include <cassert>
#include "image/image.h"
#include "window.h"
#include "window-icon.h"

int darea_width;
int darea_height;
guchar* rgbbuf;

gboolean
on_darea_expose (GtkWidget *widget,
		 GdkEventExpose *event,
		 gpointer user_data)
{
  gdk_draw_rgb_image (widget->window, 
	              widget->style->fg_gc[GTK_STATE_NORMAL],
		      0, 0, darea_width, darea_height,
		      GDK_RGB_DITHER_MAX, rgbbuf, darea_width * 3);
  gtk_widget_queue_draw(widget);

  return TRUE;
}

PreviewWindow::PreviewWindow(int width, int height) {
    this->width = width;
    this->height = height;
    darea_width = width;
    darea_height = height;
    this->image = NULL;
    gtk_init(NULL,NULL);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(window),width,height);
    gtk_window_set_title(GTK_WINDOW(window),"RayGay preview");

    // Set window icon
    GdkPixbuf* icon_pix = gdk_pixbuf_new_from_inline(-1,my_pixbuf,FALSE,NULL);
    gtk_window_set_icon(GTK_WINDOW(window),icon_pix);

    // Prepare the drawable
    rgbbuf = new guchar[width*height*3];
    rgbbuf[0] = 255;
    rgbbuf[1] = 255;
    rgbbuf[2] = 255;
    darea = gtk_drawing_area_new ();
    gtk_widget_set_size_request (darea, width, height);
    gtk_container_add (GTK_CONTAINER (window), darea);
    gtk_signal_connect (GTK_OBJECT (darea), "expose-event",
                      GTK_SIGNAL_FUNC (on_darea_expose), NULL);
}

void* window_thread_do(void* obj) {
    GtkWidget* window = (GtkWidget*) obj;
    gtk_widget_show_all(window);
    gtk_main();
    return NULL;
}

void PreviewWindow::run() {
    pthread_create(&window_main_loop, NULL, window_thread_do, window);
}

void PreviewWindow::stop() {
    pthread_join(window_main_loop, NULL);
}

void PreviewWindow::drawBlock(int xb, int yb, int w, int h) {
    assert(image != NULL);
    for(int y = 0; y < h; y++) {
	for(int x = 0; x < w; x++) {
	    RGBA col = image->getRGBA(x+xb,y+yb);
	    guchar* p = rgbbuf + ((y+yb)*width + (x+xb))*3;
	    p[0] = (guchar) (255 * col.r());
	    p[1] = (guchar) (255 * col.g());
	    p[2] = (guchar) (255 * col.b());
	//    p[3] = (guchar) (255 * col.a());
	}
    }
    rgbbuf[0] = 255;
    rgbbuf[1] = 255;
    rgbbuf[2] = 255;
    rgbbuf[3] = 0;
    rgbbuf[4] = 0;
    rgbbuf[5] = 255;
    // TODO: Refresh drawing_area
}

void PreviewWindow::setProgress(double progress) {
    int p = int(100.0 * progress);
    char title[1000];
    sprintf(title,"RayGay (%d%%)",p);
    gtk_window_set_title(GTK_WINDOW(window),title);
}

#endif

