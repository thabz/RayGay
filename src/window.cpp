
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_GTK

#include <cassert>
#include <pthread.h>
#include <iostream>
#include "image/image.h"
#include "window.h"
#include "window-icon.h"
#include <glib.h>
#include <gdk/gdkkeysyms.h>

int darea_width;
int darea_height;
guchar* rgbbuf;
bool window_open;
double zoom_scale;
int zoom_center_x;
int zoom_center_y;
void (*abortRenderingCB)(void);

gboolean on_darea_expose (GtkWidget *widget,
	GdkEventExpose *event,
	gpointer user_data) {
    gdk_draw_rgb_image (widget->window, 
	    widget->style->fg_gc[GTK_STATE_NORMAL],
	    0, 0, darea_width, darea_height,
	    GDK_RGB_DITHER_NONE, rgbbuf, darea_width * 3);
    return TRUE;
}

gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
    window_open = false;
    abortRenderingCB();
    gtk_main_quit();
    // TODO: Possible race. Window might be closed before all
    // renderthreads are done aborting and thus are trying to plot
    // some pixels.
    return FALSE;
}

gboolean keypress_event(GtkWidget* widget, GdkEventKey *event) {
    if (event->keyval == GDK_q)
    {
	window_open = false;
	abortRenderingCB();
	gtk_main_quit();
	return FALSE;
    }
    return TRUE;
}

PreviewWindow::PreviewWindow(int width, int height, void (*abc)(void)) {
    this->width = width;
    this->height = height;
    darea_width = width;
    darea_height = height;
    this->image = NULL;
    abortRenderingCB = abc;

    zoom_scale = 4;
    zoom_center_x = width / 2;
    zoom_center_y = height / 2;

    g_thread_init(NULL);
    gdk_threads_init();
    gtk_init(NULL,NULL);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_default_size(GTK_WINDOW(window),width,height);
    gtk_window_set_title(GTK_WINDOW(window),"RayGay preview");

    // Set window icon
    GdkPixbuf* icon_pix = gdk_pixbuf_new_from_inline(-1,my_pixbuf,FALSE,NULL);
    gtk_window_set_icon(GTK_WINDOW(window),icon_pix);

    // HBox
    GtkWidget* hbox = gtk_vbox_new(false,10); 

    // Insert the progress bar
    progress_bar = (GtkProgressBar*)gtk_progress_bar_new();
    gtk_box_pack_start(GTK_BOX(hbox),GTK_WIDGET(progress_bar),false,false,0);

    // Prepare the drawable
    rgbbuf = new guchar[width*height*3];
    darea = gtk_drawing_area_new ();
    gtk_widget_set_double_buffered(darea,false);
    gtk_widget_set_size_request (darea, width, height);
 //   gtk_widget_set_events (window, GDK_KEY_PRESS_MASK);
    gtk_signal_connect (GTK_OBJECT (darea), "expose-event",
	    GTK_SIGNAL_FUNC (on_darea_expose), NULL);
    gtk_signal_connect (GTK_OBJECT (window), "delete_event",
	    GTK_SIGNAL_FUNC (delete_event), NULL);
    gtk_signal_connect (GTK_OBJECT (window), "key-press-event",
	    GTK_SIGNAL_FUNC (keypress_event), NULL);
    gtk_widget_show(darea);

    // Scrolled window
    GtkWidget* scroll_window = gtk_scrolled_window_new(NULL,NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_window),GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroll_window),darea);
    gtk_box_pack_start(GTK_BOX(hbox),GTK_WIDGET(scroll_window),true,true,0);
    gtk_container_add (GTK_CONTAINER (window), hbox);
}

void* window_thread_do(void* obj) {
    GtkWidget* window = (GtkWidget*) obj;
    gtk_widget_show_all(window);
    window_open = true;
    gtk_main();
    return NULL;
}

void PreviewWindow::run() {
    pthread_create(&window_main_loop, NULL, window_thread_do, window);
}

void PreviewWindow::stop() {
    if (window_open) {
	pthread_join(window_main_loop, NULL);
    }
}

void PreviewWindow::drawBlock(int xb, int yb, int w, int h) {
    assert(image != NULL);
    if (window_open) {
	for(int y = 0; y < h; y++) {
	    for(int x = 0; x < w; x++) {
		RGBA col = image->getRGBA(x+xb,height-1-(y+yb));
		guchar* p = rgbbuf + ((height-1-(y+yb))*width + (x+xb))*3;
		p[0] = col.r() > 1.0 ? 255 : (guchar) (255.0 * col.r());
		p[1] = col.g() > 1.0 ? 255 : (guchar) (255.0 * col.g());
		p[2] = col.b() > 1.0 ? 255 : (guchar) (255.0 * col.b());
		//    p[3] = (guchar) (255 * col.a());
	    }
	}
	gdk_threads_enter();
	gtk_widget_queue_draw_area(window,xb,yb,w,h);
	gdk_flush();
	gdk_threads_leave();
    }
}

void PreviewWindow::setProgress(double progress) {
    if (window_open) {
	gdk_threads_enter();
	gtk_progress_bar_set_fraction(progress_bar,progress);
	int p = int(100.0 * progress);
	char title[1000];
	sprintf(title,"RayGay (%d%%)",p);
	gtk_window_set_title(GTK_WINDOW(window),title);
	gdk_flush();
	gdk_threads_leave();
    }
}

#endif

