
#ifdef HAS_GTK

#include "window.h"
#include "image/image.h"
#include "window-icon.h"

PreviewWindow::PreviewWindow(int argc, char* argv[]) {
    gtk_init(&argc,&argv);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_name(GTK_WINDOW(window),"RayGay preview");

    // Set window icon
    GdkPixbuf* icon_pix = gdk_pixbuf_new_from_inline(-1,my_pixbuf,FALSE,NULL);
    gtk_window_set_icon(window,icon_pix);

    drawing_area = gtk_drawing_area_new();
}

void PreviewWindow::setSize(int width, int height) {
    gtk_widget_set_size_request(GTK_WIDGET(drawing_area),width,height);
}

void* window_thread_do(void* obj) {
    gtk_widget_show(window);
    gtk_main();
}

void PreviewWindow::run() {
    pthread_create(&window_main_loop, NULL, window_thread_do, NULL);
}

void PreviewWindow::stop() {
    pthread_join(window_main_loop, NULL);
}

void PreviewWindow::drawBlock(Image* img, int xb, int yb, int w, int h) {

    int row_stride, n_channels;
    guchar* pixels;
    guchar* p;

    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    for(int y = 0; y < h; y++) {
	for(int x = 0; x < w; x++) {
	    RGBA col = image->getPixel(x+xb,y+yb);
	    p = pixels + (y+yb)*rowstride + (x+xb)*n_channels;
	    p[0] = 255 * col.r();
	    p[1] = 255 * col.g();
	    p[2] = 255 * col.b();
	    p[3] = 255 * col.a();
	}
    }
    // TODO: Refresh drawing_area
}

#endif

