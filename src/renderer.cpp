
#include "renderer.h"
#include "image.h"
#include "scene.h"
#include "rgb.h"

Renderer::Renderer() {
}

void Renderer::render(Scene* sc, Image* img) {
    scene = sc;
    scene->prepare();
    Ray ray;
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    for (double y = -img_h; y < img_h; y++) {
        for (double x = -img_w; x < img_w; x++) {
	    RGB color = getPixel(x,y);
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
    }
}
