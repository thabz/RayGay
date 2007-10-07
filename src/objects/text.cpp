
#include "text.h"

////////////////////////////////////////////////////////////////////////
// Text code
////////////////////////////////////////////////////////////////////////

Text::Text(std::wstring text, TrueTypeFont* font, double depth, double size, const Material* material) {

    vector<TrueTypeFont::Glyph*> tt_glyphs = font->getGlyphs(text);

    double x = 0.0;        
    for(uint32_t i = 0; i < tt_glyphs.size(); i++) {
        TrueTypeFont::Glyph* tt_glyph = tt_glyphs[i];    
        if (!font->isWhitespace(text[i])) {
            Glyph* glyph = new Glyph(tt_glyph, material);
            glyph->transform(Matrix::matrixScale(Vector(size,size,depth)));
            glyph->transform(Matrix::matrixTranslate(Vector(x,0,0)));
            ObjectGroup::addObject(glyph);
        }        
        if (i != tt_glyphs.size() - 1) {
            float kerning = font->getKerning(text[i], text[i+1]); 
            x += (tt_glyph->advanceWidth + kerning) * size;
        }
    }
}

////////////////////////////////////////////////////////////////////////
// Glyph code
////////////////////////////////////////////////////////////////////////

Glyph::Glyph(TrueTypeFont::Glyph* glyph, const Material* material) {
    const Contours& contours = glyph->contours;        
    for(uint32_t m = 0; m < contours.contours.size(); m++) {
        const Contour& contour = contours.contours[m];    
        Vector2 c0 = contour.coords[0];
        for(uint32_t k = 1; k <= contour.coords.size(); k++) {
            uint32_t j = k % contour.coords.size();    
            Vector2 c1 = contour.coords[j];
            if (contour.onCurve[j]) {
                // Add linesegment from c0 to c1        
                ExtrudedLine* l = new ExtrudedLine(c0, c1, material);    
                ObjectGroup::addObject(l);
                c0 = c1;
            } else {
                uint32_t j = (k+1) % contour.coords.size();    
                Vector2 c2 = contour.coords[j];
                if (!contour.onCurve[j]) {
                    // Reconstruct a new c2 that is on curve    
                    c2 = (c1 + c2) * 0.5;
                }
                // Add curve from c0 via c1 to c2

                c0 = c2;
            }
        }
    }        
}

////////////////////////////////////////////////////////////////////////
// ExtrudedLine code
////////////////////////////////////////////////////////////////////////

ExtrudedLine::ExtrudedLine(const Vector2& c1, const Vector2& c2, const Material* material) : Object(material), c1(c1), c2(c2) {
}

AABox ExtrudedLine::getBoundingBox() const {
    return AABox(Vector(c1[0], c1[1], 0), Vector(c2[0], c2[1], -1));
}

void ExtrudedLine::transform(const Matrix& m) {
    Transformer::transform(m);        
}

SceneObject* ExtrudedLine::clone() const {
    return new ExtrudedLine(c1, c2, Object::getMaterial());    
}

void ExtrudedLine::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
        
}

double ExtrudedLine::_fastIntersect(const Ray& ray) const {
        
}

////////////////////////////////////////////////////////////////////////
// ExtrudedCurve code
////////////////////////////////////////////////////////////////////////
