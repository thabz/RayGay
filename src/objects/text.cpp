
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
                // Continue from control point c1
                c0 = c1;
            } else {
                uint32_t j = (k+1) % contour.coords.size();    
                Vector2 c2 = contour.coords[j];
                if (!contour.onCurve[j]) {
                    // Reconstruct a new c2 that is on curve    
                    c2 = (c1 + c2) * 0.5;
                }
                // Add curve from c0 via c1 to c2
                ExtrudedCurve* c = new ExtrudedCurve(c0, c1, c2, material);
                ObjectGroup::addObject(c);
                // Continue from control point c2
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
    return AABox(Vector(c1[0], c1[1], 0), Vector(c2[0], c2[1], 1));
}

void ExtrudedLine::transform(const Matrix& m) {
    Transformer::transform(m);        
}

SceneObject* ExtrudedLine::clone() const {
    return new ExtrudedLine(c1, c2, Object::getMaterial());    
}

void ExtrudedLine::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    // Not implemented    
}

double ExtrudedLine::_fastIntersect(const Ray& ray) const {
    // Not implemented
    return -1;
}

////////////////////////////////////////////////////////////////////////
// ExtrudedCurve code
////////////////////////////////////////////////////////////////////////

ExtrudedCurve::ExtrudedCurve(const Vector2& p0, const Vector2& p1, const Vector2& p2, const Material* material) : Object(material), p0(p0), p1(p1), p2(p2) {
}

AABox ExtrudedCurve::getBoundingBox() const {
    Vector points[6];
    points[0] = Vector(p0[0], p0[1], 0-EPSILON);
    points[1] = Vector(p0[0], p0[1], 1+EPSILON);
    points[2] = Vector(p1[0], p1[1], 0-EPSILON);
    points[3] = Vector(p1[0], p1[1], 1+EPSILON);
    points[4] = Vector(p2[0], p2[1], 0-EPSILON);
    points[5] = Vector(p2[0], p2[1], 1+EPSILON);
    return AABox::enclosure(points, 6);
}

void ExtrudedCurve::transform(const Matrix& m) {
    Transformer::transform(m);        
}

SceneObject* ExtrudedCurve::clone() const {
    return new ExtrudedCurve(p0,p1,p2, Object::getMaterial());
}

Vector2 ExtrudedCurve::b(double t) const {
    return (1-t)*(1-t)*p0 + 2*t*(1-t)*p1 + t*t*p2;    
}

double ExtrudedCurve::findClosestT(const Ray& ray) const {
    Vector d = ray.getDirection();
    Vector o = ray.getOrigin();
    double A = d.x() * (p0.y() - 2*p1.y() + p2.y())  - d.y() * (p0.x() - 2*p1.x() + p2.x());
    double B = d.x() * (2*p0.y() + 2*p1.y()) - d.y() * (2*p0.x() + 2*p1.x());
    double C = d.x() * (p0.y() - o.y()) - d.y() * (p0.x() + o.x());
    double D = B*B - 4*A*C;
    if (D < 0) return -1;
    double t0 = (-B - D) / 2*A;
    double t1 = (-B + D) / 2*A;
    double u0 = -1, u1 = -1;
    if (t0 >= 0 && t0 <= 1) {
        double s = (b(t0).x() - o.x()) / d.x();
        if (s >= 0) {
            u0 = o.z() + d.z() * s;
            if (u0 < 0 || u0 > 1) u0 = -1;
        }
    }
    if (t1 >= 0 && t1 <= 1) {
        double s = (b(t1).x() - o.x()) / d.x();
        s = (s - o.x()) / d.x();
        if (s >= 0) {
            u1 = o.z() + d.z() * s;
            if (u1 < 0 || u1 > 1) u1 = -1;
        }
    }
    if (u0 >= 0 && u1 >= 0 && u0 < u1) return t0;
    if (u1 >= 0 && u1 >= 0) return t1;
    if (u0 >= 0) return t0;
    if (u1 >= 0) return t1;
    return -1;
}

// The following math is explained in "docs/Quadratic bezier ray intersection.pdf".
double ExtrudedCurve::_fastIntersect(const Ray& ray) const {
    double t = findClosestT(ray);
    double u = -1;
    if (t >= 0 && t <= 1) {
        Vector d = ray.getDirection();
        Vector o = ray.getOrigin();
        double s = (b(t).x() - o.x()) / d.x();
        if (s >= 0) {
            u = o.z() + d.z() * s;
            if (u < 0 || u > 1) u = -1;
        }
    }
    return u >= 0 && u <= 1 ? u : -1;
}

void ExtrudedCurve::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    Vector p = ray.getPoint(t);
    Vector2 along = b(findClosestT(ray) + EPSILON);
    Vector p2 = Vector(along.x(),along.y(),p.z());
    Vector n = Vector::xProduct(p,p2);
    n.normalize();
    Vector2 uv; // No support for UV-coordinates
    result = Intersection(p,t,n,uv);
}
