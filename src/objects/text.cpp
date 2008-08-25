
#include "text.h"

////////////////////////////////////////////////////////////////////////
// Text code
////////////////////////////////////////////////////////////////////////

Text::Text(std::wstring text, TrueTypeFont* font, double size, double depth, const Material* material) {

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
                Vector2 middle = (c0 + c1) * 0.5;
                //ExtrudedLine* l = new ExtrudedLine(c0, c1, material);    
                ExtrudedCurve* c = new ExtrudedCurve(c0, c1, c1, material);
                ObjectGroup::addObject(c);
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
    GlyphFace* front = new GlyphFace(glyph, 1, material);
    front->transform(Matrix::matrixTranslate(Vector(0,0,1)));
    GlyphFace* back = new GlyphFace(glyph, -1, material);
    ObjectGroup::addObject(front);
    ObjectGroup::addObject(back);
}

////////////////////////////////////////////////////////////////////////
// GlyphFace code
////////////////////////////////////////////////////////////////////////
GlyphFace::GlyphFace(TrueTypeFont::Glyph* glyph, double z_direction, const Material* material) : Object(material), glyph(glyph) {
    normal = Vector(0,0,z_direction);
    normal.normalize();
}

AABox GlyphFace::getBoundingBox() const {
    double e = 100*EPSILON;
    return bboxToWorld(AABox(Vector(glyph->xMin, glyph->yMin, -e),
                             Vector(glyph->xMax, glyph->yMax, e)));
}

void GlyphFace::transform(const Matrix& m) {
    Transformer::transform(m);        
}

SceneObject* GlyphFace::clone() const {
    return new GlyphFace(glyph, normal.z(), Object::getMaterial());
}

double GlyphFace::_fastIntersect(const Ray& world_ray) const {
    Ray ray = rayToObject(world_ray);
    Vector d = ray.getDirection();
    Vector o = ray.getOrigin();
    
    // Find the point p where the ray intersects the xy-plane
    if (d.z() == 0) return -1;
    double t = -o.z() / d.z();
    if (t < 0) return -1;
    
    Vector pp = ray.getPoint(t);
    Vector2 p = Vector2(pp.x(), pp.y());
    
    // Make sure that the intersection point p is inside the glyph.
    if (!glyph->isInside(p)) return -1;
    
    // Return the t in world scale
    return t / ray.t_scale;
}

void GlyphFace::_fullIntersect(const Ray& world_ray, const double world_t, Intersection& result) const {
    Ray ray = rayToObject(world_ray);
    double t = world_t*ray.t_scale;
    
    Vector p = ray.getPoint(t);
    Vector2 uv; // No support for UV-coordinates
    result = Intersection(p,t,normal,uv);
    intersectionToWorld(result);
}


////////////////////////////////////////////////////////////////////////
// ExtrudedLine code
////////////////////////////////////////////////////////////////////////

ExtrudedLine::ExtrudedLine(const Vector2& c1, const Vector2& c2, const Material* material) : Object(material), c1(c1), c2(c2) {
}

AABox ExtrudedLine::getBoundingBox() const {
    return bboxToWorld(AABox(Vector(c1[0], c1[1], -EPSILON), Vector(c2[0], c2[1], 1+EPSILON)));
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
    double e = 100*EPSILON;
    points[0] = Vector(p0[0]-e, p0[1]-e,  -e);
    points[1] = Vector(p0[0]+e, p0[1]+e, 1+e);
    points[2] = Vector(p1[0]-e, p1[1]-e,  -e);
    points[3] = Vector(p1[0]+e, p1[1]+e, 1+e);
    points[4] = Vector(p2[0]-e, p2[1]-e,  -e);
    points[5] = Vector(p2[0]+e, p2[1]+e, 1+e);
    return bboxToWorld(AABox::enclosure(points, 6));
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

// The following math is explained in "docs/Quadratic bezier ray intersection.pdf".
double ExtrudedCurve::findClosestT(const Ray& ray) const {
    Vector d = ray.getDirection();
    Vector o = ray.getOrigin();
    double A = d.x() * (p0.y() - 2*p1.y() + p2.y())  - d.y() * (p0.x() - 2*p1.x() + p2.x());
    double B = d.x() * (2*p1.y()- 2*p0.y()) - d.y() * (2*p1.x() - 2*p0.x());
    double C = d.x() * (p0.y() - o.y()) - d.y() * (p0.x() - o.x());
    double D = B*B - 4*A*C;
    if (D < 0) return -1;
    D = sqrt(D);
    double t0 = (-B - D) / (2*A);
    double t1 = (-B + D) / (2*A);
    double s0 = -1, s1 = -1;
    if (t0 >= 0 && t0 <= 1) {
        s0 = (b(t0).x() - o.x()) / d.x();
        if (s0 >= 0) {
            double u = o.z() + d.z() * s0;
            if (u < 0 || u > 1) t0 = -1;
        }
    }
    if (t1 >= 0 && t1 <= 1) {
        s1 = (b(t1).x() - o.x()) / d.x();
        if (s1 >= 0) {
            double u = o.z() + d.z() * s1;
            if (u < 0 || u > 1) t1 = -1;
        }
    }
    if (s0 >= 0 && s1 >= 0 && s0 < s1) return t0;
    if (s0 >= 0 && s1 >= 0) return t1;
    if (s0 >= 0) return t0;
    if (s1 >= 0) return t1;
    return -1;
}

double ExtrudedCurve::_fastIntersect(const Ray& world_ray) const {
    Ray ray = rayToObject(world_ray);
    double t = findClosestT(ray);
    double u = -1, s = -1;
    if (t >= 0 && t <= 1) {
        Vector d = ray.getDirection();
        Vector o = ray.getOrigin();
        s = (b(t).x() - o.x()) / d.x();
        if (s >= 0) {
            u = o.z() + d.z() * s;
            if (u < 0 || u > 1) s = -1;
        }
        if (s < 0) s = -1;
    }
    return s / ray.t_scale;
}

void ExtrudedCurve::_fullIntersect(const Ray& world_ray, const double world_t, Intersection& result) const {
    Ray ray = rayToObject(world_ray);
    double t = world_t*ray.t_scale;
    
    Vector p = ray.getPoint(t);
    Vector2 along = b(findClosestT(ray) + EPSILON);
    Vector p2 = p - Vector(along.x(), along.y(), p.z());
    Vector p3 = p - Vector(p.x(), p.y(), p.z() - EPSILON);
    Vector n = Vector::xProduct(p3,p2);
    n.normalize();
    Vector2 uv; // No support for UV-coordinates
    result = Intersection(p,t,n,uv);
    intersectionToWorld(result);
}
