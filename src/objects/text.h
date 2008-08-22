
#ifndef RAYGAY_OBJECTS_TEXT_H
#define RAYGAY_OBJECTS_TEXT_H

#include "objects/objectgroup.h"
#include "object.h"
#include "transformer.h"
#include "ttf.h"

class ExtrudedLine : public Object, public Transformer 
{
    public:
        ExtrudedLine(const Vector2& c1, const Vector2& c2, const Material* material);
        AABox getBoundingBox() const;    	
	    void transform(const Matrix& m);
	    SceneObject* clone() const;
	
    protected:
	    void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	    double _fastIntersect(const Ray& ray) const;
        
    private:
        Vector2 c1;
        Vector2 c2;
};

class ExtrudedCurve : public Object, public Transformer 
{
    public:
        ExtrudedCurve(const Vector2& p0, const Vector2& p1, const Vector2& p2, const Material* material);
        AABox getBoundingBox() const;    	
	    void transform(const Matrix& m);
	    SceneObject* clone() const;
	
    protected:
	    void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	    double _fastIntersect(const Ray& ray) const;
        
    private:
        double findClosestT(const Ray& ray) const;
        Vector2 b(double t) const;
        Vector2 p0, p1, p2;
};


// A Glyph is an objectgroup of ExtrudedLines and ExtrudedCurves.
class Glyph : public ObjectGroup
{
    public:
        Glyph(TrueTypeFont::Glyph* glyph, const Material* material);
                
};

// A Text (the main class for rendering text) is an objectgroup of Glyphs.
class Text : public ObjectGroup
{
    public:
       Text(std::wstring text, TrueTypeFont* font, double size, double depth, const Material* material);
};

#endif

