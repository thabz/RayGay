
#ifndef RAYGAY_TRUETYPEFONT_H
#define RAYGAY_TRUETYPEFONT_H

#include <string>
#include <iostream>
#include <istream>
#include <fstream>
#include <list>
#include <vector>

using namespace std;

class TrueTypeFont 
{
    public:
        class Coord {
            public:        
                Coord(float x, float y) : x(x), y(y) {};        
                float x,y;    
        };
        
        // A contour is a closed shape
        class Contour {
            public:
                Contour(uint16_t t) : type(t) {};             
                uint16_t type;    // 0 = lines, 1 = cur
                vector<Coord> coords;
        };
        
        class Line : public Contour {
            public:        
                Line() : Contour(0) {};    
        };
        
        class Curve : public Contour {
            public:
                Curve() : Contour(1) {};            
        };
        
        struct Glyph {
            float advanceWidth;
            float leftSideBearing;
            float xMin, xMax;
            float yMin, yMax;
            // rsb = aw - (lsb + xmax - xmin)            
            vector<Contour> contours;
            float xOffset;
        };

    public:
        TrueTypeFont(string filename);
        ~TrueTypeFont();
        
        vector<Glyph*> getGlyphs(string s, uint32_t pts);
        Glyph* getGlyph(char c, uint32_t pts);
        
    private:
            
        void read_glyf_table(uint32_t offset);
        void read_cmap_table(uint32_t offset);
        void read_head_table(uint32_t offset);
        void read_maxp_table(uint32_t offset);
        void read_loca_table(uint32_t offset);
        void read_hmtx_table(uint32_t offset);
        void read_hhea_table(uint32_t offset);
        
        uint32_t read_uint32();           
        uint16_t read_uint16();
        int16_t read_int16();
        uint8_t read_uint8();
        void read_struct(char* types, char* addr, uint32_t bytes);
        
        string filename;
        ifstream* is;
        uint32_t* glyphOffsets;
        uint32_t glyf_table_offset;
        uint32_t glyf_table_length;
        uint16_t indexToLocFormat;
        uint16_t numOfLongHorMetrics;
        uint16_t unitsPerEm;
        uint16_t numGlyphs;
        uint8_t glyphIndexArray[256];
    
        Glyph** glyphs;
        Glyph* getGlyph(uint32_t glyphIndex);    

        void processSimpleGlyph(Glyph* glyph, int16_t numberOfContours);
        void processCompoundGlyph(Glyph* glyph);
        void processGlyph(Glyph* glyph, uint32_t glyphIndex);
};

#endif