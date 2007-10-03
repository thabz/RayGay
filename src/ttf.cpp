
#include "ttf.h"
#include "exception.h"
#include <iomanip>

#define ids2kernkey(l,r) (((l) << 16) | (r))

struct OffsetSubtable {
    uint32_t scalerType;
    uint16_t numTables;
    uint16_t searchRange;
    uint16_t entrySelector;
    uint16_t rangeShift;    
};

struct TableDirectoryEntry {
    uint8_t tag[4];
    uint32_t checkSum;
    uint32_t offset;
    uint32_t length;            
};

struct CmapEncodingSubtable {
    uint16_t platformID;        
    uint16_t platformSpecificID;
    uint32_t offset;
};

struct CmapIndex {
    uint16_t version;        
    uint16_t numberSubtables;
};

struct CmapSubtableHeader {
    uint16_t format;        
    uint16_t length;
    uint16_t language;
};

struct HeadTable {
    uint32_t ignored_1[3];
    uint32_t magicNumber;       // Is always set to 0x5F0F3CF5
    uint16_t flags;
    uint16_t unitsPerEm;        // Range from 64 to 16864
    uint16_t ignored_2[15];
    int16_t indexToLocFormat;   // 0 for short offsets, 1 for long
};

struct MaximumProfileTable {
    uint32_t version          __attribute__ ((packed));
    uint16_t numGlyphs        __attribute__ ((packed));
    uint16_t ignored_2[13]    __attribute__ ((packed));
};

struct HorizontalHeader {
    uint32_t version;
    uint16_t ascent;
    uint16_t descent;
    uint16_t lineGap;
    int16_t advanceWidthMax;
    uint16_t minLeftSideBearing;
    uint16_t minRightSideBearing;
    uint16_t xMaxExtent;          // max(lsb + (xMax-xMin))
    uint16_t ignored[8];
    uint16_t numOfLongHorMetrics;
};

struct KerningSubtable {
    uint16_t version;
    uint16_t length;
    uint16_t coverage;            
};

struct GlyphDescription {
    int16_t numberOfContours;        
    int16_t xMin;
    int16_t yMin;
    int16_t xMax;
    int16_t yMax;
};

TrueTypeFont::TrueTypeFont(string filename) {
    this->glyphOffsets = NULL;
    this->is = NULL;
    this->glyphs = NULL;
    this->advanceWidths = NULL;
    this->leftSideBearings = NULL;
    this->endCode = this->startCode = this->idDelta = this->idRangeOffset = NULL;
    this->glyphIndexArray = NULL;
    this->filename = filename;
        
    this->is = new ifstream(filename.c_str(), ios::in|ios::binary);
    if (is->bad()) {
        throw_exception("Can't open " + filename);    
    }
    
    OffsetSubtable offsetSubtable;
    read_struct("issss", (char*)&offsetSubtable, sizeof(OffsetSubtable));
    //cout << "Scalertype: " << hex << offsetSubtable.scalerType << endl;
    //cout << "Num tables " << hex << offsetSubtable.numTables << endl;
    
    glyf_table_offset = 0;
    uint32_t cmap_table_offset = 0;
    uint32_t head_table_offset = 0;
    uint32_t loca_table_offset = 0;
    uint32_t maxp_table_offset = 0;
    uint32_t hmtx_table_offset = 0;
    uint32_t hhea_table_offset = 0;
    uint32_t kern_table_offset = 0;

    TableDirectoryEntry entry;
    for(uint32_t i = 0; i < offsetSubtable.numTables; i++) {
        read_struct("bbbbiii", (char*)&entry, sizeof(TableDirectoryEntry));
        string tag = string((char*)entry.tag,4);
        if (tag == "glyf") {
            glyf_table_offset = entry.offset;        
            glyf_table_length = entry.length;
        } else if (tag == "cmap") {
            cmap_table_offset = entry.offset;        
        } else if (tag == "loca") {
            loca_table_offset = entry.offset;        
        } else if (tag == "head") {
            head_table_offset = entry.offset;        
        } else if (tag == "hhea") {
            hhea_table_offset = entry.offset;        
        } else if (tag == "hmtx") {
            hmtx_table_offset = entry.offset;        
        } else if (tag == "kern") {
            kern_table_offset = entry.offset;        
        } else if (tag == "maxp") {
            maxp_table_offset = entry.offset;        
        }
        //cout << "Tag: " << tag << ", length " << dec << entry.length << ", offset 0x" << hex << entry.offset << endl;
    }
    
    if (glyf_table_offset == 0 || cmap_table_offset == 0 || loca_table_offset == 0 
                               || head_table_offset == 0 || maxp_table_offset == 0
                               || hmtx_table_offset == 0 || hhea_table_offset == 0) {
        throw_exception("Can't find all font headers in " + filename);    
    }
    
    // The order is not unimportant below. Head and maxp must be read before loca.
    read_head_table(head_table_offset);   // Find indexToLocFormat and unitsPerEm
    read_maxp_table(maxp_table_offset);   // Find numGlyphs
    read_loca_table(loca_table_offset);   // Find glyphOffsets[numGlyphs]

    glyphs = new Glyph*[numGlyphs];    
    for(uint16_t i = 0; i < numGlyphs; i++) {
        glyphs[i] = NULL;    
    }

    read_hhea_table(hhea_table_offset);    // Find numOfLongHorMetrics
    read_hmtx_table(hmtx_table_offset);    // Fill in advanceWidths and leftSideBearings
    
    read_glyf_table(glyf_table_offset);
    read_cmap_table(cmap_table_offset);
    
    if (kern_table_offset != 0) {
        read_kern_table(kern_table_offset);    
    }
};

TrueTypeFont::~TrueTypeFont() {
    if (glyphOffsets != NULL) {
        delete [] glyphOffsets;        
    }
    if (is != NULL) {
        is->close();
        delete is;        
    }
    if (glyphs != NULL) {
        for(uint32_t i = 0; i < numGlyphs; i++) {
             if (glyphs[i] != NULL) delete glyphs[i];         
         }
         delete [] glyphs;
    }
    
    if (endCode != NULL) {
        delete [] endCode;
        delete [] startCode;
        delete [] idDelta;
        delete [] idRangeOffset;
        delete [] glyphIndexArray;
    }
}

void TrueTypeFont::read_head_table(uint32_t offset) {
    is->seekg(offset);
    HeadTable headTable;
    read_struct("iiiissssssssssssssssssS", (char*)&headTable, sizeof(HeadTable));
    indexToLocFormat = headTable.indexToLocFormat;
    unitsPerEm = headTable.unitsPerEm;
    if (indexToLocFormat != 1 && indexToLocFormat != 0) {
        throw_exception("Invalid indexToLocFormat in " + filename);    
    }
    if (unitsPerEm < 16 || unitsPerEm > 16384) {
        throw_exception("Invalid unitsPerEm in " + filename);    
    }
    if (headTable.magicNumber != 0x5f0f3cf5) {
        throw_exception("Wrong magic number in " + filename);    
    }
}

void TrueTypeFont::read_maxp_table(uint32_t offset) {
    is->seekg(offset);
    MaximumProfileTable maxpTable;
    read_struct("issssssssssssss", (char*)&maxpTable, sizeof(MaximumProfileTable));
    numGlyphs = maxpTable.numGlyphs;
//    cout << "numGlyphs: " << dec << numGlyphs << endl;
    if (numGlyphs == 0) {
        throw_exception("No glyphs in found in " + filename);    
    }
    if (maxpTable.version != 0x00010000) {
        throw_exception("Wrong maxp.version in " + filename);    
    }
}

void TrueTypeFont::read_loca_table(uint32_t loca_table_offset) {
    is->seekg(loca_table_offset);
    glyphOffsets = new uint32_t[numGlyphs];
    for(uint16_t i = 0; i < numGlyphs; i++) {
        uint32_t offset;
        if (indexToLocFormat == 1) {
            offset = read_uint32();        
        } else {
            offset = uint32_t(read_uint16()) * 2;
        }
        glyphOffsets[i] = offset;   
        if (offset >= glyf_table_length) {
            throw_exception("Glyph offset out of bounds in " + filename);        
        }     
    }
}

void TrueTypeFont::read_hhea_table(uint32_t offset) {
    is->seekg(offset);
    HorizontalHeader horizontalHeader;
    read_struct("isssSssssssssssss", (char*)&horizontalHeader, sizeof(HorizontalHeader));
    numOfLongHorMetrics = horizontalHeader.numOfLongHorMetrics;
    if (numOfLongHorMetrics > numGlyphs) {
        throw_exception("Invalid numOfLongHorMetrics in " + filename);    
    }
}

void TrueTypeFont::read_hmtx_table(uint32_t offset) {
    is->seekg(offset);
    uint16_t advanceWidth = 0;
    advanceWidths = new uint16_t[numGlyphs];
    leftSideBearings = new int16_t[numGlyphs];
    for(uint16_t i = 0; i < numOfLongHorMetrics; i++) {
        advanceWidth = read_uint16();
        leftSideBearings[i] = read_int16();
        advanceWidths[i] = advanceWidth;
    }
    for(uint16_t i = numOfLongHorMetrics; i < numGlyphs; i++) {
        leftSideBearings[i] = read_int16();
        advanceWidths[i] = advanceWidth;
    }
}

void TrueTypeFont::read_kern_table(uint32_t offset) {
    is->seekg(offset);
    read_uint16();    // Ignore version
    uint16_t nTables = read_uint16();
    for(uint16_t i = 0; i < nTables; i++) {
        KerningSubtable kerningSubtable;
        read_struct("sss", (char*)&kerningSubtable, sizeof(KerningSubtable));
        uint8_t format = kerningSubtable.coverage >> 8;
        uint8_t horizontal = kerningSubtable.coverage & 1;
        uint8_t minimum = kerningSubtable.coverage & 2;
        uint8_t crossStream = kerningSubtable.coverage & 4;
        if (horizontal != 0 && minimum == 0 && crossStream == 0 && format == 0) {
            // We only support format 0 horizontal kerningdata
            uint16_t nPairs = read_uint16();
//            cout << "Kerning pairs: " << nPairs << endl;
            is->ignore(6);
            for(uint16_t j = 0; j < nPairs; j++) {
                uint16_t left_index = read_uint16();    
                uint16_t right_index = read_uint16();    
                int16_t value = read_int16();
                float fvalue = float(value) / unitsPerEm;
                uint32_t key = ids2kernkey(left_index, right_index);
                kernings[key] = fvalue;
            }
            return;                
        }
        // Skip forward to next subtable
        if (i != nTables - 1) {         
            is->ignore(kerningSubtable.length - sizeof(KerningSubtable));        
        }
    }
}

void TrueTypeFont::read_glyf_table(uint32_t offset) {
    assert(glyphOffsets != NULL);        
    for(uint16_t i = 0; i < numGlyphs; i++) {
        //getGlyphFromIndex(i);    
    }
}

// TODO: Pick a better cmap to support unicode
void TrueTypeFont::read_cmap_table(uint32_t offset) {
    is->seekg(offset);
    CmapIndex index;
    CmapEncodingSubtable encSubTable;
    CmapSubtableHeader subtableHeader;
    read_struct("ss", (char*)&index, sizeof(CmapIndex));
//    cout << "cmap subtables " << index.numberSubtables << endl;
    
    uint32_t offsets[index.numberSubtables];
    uint16_t platformIDs[index.numberSubtables];
    uint16_t platformSpecificIDs[index.numberSubtables];
    
    for(int i = 0; i < index.numberSubtables; i++) {
        read_struct("ssi", (char*)&encSubTable, sizeof(CmapEncodingSubtable));
        offsets[i] = encSubTable.offset;
        platformIDs[i] = encSubTable.platformID;
        platformSpecificIDs[i] = encSubTable.platformSpecificID;
//        cout << "cmap offset: " << offsets[i] << ", platform: " << encSubTable.platformID << ", specific: " << encSubTable.platformSpecificID << endl;
    }
    
    for(int i = 0; i < index.numberSubtables; i++) {
        is->seekg(offset + offsets[i]);
        read_struct("sss", (char*)&subtableHeader, sizeof(CmapSubtableHeader));
        if (subtableHeader.format == 4 && platformIDs[i] == 3 && platformSpecificIDs[i] == 1) {
             // Found the unicode subtable
             uint16_t segCountX2 = read_uint16();
             is->ignore(3 * sizeof(uint16_t));
             uint16_t segCount = segCountX2 / 2;
             uint32_t remaining = subtableHeader.length - 4*segCount*sizeof(uint16_t) - 8*sizeof(uint16_t);
             remaining /= 2;

             endCode = new uint16_t[segCount];
             startCode = new uint16_t[segCount];
             idDelta = new uint16_t[segCount];
             idRangeOffset = new uint16_t[segCount+remaining];

             for(uint16_t j = 0; j < segCount; j++) {
                 endCode[j] = read_uint16();
             }
             if (endCode[segCount-1] != 0xffff) throw_exception ("Invalid endcode in " + filename);
             uint16_t ignore = read_uint16();
             if (ignore != 0) throw_exception ("Reserved pad not zero in " + filename);
             for(uint16_t j = 0; j < segCount; j++) {
                 startCode[j] = read_uint16();
             }
             for(uint16_t j = 0; j < segCount; j++) {
                 idDelta[j] = read_uint16();
             }
             for(uint16_t j = 0; j < segCount; j++) {
                 idRangeOffset[j] = read_uint16();
             }
                 
             // glyphIndexArray is tagged at the end of idRangeOffset
             for(uint32_t j = 0; j < remaining; j++) {
                 idRangeOffset[j+segCount] = read_uint16();    
             }
             return;
        }
        
        /*
        cout << i << ". Format: " << subtableHeader.format << ", lang: " << subtableHeader.language << ", length: " << subtableHeader.length << endl;
        if (subtableHeader.format == 0) {
            for(int j = 0; j < 256; j++) {
                glyphIndexArray[j] = read_uint8();
            }            
        }
        */
    }
    throw_exception("Font contains no unicode mappings: " + filename);
}

void TrueTypeFont::processSimpleGlyph(TrueTypeFont::Glyph* glyph, int16_t numberOfContours) {
    uint16_t endPtsOfContours[numberOfContours];
    
    for(int16_t i = 0; i < numberOfContours; i++) {
        endPtsOfContours[i] = read_uint16();
    }
    uint16_t numberOfPoints = endPtsOfContours[numberOfContours-1]+1;
    
    // Skip instructions TODO: Use is->ignore(instructionLength)
    uint16_t instructionLength = read_uint16();
    for(int16_t i = 0; i < instructionLength; i++) {
        read_uint8();
    }

    // Read RLE-encoded flags
    uint8_t flags[numberOfPoints];
    for(int16_t i = 0; i < numberOfPoints; i++) {
        uint8_t flag = read_uint8();
        if ((flag & 0xc0) != 0) throw_exception("Invalid glyph-flag in " + filename);
        flags[i] = flag;
        if (flag & 0x08) {
            uint8_t repeat_num = read_uint8();        
            for(uint8_t j = 0; j < repeat_num; j++) {
                i++;
                flags[i] = flag;    
            }
        }
    }
    
    // Read coordinates
    int16_t xCoordinates[numberOfPoints];
    int16_t yCoordinates[numberOfPoints];
    
    int16_t xCoord = 0;
    for(uint16_t i = 0; i < numberOfPoints; i++) {
        uint8_t flag = flags[i];
        uint8_t xShort = (flag >> 1) & 1;
        uint8_t xIsSame = (flag >> 4) & 1;
        if (xShort) {
            xCoord += int16_t(read_uint8()) * (xIsSame ? 1 : -1);
        } else {
            if (xIsSame) {
                //if (i == 0) throw_exception("Invalid repeat of glyph-coordinate in " + filename);    
                //xCoord = xCoordinates[i-1];    
            } else {
                xCoord += read_int16();    
            }        
        }
        xCoordinates[i] = xCoord;
    }
    
    int16_t yCoord = 0;
    for(uint16_t i = 0; i < numberOfPoints; i++) {
        uint8_t flag = flags[i];
        uint8_t yShort = (flag >> 2) & 1;
        uint8_t yIsSame = (flag >> 5) & 1;
        if (yShort) {
            yCoord += int16_t(read_uint8()) * (yIsSame ? 1 : -1);
        } else {
            if (yIsSame) {
                //if (i == 0) throw_exception("Invalid repeat of glyph-coordinate in " + filename);    
                //yCoord = yCoordinates[i-1];    
            } else {
                yCoord += read_int16();    
            }        
        }
        yCoordinates[i] = yCoord;
    }
    
    // Create our own glyph structure from the data gathered above
    uint16_t j = 0;
    for(uint16_t i = 0; i < numberOfContours; i++) {
        Contour contour;
        while(j <= endPtsOfContours[i]) {
            Vector2 coord = Vector2(float(xCoordinates[j]) / unitsPerEm, float(yCoordinates[j]) / unitsPerEm);        
            contour.coords.push_back(coord);
            bool onCurve = (flags[j] & 1) == 1;
            contour.onCurve.push_back(onCurve);
            j++;        
        }
        glyph->contours.push_back(contour);    
    }
}

#define ARG_1_AND_2_ARE_WORDS    0x0001
#define ARGS_ARE_XY_VALUES       0x0002
#define ROUND_XY_TO_GRID         0x0004
#define WE_HAVE_A_SCALE          0x0008
#define MORE_COMPONENTS          0x0020
#define WE_HAVE_AN_X_AND_Y_SCALE 0x0040
#define WE_HAVE_A_TWO_BY_TWO     0x0080
#define WE_HAVE_INSTRUCTIONS     0x0100
#define USE_MY_METRICS           0x0200

// See http://developer.apple.com/textfonts/TTRefMan/RM06/Chap6glyf.html
void TrueTypeFont::processCompoundGlyph(TrueTypeFont::Glyph* glyph) {
    uint16_t flags;        
    float a,b,c,d,e,f;
    do {        
        flags = read_uint16();    
        uint16_t glyphIndex = read_uint16();

        if (flags & ARGS_ARE_XY_VALUES) {
            if (flags & ARG_1_AND_2_ARE_WORDS) {
                e = read_int16();
                f = read_int16();        
            } else {
                e = read_int8();        
                f = read_int8();        
            }
            e /= unitsPerEm;
            f /= unitsPerEm;
        } else {
           e = f = 0;        
           cout << "Unsupported composite stuff";        
        }
        
        if (flags & WE_HAVE_A_SCALE) {
            a = d = read_uint16() / (1 << 14);
            b = c = 0.0;
        } else if (flags & WE_HAVE_AN_X_AND_Y_SCALE) {
            a = read_uint16() / (1 << 14);
            d = read_uint16() / (1 << 14);
            cout << "a " << a << " d " << d << endl;
            b = c = 0.0;
        } else if (flags & WE_HAVE_A_TWO_BY_TWO) {
            a = read_uint16() / (1 << 14);
            b = read_uint16() / (1 << 14);
            c = read_uint16() / (1 << 14);
            d = read_uint16() / (1 << 14);
        } else {
            a = d = 1.0;        
            b = c = 0.0;
        }
        
        int saved_pos = is->tellg();
        TrueTypeFont::Glyph* subglyph = createGlyph(glyphIndex);
        subglyph->transform(a,b,c,d,e,f);
        glyph->contours.insert(glyph->contours.end(), subglyph->contours.begin(), subglyph->contours.end());
        if (flags & USE_MY_METRICS) {
            glyph->xMin = subglyph->xMin;
            glyph->xMax = subglyph->xMax;
            glyph->yMin = subglyph->yMin;
            glyph->yMax = subglyph->yMax;
            glyph->advanceWidth = subglyph->advanceWidth;
            glyph->leftSideBearing = subglyph->leftSideBearing;
        }
        is->seekg(saved_pos);
        
    } while (flags & MORE_COMPONENTS);
}


void TrueTypeFont::processGlyph(TrueTypeFont::Glyph* glyph, uint32_t glyphIndex) {
//    cout << "Processing glyph " << dec << glyphIndex << endl;        
    is->seekg(glyf_table_offset + glyphOffsets[glyphIndex]);
    GlyphDescription glyphDescr;
    read_struct("SSSSS", (char*)&glyphDescr, sizeof(GlyphDescription));
    glyph->xMin = float(glyphDescr.xMin) / unitsPerEm;
    glyph->xMax = float(glyphDescr.xMax) / unitsPerEm;
    glyph->yMin = float(glyphDescr.yMin) / unitsPerEm;
    glyph->yMax = float(glyphDescr.yMax) / unitsPerEm;
    glyph->advanceWidth = float(advanceWidths[glyphIndex]) / unitsPerEm;
    glyph->leftSideBearing = float(leftSideBearings[glyphIndex]) / unitsPerEm;
    if (glyphDescr.numberOfContours >= 0) {
        processSimpleGlyph(glyph, glyphDescr.numberOfContours);
    } else if (glyphDescr.numberOfContours == -1) {
        processCompoundGlyph(glyph);
    } else {
        cout << "Contours: " << dec << glyphDescr.numberOfContours << endl;    
        throw_exception("Illegal number of contours in " + filename);    
    }
}

TrueTypeFont::Glyph* TrueTypeFont::createGlyph(uint32_t glyphIndex) {
    TrueTypeFont::Glyph* glyph = new TrueTypeFont::Glyph();
    processGlyph(glyph, glyphIndex);
    return glyph;
}

TrueTypeFont::Glyph* TrueTypeFont::getGlyphFromIndex(uint32_t glyphIndex) {
    if (glyphs[glyphIndex] == NULL) {
        glyphs[glyphIndex] = createGlyph(glyphIndex);
    }
    return glyphs[glyphIndex];       
}

uint16_t TrueTypeFont::char2glyphIndex(wchar_t c) {
    uint16_t segment = 0;

    while (c > endCode[segment]) segment++;
    
    if (startCode[segment] > c) {
        return 0;
    }
    
    uint16_t rangeOffset = idRangeOffset[segment];
    if (rangeOffset == 0) {
        return c + idDelta[segment];    
    } else {
        return *( &idRangeOffset[segment] + idRangeOffset[segment] / 2 + (c - startCode[segment]) );    
    }
}

vector<TrueTypeFont::Glyph*> TrueTypeFont::getGlyphs(wstring str) {
    vector<Glyph*> result;
    result.reserve(str.size());
    for(uint32_t i = 0; i < str.size(); i++) {
        wchar_t c = str[i];
        Glyph* glyph = getGlyph(c);
        result.push_back(glyph);
    }
    return result;
}

TrueTypeFont::Glyph* TrueTypeFont::getGlyph(wchar_t c) {
//    cout << "Getting glyph for unicode " << setw(5) << setfill('0') << hex << c << endl;        
    return getGlyphFromIndex(char2glyphIndex(c));
}

float TrueTypeFont::getKerning(wchar_t left, wchar_t right) {
    uint16_t left_index = char2glyphIndex(left);    
    uint16_t right_index = char2glyphIndex(right);
    uint32_t key = ids2kernkey(left_index, right_index);
    map<uint32_t,float>::iterator iterator = kernings.find(key);
    if (iterator == kernings.end()) {
        return 0.0;    
    } else {
        return iterator->second;    
    }
}

bool TrueTypeFont::isWhitespace(wchar_t c) {
    return iswspace(c);        
}


// See http://developer.apple.com/textfonts/TTRefMan/RM06/Chap6glyf.html
void TrueTypeFont::Glyph::transform(float a, float b, float c, float d, float e, float f) {
    float m = std::max(fabs(a),fabs(b));
    float n = std::max(fabs(c),fabs(d));
    if (fabs(fabs(a)-fabs(c)) <= 33/65536) m *= 2;
    if (fabs(fabs(c)-fabs(d)) <= 33/65536) n *= 2;
        
    for(uint32_t i = 0; i < contours.size(); i++) {
        Contour& contour = contours[i];  
        for(uint32_t j = 0; j < contour.coords.size(); j++) {
            Vector2& v = contour.coords[j];        
            float x = m * ((a/m)*v[0] + (c/m)*v[1] + e);
            float y = n * ((b/n)*v[0] + (d/n)*v[1] + f);
            v[0] = x;
            v[1] = y;
        }
    }   
}

///////////////////////////////////////////////////////////
// Helpers
///////////////////////////////////////////////////////////

uint32_t TrueTypeFont::read_uint32() {
    union { 
        uint32_t result;
        uint8_t bytes[4];
        uint16_t shorts[2];
    };    
    is->read((char*)&result, 4);
    if (is->bad()) {
        throw_exception("Error reading from " + filename);    
    }
#ifdef WORDS_BIGENDIAN
    return result;
#else
    swap(bytes[0],bytes[3]);
    swap(bytes[1],bytes[2]);
    return result;
#endif                
}

uint16_t TrueTypeFont::read_uint16() {
    union { 
        uint16_t result;
        uint8_t bytes[2];
    };    
    is->read((char*)&result, 2);
#ifdef WORDS_BIGENDIAN
    return result;
#else
    swap(bytes[0],bytes[1]);
    return result;
#endif                
}

int16_t TrueTypeFont::read_int16() {
    union { 
        int16_t result;
        uint8_t bytes[2];
    };    
    is->read((char*)&result, 2);
#ifdef WORDS_BIGENDIAN
    return result;
#else
    swap(bytes[0],bytes[1]);
    return result;
#endif                
}

uint8_t TrueTypeFont::read_uint8() {
    uint8_t result;
    is->read((char*)&result, 1);
    return result;            
}

int8_t TrueTypeFont::read_int8() {
    int8_t result;
    is->read((char*)&result, 1);
    return result;            
}


void TrueTypeFont::read_struct(char* types, char* addr, uint32_t bytes) {
    int i = 0;
    while(bytes > 0) {
        char t = types[i++];            
        switch(t) {
            case 'i' :        
            case 'w' : *((uint32_t*)addr) = read_uint32();
                       addr += 4; bytes -= 4;
                       break;
            case 's' : *((uint16_t*)addr) = read_uint16();
                       addr += 2; bytes -= 2;
                       break;
            case 'S' : *((int16_t*)addr) = read_int16();
                       addr += 2; bytes -= 2;
                       break;
            case 'b' : *((uint8_t*)addr) = read_uint8();
                       addr += 1; bytes -= 1;
                       break;
        }
    }
}

