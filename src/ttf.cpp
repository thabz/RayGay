
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
    this->filename = filename;
        
    cout << "Loading font: " << filename << endl;
    
    this->is = new ifstream(filename.c_str(), ios::in|ios::binary);
    if (is->bad()) {
        throw_exception("Can't open " + filename);    
    }
    
    OffsetSubtable offsetSubtable;
    read_struct("issss", (char*)&offsetSubtable, sizeof(OffsetSubtable));
    cout << "Scalertype: " << hex << offsetSubtable.scalerType << endl;
    cout << "Num tables " << hex << offsetSubtable.numTables << endl;
    
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
        cout << "Tag: " << tag << ", length " << dec << entry.length << ", offset 0x" << hex << entry.offset << endl;
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
    
    read_glyf_table(glyf_table_offset);
    read_cmap_table(cmap_table_offset);
    read_hhea_table(hhea_table_offset);    // Find numOfLongHorMetrics
    read_hmtx_table(hmtx_table_offset);
    
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
    cout << "numGlyphs: " << dec << numGlyphs << endl;
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
    int16_t leftSideBearing;
    for(uint16_t i = 0; i < numOfLongHorMetrics; i++) {
        advanceWidth = read_uint16();
        leftSideBearing = read_int16();
        glyphs[i]->advanceWidth = float(advanceWidth) / unitsPerEm;
        glyphs[i]->leftSideBearing = float(leftSideBearing) / unitsPerEm;
    }
    for(uint16_t i = numOfLongHorMetrics; i < numGlyphs; i++) {
        leftSideBearing = read_int16();
        glyphs[i]->advanceWidth = float(advanceWidth) / unitsPerEm;
        glyphs[i]->leftSideBearing = float(leftSideBearing) / unitsPerEm;
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
            cout << "Kerning pairs: " << nPairs << endl;
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
        getGlyphFromIndex(i);    
    }
}

// TODO: Pick a better cmap to support unicode
void TrueTypeFont::read_cmap_table(uint32_t offset) {
    is->seekg(offset);
    CmapIndex index;
    CmapEncodingSubtable encSubTable;
    CmapSubtableHeader subtableHeader;
    read_struct("ss", (char*)&index, sizeof(CmapIndex));
    cout << "cmap subtables " << index.numberSubtables << endl;
    uint32_t offsets[index.numberSubtables];
    for(int i = 0; i < index.numberSubtables; i++) {
        read_struct("ssi", (char*)&encSubTable, sizeof(CmapEncodingSubtable));
        offsets[i] = encSubTable.offset;
        cout << "cmap offset: " << offsets[i] << ", platform: " << encSubTable.platformID << ", specific: " << encSubTable.platformSpecificID << endl;
    }
    for(int i = 0; i < index.numberSubtables; i++) {
        is->seekg(offset + offsets[i]);
        read_struct("sss", (char*)&subtableHeader, sizeof(CmapSubtableHeader));
        cout << i << ". Format: " << subtableHeader.format << ", lang: " << subtableHeader.language << ", length: " << subtableHeader.length << endl;
        if (subtableHeader.format == 0) {
            for(int j = 0; j < 256; j++) {
                glyphIndexArray[j] = read_uint8();
            }            
        }
    }
}

void TrueTypeFont::processSimpleGlyph(TrueTypeFont::Glyph* glyph, int16_t numberOfContours) {
    uint16_t endPtsOfContours[numberOfContours];
    
    for(int16_t i = 0; i < numberOfContours; i++) {
        endPtsOfContours[i] = read_uint16();
    }
    uint16_t numberOfPoints = endPtsOfContours[numberOfContours-1]+1;
    
    // Skip instructions 
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
        TrueTypeFont::Contour contour;
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

void TrueTypeFont::processCompoundGlyph(TrueTypeFont::Glyph* glyph) {
        
}


void TrueTypeFont::processGlyph(TrueTypeFont::Glyph* glyph, uint32_t glyphIndex) {
    is->seekg(glyf_table_offset + glyphOffsets[glyphIndex]);
    GlyphDescription glyphDescr;
    read_struct("SSSSS", (char*)&glyphDescr, sizeof(GlyphDescription));
    glyph->xMin = glyphDescr.xMin;
    glyph->xMax = glyphDescr.xMax;
    glyph->yMin = glyphDescr.yMin;
    glyph->yMax = glyphDescr.yMax;
    if (glyphDescr.numberOfContours >= 0) {
        processSimpleGlyph(glyph, glyphDescr.numberOfContours);
    } else if (glyphDescr.numberOfContours == -1) {
        processCompoundGlyph(glyph);
    } else {
        cout << "Contours: " << dec << glyphDescr.numberOfContours << endl;    
        throw_exception("Illegal number of contours in " + filename);    
    }
}


TrueTypeFont::Glyph* TrueTypeFont::getGlyphFromIndex(uint32_t glyphIndex) {
    if (glyphs[glyphIndex] == NULL) {
        TrueTypeFont::Glyph* glyph = new TrueTypeFont::Glyph();
        glyphs[glyphIndex] = glyph;
        processGlyph(glyph, glyphIndex);
    }
    return glyphs[glyphIndex];       
}

vector<TrueTypeFont::Glyph*> TrueTypeFont::getGlyphs(string str) {
    vector<Glyph*> result;
    result.reserve(str.size());
    for(uint32_t i = 0; i < str.size(); i++) {
        Glyph* glyph = getGlyph(str[i]);
        result.push_back(glyph);
    }
    return result;
}

TrueTypeFont::Glyph* TrueTypeFont::getGlyph(char c) {
    return getGlyphFromIndex(glyphIndexArray[uint32_t(c)]);    
}

float TrueTypeFont::getKerning(char left, char right) {
    uint16_t left_index = glyphIndexArray[uint32_t(left)];    
    uint16_t right_index = glyphIndexArray[uint32_t(right)];
    uint32_t key = ids2kernkey(left_index, right_index);
    map<uint32_t,float>::iterator iterator = kernings.find(key);
    if (iterator == kernings.end()) {
        return 0.0;    
    } else {
        return iterator->second;    
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

