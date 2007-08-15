
#include "ttf.h"
#include "exception.h"
#include <iostream>
#include <fstream>
#include <iomanip>

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

TrueTypeFont::TrueTypeFont(string filename) {
    cout << "Loading font: " << filename << endl;
    
    ifstream ifs(filename.c_str(), ios::in|ios::binary);
    if (ifs.bad()) {
        throw_exception("Can't open " + filename);    
    }
    
    OffsetSubtable offsetSubtable;
    read_struct(ifs, "issss", (char*)&offsetSubtable, sizeof(OffsetSubtable));
    cout << "Scalertype: " << hex << offsetSubtable.scalerType << endl;
    cout << "Num tables " << hex << offsetSubtable.numTables << endl;

    TableDirectoryEntry entry;
    for(uint32_t i = 0; i < offsetSubtable.numTables; i++) {
        read_struct(ifs, "bbbbiii", (char*)&entry, sizeof(TableDirectoryEntry));
        cout << "Tag: " << string((char*)entry.tag,4) << endl;
    }
};

uint32_t TrueTypeFont::read_uint32(istream& is) {
    union { 
        uint32_t result;
        uint8_t bytes[4];
        uint16_t shorts[2];
    };    
    is.read((char*)&result, 4);
#ifdef WORDS_BIGENDIAN
    return result;
#else
    swap(bytes[0],bytes[3]);
    swap(bytes[1],bytes[2]);
    return result;
#endif                
}

uint16_t TrueTypeFont::read_uint16(istream& is) {
    union { 
        uint16_t result;
        uint8_t bytes[2];
    };    
    is.read((char*)&result, 2);
#ifdef WORDS_BIGENDIAN
    return result;
#else
    swap(bytes[0],bytes[1]);
    return result;
#endif                
}

uint8_t TrueTypeFont::read_uint8(istream& is) {
    uint8_t result;
    is.read((char*)&result, 1);
    return result;            
}


void TrueTypeFont::read_struct(istream& is, char* types, char* addr, uint32_t bytes) {
    int i = 0;
    while(bytes > 0) {
        char t = types[i++];            
        switch(t) {
            case 'i' :        
            case 'w' : *((uint32_t*)addr) = read_uint32(is);
                       addr += 4; bytes -= 4;
                       break;
            case 's' : *((uint16_t*)addr) = read_uint16(is);
                       addr += 2; bytes -= 2;
                       break;
            case 'b' : *((uint8_t*)addr) = read_uint8(is);
                       addr += 1; bytes -= 1;
                       break;
        }
    };
}

