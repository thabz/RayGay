
#ifndef RAYGAY_TRUETYPEFONT_H
#define RAYGAY_TRUETYPEFONT_H

#include <string>

using namespace std;

class TrueTypeFont {
    public:
        TrueTypeFont(string filename);
        
    private:
        uint32_t read_uint32(istream& is);           
        uint16_t read_uint16(istream& is);
        uint8_t read_uint8(istream& is);
        void read_struct(istream& is, char* types, char* addr, uint32_t bytes);
};

#endif