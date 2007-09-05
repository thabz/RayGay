
#include "bigint.h"
#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace std;

#define MININT (1 << (32 - 1))
#define MAXINT (~MININT) 
#define INT_BITS 31

int64_t BigInt::RADIX = (int64_t(1) << 31);

BigInt BigInt::_ZERO = BigInt(0);
BigInt BigInt::_ONE = BigInt(1);
BigInt BigInt::_TWO = BigInt(2);

const BigInt& BigInt::ONE = _ONE;
const BigInt& BigInt::TWO = _TWO;
const BigInt& BigInt::ZERO = _ZERO;

BigInt::BigInt(int32_t n) {
    sign = 1;
    digits.push_back(n);
    normalize();
}

BigInt::BigInt(string str, uint radix) 
{
    int fsign = 1;        

    sign = 1;        
    digits.push_back(0);
    normalize();

    uint i = 0;
         
    if (str[0] == '-') {
        fsign = -1;
        i++;    
    }       
    
    while (str[i] != '\0') {
        char c = str[i];
        uint32_t digit;
        if (c >= '0' && c <= '9') {
            digit = c - '0';        
        } else if (c >= 'a' && c <= 'z') {
            digit = 10 + (c - 'a');        
        } else if (c >= 'A' && c <= 'Z') {
            digit = 10 + (c - 'A');        
        } else {
            throw invalid_argument("Not a valid digit: " + c);        
        }
        
        if (digit >= radix) {
            throw invalid_argument("Not a valid digit: " + c);        
        }

        *(this) *= radix;
        *(this) += digit;
        i++;
    }
    normalize();
    sign = is_zero() ? 1 : fsign;
}

BigInt::BigInt(const BigInt& o) {
    digits = o.digits;        
    sign = o.sign;
}

string BigInt::toString(uint radix) const {
    char chars[] = "0123456789abcdefghijklmnopqrstuvwxyz";        
    if (radix > 26+10 || radix == 0) throw invalid_argument("Invalid radix");
    return "NOT IMPLEMENTED YET";
}


bool BigInt::is_zero() const {
    return digits.size() == 1 && digits[0] == 0;     
}

void BigInt::dump() const {
    cout << "Sign: " << sign << endl;        
    cout << "Digits: " << digits.size() << endl;
    for(uint i = 0; i < digits.size(); i++) {
        cout << "   digit[" << i << "]: " << digits[i] << endl;    
    }
}

bool BigInt::operator==(const BigInt& o) const {
    if (digits.size() != o.digits.size() || sign != o.sign) {
        return false;            
    }        
    for(uint i = 0; i < digits.size(); i++) {
        if (digits[i] != o.digits[i]) return false;               
    }
    return true;
}

BigInt BigInt::operator+(const BigInt &v) const {
    BigInt r = *this;
    r.resize(max(r.digits.size(), v.digits.size()));
    for(uint i = 0; i < r.digits.size(); i++) {
        if (r.sign == v.sign) {
            r.digits[i] += v.digits[i];        
        } else {
            r.digits[i] -= v.digits[i];        
        }
    }
    r.normalize();
    return r;
}

BigInt BigInt::operator+(int32_t n) const {
    BigInt r = *this;    
    if (r.sign == 1) {
        r.digits[0] += n;    
    } else {
        r.digits[0] -= n;    
    }
    r.normalize();
    return r;
}

BigInt& BigInt::operator+=(int32_t n) {
    if (sign == 1) {
        digits[0] += n;    
    } else {
        digits[0] -= n;    
    }
    normalize();
    return *this;
}

BigInt BigInt::operator-(const BigInt &v) const {
    BigInt r = *this;
    r.resize(max(r.digits.size(), v.digits.size()));
    for(uint i = 0; i < r.digits.size(); i++) {
        if (r.sign == v.sign) {
            r.digits[i] -= v.digits[i];        
        } else {
            r.digits[i] += v.digits[i];        
        }
    }
    r.normalize();
    return r;
        
}

BigInt BigInt::operator-(int32_t n) const {
    BigInt r = *this;    
    if (r.sign == 1) {
        r.digits[0] -= n;    
    } else {
        r.digits[0] += n;    
    }
    r.normalize();
    return r;        
}

BigInt BigInt::operator-() const {
    BigInt r = *this;    
    r.sign = -r.sign;
    return r;     
}

BigInt& BigInt::operator-=(int32_t n) {
    if (sign == 1) {
        digits[0] -= n;    
    } else {
        digits[0] += n;    
    }
    normalize();
    return *this;
}


BigInt BigInt::operator*(const BigInt &o) const 
{
    BigInt r = ZERO;

    int size = o.digits.size() + digits.size();
    r.resize(size);
    for(uint i = 0; i < digits.size(); i++) {
        for(uint j = 0; j < o.digits.size(); j++) {
            r.digits[i+j] += digits[i] * o.digits[j];            
        }    
        r.normalize();
        r.resize(size);
    }
    r.normalize();
    r.sign = r.is_zero() ? 1 : sign * o.sign;
    return r;
}

BigInt BigInt::operator*(int32_t n) const 
{
    BigInt r = *this;    

    if (n < 0) {
        n = -n;    
        r.sign = -r.sign;
    }
    for(uint i = 0; i < r.digits.size(); i++) {
        r.digits[i] *= n;            
    }
    r.normalize();
    return r;
}

BigInt& BigInt::operator*=(int32_t n) {
    if (n < 0) {
        n = -n;    
        sign = -sign;
    }
    for(uint i = 0; i < digits.size(); i++) {
        digits[i] *= n;            
    }
    normalize();
    return *this;
}

BigInt BigInt::operator/(int32_t n) const {
    BigInt s = *this;
    if (n == 0) throw range_error("Division by zero");
    if (n < 0) {
        n = -n;    
        s.sign = -s.sign;
    }
    int64_t r = 0;
    for(uint i = s.digits.size()-1; i >= 0; i--) {
        r = r * RADIX + s.digits[i];    
        s.digits[i] = r / n;
        r %= n;
    }
    s.normalize();
    return s;
}

BigInt BigInt::abs() const {
    BigInt r = *this;    
    r.sign = 1;
    return r;    
}

int BigInt::compare(const BigInt& b1, const BigInt& b2) {
    if (b1.sign > b2.sign) {
        return 1;       
    } else if (b1.sign < b2.sign) {
        return -1;    
    } else if (b1.digits.size() > b2.digits.size()) {
        return 1;    
    } else if (b1.digits.size() < b2.digits.size()) {
        return -1; 
    } else {
        // Same number of digits and same sign. Compare digits.
        for(uint i = b1.digits.size()-1; i >= 0; i--) {
            if (b1.digits[i] > b2.digits[i]) {
                return 1;    
            } else if (b1.digits[i] < b2.digits[i]) {
                return -1;    
            } 
        }    
        return 0;
    }    
}

bool BigInt::operator<(const BigInt& o) const {
    int c = BigInt::compare(*this, o);        
    return c == -1;
}

bool BigInt::operator>(const BigInt& o) const {
    int c = BigInt::compare(*this, o);        
    return c == 1;
}

bool BigInt::operator<=(const BigInt& o) const {
    int c = BigInt::compare(*this, o);
    return c == -1 || c == 0;
}

bool BigInt::operator>=(const BigInt& o) const {
    int c = BigInt::compare(*this, o);        
    return c == 1 || c == 0;
}


// Normalize so that 
// 1) All digits are 0 <= d < RADIX
// 2) Sign 1 or -1
// 3) Leading zero digits are removed
// 4) Fix sign for zero
void BigInt::normalize() 
{
    for(uint i = 0; i < digits.size()-1; i++) {
        if (digits[i] < 0) {
            digits[i+1] += digits[i] / RADIX - 1;    
            digits[i] %= RADIX; 
            if (digits[i] != 0) {
                digits[i] += RADIX;    
            } else {
                digits[i+1] += 1;    
            }
        }    
    }
    
    if (digits[digits.size()-1] < 0) {
        sign = -sign;     
        for(uint i = 0; i < digits.size() - 1; i++) {
            digits[i] = RADIX - digits[i];
            digits[i+1] += 1;            
        }
        digits[digits.size()-1] = -digits[digits.size()-1];
    }

    uint64_t dsize = digits.size();
    for(uint i = 0; i < dsize; i++) {
        if (digits[i] >= RADIX) {
            if (i+1 >= dsize) {
                resize(digits.size()+1);
            }        
            digits[i+1] += digits[i] / RADIX;        
            digits[i] %= RADIX;
        }    
    }
    
    uint i = digits.size();
    for(; i > 1 && digits[i-1] == 0; i--);
    resize(i);
    
    // Fix sign for zero
    if (digits.size() == 1 && digits[0] == 0 && sign == -1) {
        sign = 1;   
    }
}

// To get a specific number of digits we pad with zeroes on the left
void BigInt::resize(int32_t new_digits_num) {
    assert(new_digits_num > 0);
    digits.resize(new_digits_num, 0);        
}

int BigInt::sizeInBits() const {
    int c = (digits.size()-1)*INT_BITS;
    int64_t n = digits[digits.size()-1];
    while(n != 0) {
	n >>= 1;
	c++;
    }	
    return c;
}

