
#ifndef SCHEME_BIGINT
#define SCHEME_BIGINT

#include <vector>
#include <string>

// The code asserts int is 32 bits
// http://cs.marlboro.edu/term/cs-fall02/algorithms/crypto/RSA/bigint/
class BigInt 
{
    public:        
        BigInt(int32_t n);
        BigInt(const char* str, uint radix = 10);
        BigInt(const BigInt& o);
        
<<<<<<< .mine
        bool fitsInInt64();
        int64_t asInt64();
=======
        bool fitsLong();
        long asLong();
>>>>>>> .r7993
        bool fitsInt();
        int  asInt();
        BigInt operator+(const BigInt &b) const;
        BigInt operator+(int32_t n) const;
        BigInt& operator+=(int32_t n);
        BigInt operator*(const BigInt &b) const;
        BigInt operator*(int32_t n) const;
        BigInt& operator*=(int32_t n);
        bool operator==(const BigInt& o) const;
        bool operator<(const BigInt& o) const;
        bool operator>(const BigInt& o) const;
        bool operator<=(const BigInt& o) const;
        bool operator>=(const BigInt& o) const;
        
        bool is_zero() const;
        void dump();
        
    public:
        static const BigInt& ZERO;
        static const BigInt& ONE;
        static const BigInt& TWO;
        
    private:
        static BigInt _ZERO;
        static BigInt _ONE;
        static BigInt _TWO;
        static int64_t RADIX;
    
    private:
        void normalize();
        static int compare(const BigInt& b1, const BigInt& b2); 
        void resize(int32_t new_digits_num);

    private:        
        std::vector<int64_t> digits;    
        int sign;     
};

#endif
