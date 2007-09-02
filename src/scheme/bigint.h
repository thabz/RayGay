
#ifndef SCHEME_BIGINT
#define SCHEME_BIGINT

#include <vector>
#include <string>

// All the code asserts the long is 64 bits and int 32 bits
// http://cs.marlboro.edu/term/cs-fall02/algorithms/crypto/RSA/bigint/
class BigInt {
    public:        
        BigInt(int n);
        BigInt(const std::string& str);
        BigInt(const BigInt& o);
        
        bool fitsInLong();
        long asLong();
        BigInt operator+(const BigInt &b) const;
        BigInt operator+(int n) const;
        BigInt& operator+=(int n);
        BigInt operator*(const BigInt &b) const;
        BigInt operator*(int n) const;
        BigInt& operator*=(int n);
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
    
    private:
        void normalize();
        static int compare(const BigInt& b1, const BigInt& b2); 
        void resize(std::vector<long>::size_type new_digits_num);

    private:        
        int sign;     
        std::vector<long> digits;    
};

#endif
