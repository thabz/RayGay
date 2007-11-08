
#ifndef SCHEME_BIGINT
#define SCHEME_BIGINT

#include <vector>
#include <string>
#include <ostream>

// The code asserts int is 32 bits
// http://cs.marlboro.edu/term/cs-fall02/algorithms/crypto/RSA/bigint/
class bigint 
{

    friend std::ostream & operator<< (std::ostream &os, const bigint &b);

    public:        
        bigint(int32_t n);
        bigint(std::string str, uint radix = 10);
        bigint(const bigint& o);
        ~bigint();
        
        bool fitsInt64() const;
        int64_t toInt64() const;
        bool fitsInt32() const;
        int32_t toInt32() const;
        std::string toString(uint radix = 10) const;

        bigint& operator+=(const bigint &b);
        bigint& operator+=(int32_t n);
        bigint& operator-=(const bigint &b);
        bigint& operator-=(int32_t n);
        bigint& operator*=(int32_t n);

        bigint operator+(const bigint &b) const;
        bigint operator+(int32_t n) const;
        bigint operator-(const bigint &b) const;
        bigint operator-(int32_t n) const;
        bigint operator-() const;
        bigint operator*(const bigint &b) const;
        bigint operator*(int32_t n) const;
        bigint operator/(int32_t n) const;
        bigint operator/(const bigint &b) const;
        int32_t operator%(int32_t n) const;
        bool operator==(const bigint& o) const;
        bool operator!=(const bigint& o) const;
        bool operator<(const bigint& o) const;
        bool operator>(const bigint& o) const;
        bool operator<=(const bigint& o) const;
        bool operator>=(const bigint& o) const;
        
        bigint abs() const;
        bigint sqrt() const;
        bigint expt(int power) const;
        bigint times_two() const;
        bigint square() const;
        
        bool is_zero() const;
        bool is_one() const;
	    int sizeInBits() const;
        void dump() const;
        
    public:
        static const bigint& ZERO;
        static const bigint& ONE;
        static const bigint& TWO;
        
    private:
        static bigint _ZERO;
        static bigint _ONE;
        static bigint _TWO;
        static int64_t RADIX;
    
    private:
        void normalize();
        static int compare(const bigint& b1, const bigint& b2); 
        void resize(int32_t new_digits_num);
        uint32_t size() const;

    private:        
        std::vector<int64_t> digits;    
        int sign;     
};

#endif

inline
uint32_t bigint::size() const {
    return digits.size();        
}
