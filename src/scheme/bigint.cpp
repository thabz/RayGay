
#include "bigint.h"
#include <algorithm>
#include <iostream>
#include <stdexcept>
#include <cassert>

using namespace std;

#define MININT (1 << (32 - 1))
#define MAXINT (~MININT) 
#define INT_BITS 31

int64_t bigint::RADIX = (int64_t(1) << 31);

bigint bigint::_ZERO = bigint(0);
bigint bigint::_ONE = bigint(1);
bigint bigint::_TWO = bigint(2);

const bigint& bigint::ONE = _ONE;
const bigint& bigint::TWO = _TWO;
const bigint& bigint::ZERO = _ZERO;

bigint::~bigint() {};

bigint::bigint(int32_t n) {
    sign = 1;
    digits.push_back(n);
    normalize();
}

bigint::bigint(const string& str, uint32_t radix) 
{
    if (radix >= 37 || radix == 0) throw invalid_argument("Invalid radix");

    int fsign = 1;        

    sign = 1;        
    digits.push_back(0);
    normalize();

    uint32_t i = 0;
         
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

bigint::bigint(const bigint& o) {
    digits = o.digits;
    sign = o.sign;
}

// TODO: This is very slow
string bigint::toString(uint32_t radix) const {
    char chars[37] = "0123456789abcdefghijklmnopqrstuvwxyz";        
    if (radix >= 37 || radix == 0) throw invalid_argument("Invalid radix");
    bigint b = abs(*this);
    string s = "";
    while (!b.is_zero()) {
        int digit = b % radix;
        s += chars[digit];
        b = b / radix;         
    }
    if (sign == -1) {
        s += "-";
    }
    std::reverse(s.begin(), s.end());
    return s;
}

bool bigint::is_zero() const {
    return digits.size() == 1 && digits[0] == 0;     
}

bool bigint::is_one() const {
    return digits.size() == 1 && digits[0] == 1 && sign == 1;     
}


void bigint::dump() const {
    cout << "Sign: " << sign << endl;        
    cout << "Digits: " << digits.size() << endl;
    for(uint32_t i = 0; i < digits.size(); i++) {
        cout << "   digit[" << i << "]: " << digits[i] << endl;    
    }
}

bool bigint::operator==(const bigint& o) const {
    if (digits.size() != o.digits.size() || sign != o.sign) {
        return false;            
    }        
    for(uint32_t i = 0; i < digits.size(); i++) {
        if (digits[i] != o.digits[i]) return false;               
    }
    return true;
}

bool bigint::operator!=(const bigint& o) const {
    return !(*this == o);        
}

bigint bigint::operator+(const bigint &v) const {
    bigint r = *this;
    r += v;
    return r;
}

bigint bigint::operator+(int32_t n) const {
    bigint r = *this;    
    if (r.sign == 1) {
        r.digits[0] += n;    
    } else {
        r.digits[0] -= n;    
    }
    r.normalize();
    return r;
}

bigint& bigint::operator+=(int32_t n) {
    if (sign == 1) {
        digits[0] += n;    
    } else {
        digits[0] -= n;    
    }
    normalize();
    return *this;
}

bigint& bigint::operator+=(const bigint &v) {
    resize(max(digits.size(), v.digits.size()));
    for(uint32_t i = 0; i < digits.size(); i++) {
        if (sign == v.sign) {
            digits[i] += v.digits[i];        
        } else {
            digits[i] -= v.digits[i];        
        }
    }
    normalize();
    return *this;
}


bigint bigint::operator-(const bigint &v) const {
    bigint r = *this;
    r.resize(max(r.digits.size(), v.digits.size()));
    for(uint32_t i = 0; i < r.digits.size(); i++) {
        if (r.sign == v.sign) {
            r.digits[i] -= v.digits[i];        
        } else {
            r.digits[i] += v.digits[i];        
        }
    }
    r.normalize();
    return r;
}

bigint bigint::operator-(int32_t n) const {
    bigint r = *this;    
    if (r.sign == 1) {
        r.digits[0] -= n;    
    } else {
        r.digits[0] += n;    
    }
    r.normalize();
    return r;        
}

bigint bigint::operator-() const {
    bigint r = *this;    
    r.sign = -r.sign;
    return r;     
}

bigint& bigint::operator-=(int32_t n) {
    if (sign == 1) {
        digits[0] -= n;    
    } else {
        digits[0] += n;    
    }
    normalize();
    return *this;
}

bigint& bigint::operator-=(const bigint &v) {
    resize(max(digits.size(), v.digits.size()));
    for(uint32_t i = 0; i < digits.size(); i++) {
        if (sign == v.sign) {
            digits[i] -= v.digits[i];        
        } else {
            digits[i] += v.digits[i];        
        }
    }
    normalize();
    return *this;
}

bigint bigint::operator*(const bigint &o) const 
{
    bigint r = ZERO;

    int size = o.digits.size() + digits.size();
    r.resize(size);
    for(uint32_t i = 0; i < digits.size(); i++) {
        for(uint32_t j = 0; j < o.digits.size(); j++) {
            r.digits[i+j] += digits[i] * o.digits[j];            
        }    
        r.normalize();
        r.resize(size);
    }
    r.normalize();
    r.sign = r.is_zero() ? 1 : sign * o.sign;
    return r;
}

bigint bigint::operator*(int32_t n) const 
{
    bigint r = *this;    

    if (n < 0) {
        n = -n;    
        r.sign = -r.sign;
    }
    for(uint32_t i = 0; i < r.digits.size(); i++) {
        r.digits[i] *= n;            
    }
    r.normalize();
    return r;
}

bigint& bigint::operator*=(int32_t n) {
    if (n < 0) {
        n = -n;    
        sign = -sign;
    }
    for(uint32_t i = 0; i < digits.size(); i++) {
        digits[i] *= n;            
    }
    normalize();
    return *this;
}

bigint bigint::operator/(int32_t n) const 
{
    bigint s = *this;
    if (n == 0) throw range_error("Division by zero");
    if (n < 0) {
        n = -n;    
        s.sign = -s.sign;
    }
    int64_t r = 0;
    for(int i = s.digits.size()-1; i >= 0; i--) {
        r = r * RADIX + s.digits[i];    
        s.digits[i] = r / n;
        r %= n;
    }
    s.normalize();
    return s;
}

// Donald Knuth, The Art of Computer Programming, Volume 2, 2nd ed., 1981, pp. 257-258.
// Knuths algo in C: http://www.ddj.com/showArticle.jhtml?documentID=cuj9611breitz&pgno=8
bigint bigint::operator/(const bigint &denom) const {
     bigint q = 1;
     q.sign = this->sign * denom.sign;

     if (abs(*this) < abs(denom)) return bigint(0);

     if (denom.exp() == 0) { 
	 // denom is a long, so send it
         // to the easy routine
         return *this / (denom.sign*denom.digits[0]);
     }else{
	 q.resize(this->exp() - denom.exp() + 1);
         int64_t qhat;  
	 int qpos = q.exp();
         int64_t d = RADIX / (denom.digits[denom.exp()] + 1); //d is a normalizer

         bigint u(*this); //make a copy of the numerator
         bigint v(denom); //make a copy of the denominator
         int64_t start = u.exp() + 1;
         u = u*d;  v = v*d;
         if (u.exp() < start) { 
	     u.resize(start+1);  
	     u.digits[start] = 0;
	 }

         for(;;) {
          if (abs(u) < abs(v)) { // when u < v we can't divide anymore
              u = u / d;             // unnormalize u to get remainder
              return q;
          }
          int i = 0; int64_t stop;
          while(u.digits[start+i] == v.digits[v.exp()+i] && v.exp() + i) i--;
          if (u.digits[start+i] < v.digits[v.exp()+i]) stop = start - v.exp() -1;
          else stop = start - v.exp();

          qhat = int64_t(u.digits[start]);         //make a guess at qhat
          qhat = qhat*RADIX + u.digits[start-1];qhat/=v.digits[v.exp()];
          if (qhat > RADIX-1) qhat = RADIX-1;

          int64_t temp;
          //fast check to see if qhat is too big
          if (start-1) temp = u.digits[start-2]; else temp = 0;
          while (int64_t(v.digits[v.exp()-1])*qhat>(int64_t(u.digits[start])*RADIX + 
                      u.digits[start-1]-qhat*int64_t(v.digits[v.exp()]))*RADIX +
                      temp)
          qhat--;

          bigint work(v*qhat);
	  work.resize(u.exp()+1);

          //qhat still too big??
          while(start-stop < work.exp()){
	      --qhat;  
	      work = work - v;
	  }

          int64_t borrow = 0;                 //subtract work from u
          work.digits[work.exp()+1] = 0;
          for(i=stop;i<=start; i++){
             temp = int64_t(u.digits[i]) - int64_t(work.digits[i-stop]) + borrow;
             if (temp < 0){borrow = -1;temp+=RADIX;}
             else borrow = 0;
             u.digits[i] = temp;
          }

          v.digits[v.exp()+1] = 0;
          while (borrow < 0 && qhat) {//oops qhat was still too big
                                     //add back
             qhat--;
             int64_t carry = 0;
             for(i=stop;i<=start; i++){
                temp = u.digits[i] + v.digits[i-stop] + carry;
                carry = temp / RADIX;
                u.digits[i] = temp % RADIX;
             }
             borrow += carry;
          }

          q.digits[qpos--] = qhat;
          //work on next digit of u
	  start = u.exp() - 1;
	  u.resize(u.size()-1);
        }
     }
    
}


int32_t bigint::operator%(int32_t n) const 
{
    if (n == 0) throw range_error("Division by zero");
    if (n < 0) {
        n = -n;    
    }

    int64_t r = 0, rad = 1;
    for(uint32_t i = 0; i < digits.size(); i++) {
        r = (r + digits[i] * rad) % n;    
        rad = (rad * RADIX) % n;
    }
    if (sign == -1) {
        r = -r;    
    }
    return r;
}


int bigint::compare(const bigint& b1, const bigint& b2) {
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
        for(int i = b1.digits.size()-1; i >= 0; i--) {
            if (b1.digits[i] > b2.digits[i]) {
                return 1;    
            } else if (b1.digits[i] < b2.digits[i]) {
                return -1;    
            } 
        }    
        return 0;
    }    
}

bool bigint::operator<(const bigint& o) const {
    int c = bigint::compare(*this, o);        
    return c == -1;
}

bool bigint::operator>(const bigint& o) const {
    int c = bigint::compare(*this, o);        
    return c == 1;
}

bool bigint::operator<=(const bigint& o) const {
    int c = bigint::compare(*this, o);
    return c == -1 || c == 0;
}

bool bigint::operator>=(const bigint& o) const {
    int c = bigint::compare(*this, o);        
    return c == 1 || c == 0;
}


// Normalize so that 
// 1) All digits are 0 <= d < RADIX
// 2) Sign 1 or -1
// 3) Leading zero digits are removed
// 4) Fix sign for zero, ie. eliminate -0.
void bigint::normalize() 
{
    for(uint32_t i = 0; i < digits.size()-1; i++) {
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
        for(uint64_t i = 0; i < digits.size() - 1; i++) {
            digits[i] = RADIX - digits[i];
            digits[i+1] += 1;            
        }
        digits[digits.size()-1] = -digits[digits.size()-1];
    }

    uint64_t dsize = digits.size();
    for(uint32_t i = 0; i < dsize; i++) {
        if (digits[i] >= RADIX) {
            if (i+1 >= dsize) {
                resize(digits.size()+1);
            }        
            digits[i+1] += digits[i] / RADIX;        
            digits[i] %= RADIX;
        }    
    }
    
    uint32_t i = digits.size();
    for(; i > 1 && digits[i-1] == 0; i--);
    resize(i);
    
    // Fix sign for zero
    if (digits.size() == 1 && digits[0] == 0 && sign == -1) {
        sign = 1;   
    }
}

// To get a specific number of digits we pad with zeroes on the left
void bigint::resize(int32_t new_digits_num) {
    //cout << "Resizing: " << new_digits_num << endl;
    assert(new_digits_num > 0);
    digits.resize(new_digits_num, 0);        
}

int bigint::sizeInBits() const {
    int c = (digits.size()-1)*INT_BITS;
    int64_t n = digits[digits.size()-1];
    while(n != 0) {
	n >>= 1;
	c++;
    }	
    return c;
}

// TODO: Respect when the ostream is in dec or hex mode
ostream & operator<<(ostream &os, const bigint &b) {
    os << b.toString();
    return os;
}

bigint bigint::times_two() const {
    bigint r = *this;
    for(int32_t i = r.size()-1; i >= 0; i--) {
        r.digits[i] <<= 1;
        if (r.digits[i] >=  RADIX) {
            if (i+1 >= r.size()) {
                r.resize(r.size()+1);    
            }
            r.digits[i] -= RADIX;
            r.digits[i+1]++;
        }
    }
    return r;
}

// TODO: Optimize this. Squaring can be done with half as many mults.
bigint bigint::square() const {
    return *this * *this;        
}

// Returns this raised to the power p
// TODO: This could be done faster by replacing recursion with a loop and by just using one bigint modified inplace, ie. *= instead of *.
bigint bigint::expt(int power) const {
    if (power == 0) {
        return ONE;    
    } else if (power == 1 || this->is_one()) {
        return *this;    
    }        
    if (power % 2 == 0) {
        bigint r = this->expt(power/2);
        return r.square();
    } else {
        bigint r = this->expt(power-1);
        return *this * r;
    }        
}

// Newton's method
bigint bigint::sqrt() const {
    if (sign == -1) {
        throw range_error("Imaginary result");
    }
    if (is_zero()) {
        return bigint::ZERO;    
    }
    if (is_one()) {
        return bigint::ONE;    
    }
    
    // A good initial guess is the square root of the most significant digit times the square root of its radix part.
    bigint x_1 = ONE * 100;
    
    while(true) {
        bigint x_2 = (*this) / x_1;
        bigint diff =  x_2 - x_1;
        if (diff.is_zero()) {
            return x_1;         
        }
        if (diff.is_one()) {
            return x_2;        
        }
        x_1 = (x_1 + x_2) / 2;
       // cout << "Guess " << x_1 << endl;
    }
}


