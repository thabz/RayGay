
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

bigint::bigint(string str, uint32_t radix) 
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
    bigint b = (*this).abs();
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

/*
bigint bigint::operator/(const bigint &y) const 
{
    uint32_t n = digits.size();
    uint32_t t = y.digits.size();

    if (y.is_zero()) throw range_error("Division by zero");
    if (is_zero()) return ZERO;
    
    if (t > n) return ZERO;    
    
    // We could bail out quickly if o == 1 (return this) 
    // but I don't think the cost of the comparison makes 
    // it worthwhile.
    bigint x = *this;

    bigint q;
    bigint r;
    q.resize(n-t+1);
    r.resize(t);
}
*/

/*
// See http://fox.wikis.com/wc.dll?Wiki~MultiprecisionDivision~VFP
bigint bigint::operator/(const bigint &b) const {
    
    if (b.digits.size() == 1) {
        return *this / (b.sign * b.digits[0]);    
    }        
        
    bigint quotient = bigint::ZERO;
    bigint newdividend = *this;
    bigint divisor = b;
    bigint offset = bigint::ZERO;
    
    while(newdividend >= divisor) {
        int diffdigits = newdividend.digits.size() - divisor.digits.size();
        if (diffdigits > 0) {
            if (newdividend.digits[newdividend.digits.size()-1] > divisor.digits[divisor.digits.size()-1]) {
                offset = bigint::ZERO;
                offset.resize(diffdigits+1);
                offset.digits[diffdigits] = 1;
                bigint newdivisor = divisor * offset;
                newdividend = newdividend - newdivisor;
                quotient += offset;
            } else {
                offset = bigint::ZERO;
                offset.resize(diffdigits);
                offset.digits[diffdigits-1] = 1;
                bigint newdivisor = divisor * offset;
                newdividend = newdividend - newdivisor;
                quotient += offset;
            }
        }
        if (newdividend.digits.size() == divisor.digits.size()) {
            newdividend -= divisor;
            quotient += 1;
        }
    }
    return quotient;  
}
*/

// Donald Knuth, The Art of Computer Programming, Volume 2, 2nd ed., 1981, pp. 257-258.
bigint bigint::operator/(const bigint &denom) const {
    bigint q = ZERO;
    q.resize(this->digits.size() - denom.size() + 1);
    q.sign = this->sign * denom.sign;
    int64_t q_hat; 
    int qpos = q.size()-1;
    int64_t d = RADIX / (denom.digits[denom.size()-1]+1);

    bigint u = *this;
    bigint v = denom;
    int64_t start = u.digits.size();
    u *= d;
    v *= d;
    if (u.digits.size()-1 < start) {
	u.resize(start+1);
	u.digits[start] = 0;
    }
    while(true) {
	if (u.abs() < v.abs()) {
	    u = u / d;
	    return q;
	}
	int i = 0; int64_t stop;
	while(u.digits[start+i] == v.digits[v.digits.size()-1+i] && v.digits.size()-1+i != 0) i--;
	if (u.digits[start+i]< v.digits[v.digits.size()-1+i]) 
	    stop = start - v.digits.size() - 1 -1;
	else 
	    stop = start - v.digits.size() - 1;
	
	q_hat = u.digits[start];
	q_hat = q_hat * RADIX + u.digits[start-1];
	q_hat /= v.digits[v.digits.size()-1];
	if (q_hat > RADIX - 1) q_hat = RADIX - 1;
	long temp;
	if (start-1 != 0) 
	    temp = u.digits[start-2];
	else 
	    temp = 0;
	while(v.digits[v.digits.size()-2]*q_hat >
	      (u.digits[start]*RADIX + u.digits[start-1] -
              q_hat*v.digits[v.digits.size()-1])*RADIX + temp)
	    q_hat--;

        bigint work = ONE;
        //work = v*q_hat << u.digits().size()-1;

	while(start-stop < work.digits.size()) {
	    --q_hat;
	    work -= v;
	}

	long borrow = 0;
	work.digits[work.digits.size()-1] = 0; // Eh?
	for(i = stop; i <= start; i++) {
            temp = u.digits[i] - work.digits[i-stop] + borrow;
            if (temp < 0){
                borrow = -1;
                temp += RADIX;
            }
            else borrow = 0;
            u.digits[i] = temp;
	}
	
	v.digits[v.digits.size()-1] = 0; // Eh?
        while (borrow < 0 && q_hat) {
            //oops qhat was still too big
            //add back
            q_hat--;
            long carry = 0;
            for(i = stop; i <= start; i++){
               temp = u.digits[i] + v.digits[i-stop] + carry;
               carry = temp / RADIX;
               u.digits[i] = temp % RADIX;
            }
            borrow += carry;
        }

        q.digits[qpos--] = q_hat;
        start = u.digits.size()-2;
        u.normalize();
    }
}

// Knuths algo in C: http://www.ddj.com/showArticle.jhtml?documentID=cuj9611breitz&pgno=8

/*
// Donald Knuth, The Art of Computer Programming, Volume 2, 2nd ed., 1981, pp. 257-258.
bigint bigint::operator/(const bigint &divisor) const {
    bigint u = *this;
    bigint v = divisor;
    int n = v.digits.size();
    int m = u.digits.size() - n;

    bigint q = 0;
    q.resize(m+1);
    
    // D1 (normalize)
    int32_t d = RADIX / (v.digits[v.digits.size()-1] + 1);
    u *= d;
    v *= d;
    
    // D2 (initialize j)
    for(int j = 0; j <= m; j++) {
        // D3 (calculate q_hat)
        int32_t q_hat;
        if (u.digits[j] == *(v.digits.end()-1)) {
            q_hat = RADIX - 1;          
        } else {
            q_hat = (u.digits[j]*RADIX + u.digits[j+1]) / (*(v.digits.end()-1));
        }
        while (*(v.digits.end()-2)*q_hat > (u.digits[j]*RADIX + u.digits[j-1]-q_hat*(*(v.digits.end()-1)))*RADIX + u.digits[j+2]) {
            q_hat--;        
        }
        
        // D4 (multiply and subtract)
        u -= v * q_hat; // FIXME: Do a local loop on the correct digits
        
        // D5 (test remainder)
        q.digits[j] = q_hat;
        if (u.sign == -1) {
            // D6 (add back)
            q.digits[j] -= 1;
            u += v; // FIXME: Do a local loop on the correct digits
        }
        // D7 (loop on j)
    }
    return q;
}
*/

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
    for(uint32_t i = r.size()-1; i >= 0; i--) {
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
        cout << "Guess " << x_1 << endl;
    }
}

bigint bigint::abs() const {
    bigint r = *this;    
    r.sign = 1;
    return r;    
}

