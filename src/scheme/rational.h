
#ifndef RATIONAL_H
#define RATIONAL_H

#include <limits>
#include <sstream>

// Forward declaration
template<typename K> class rational;

template<typename K> double sin(const rational<K>&);
template<typename K> double cos(const rational<K>&);
template<typename K> double tan(const rational<K>&);
template<typename K> double exp(const rational<K>&);
template<typename K> double sqrt(const rational<K>&);
template<typename K> rational<K> pow(const rational<K>&, const K&);
template<typename K> rational<K> abs(const rational<K>&);
template<typename K> K floor(const rational<K>&);
template<typename K> K ceil(const rational<K>&);
template<typename K> K round(const rational<K>&);
template<typename K> K trunc(const rational<K>&);

template<typename K>
class rational 
{
    public:
        typedef K value_type;    
        
        rational(const K& n = K(0), const K& d = K(1));
        //rational(const K& z);
        template<typename N>
        rational(const rational<N>&);
                
        const K& numerator() const;
        K& numerator();
        const K& denominator() const;
        K& denominator();

        rational<K>& operator=(const K&); 
        rational<K>& operator+=(const K&); 
        rational<K>& operator-=(const K&); 
        rational<K>& operator*=(const K&); 
        rational<K>& operator/=(const K&);

        template<typename N>
        rational<K>& operator=(const rational<N>&); 
        template<typename N>
        rational<K>& operator+=(const rational<N>&); 
        template<typename N>
        rational<K>& operator-=(const rational<N>&); 
        template<typename N>
        rational<K>& operator*=(const rational<N>&); 
        template<typename N>
        rational<K>& operator/=(const rational<N>&);
        
        // Returns 1/z
        rational<K> inverse() const;
        
        rational<K> normalized() const;

        double real() const;
        
    private:
        K n;
        K d;
};

/////////////////////////////////////////////////////
// Getters
/////////////////////////////////////////////////////

template<typename K> 
inline const K& rational<K>::numerator() const {
    return n;        
}

template<typename K> 
inline K& rational<K>::numerator() {
    return n;        
}

template<typename K> 
inline const K& rational<K>::denominator() const {
    return d;        
}

template<typename K> 
inline K& rational<K>::denominator() {
    return d;        
}

template<typename K> 
inline double rational<K>::real() const {
    return double(n) / double(d);
}

template<typename K>
inline rational<K> 
rational<K>::inverse() const {
    return rational<K>(d,n);
}

/////////////////////////////////////////////////////
// Constructors
/////////////////////////////////////////////////////

// TODO: Should we throw an exception when _d == 0?
template<typename K> 
inline rational<K>::rational(const K& _n, const K& _d)  
: n(_n), d(_d) {}; 

/*
template<typename K> 
inline rational<K>::rational(const K& _z)  
: n(_z), d(K(1)) {}; 
*/

/////////////////////////////////////////////////////
// Normalize
/////////////////////////////////////////////////////

template<typename K>
inline K
rational_gcd(K a, K b) {
    K t = a;
    while (b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

template<typename K>
inline rational<K> 
rational<K>::normalized() const 
{
    K gcd = rational_gcd<K>(numerator(), denominator());
    K nn = numerator();
    K dd = denominator();
    if (gcd != K(1)) {
        nn /= gcd;
        dd /= gcd;
    }
    if (dd < 0) {
        nn = -nn;    
        dd = -dd;
    }
    return rational<K>(nn,dd);
}

/////////////////////////////////////////////////////
// Operators
/////////////////////////////////////////////////////

template<typename K>
inline rational<K>& 
rational<K>::operator=(const K& o) 
{
    n = o;
    d = K(1);
    return *this;
}

// Using a/b + c = (a+bc)/b
template<typename K>
inline rational<K>& 
rational<K>::operator+=(const K& o) 
{
    n += o * d;
    return *this;
}

// Using a/b + c = (a-bc)/b
template<typename K>
inline rational<K>& 
rational<K>::operator-=(const K& o) 
{
    n -= o * d;
    return *this;
}

// Using a/b * c = ac/b
template<typename K>
inline rational<K>& 
rational<K>::operator*=(const K& o) 
{
    n *= o;
    return *this;
}

// Using a/b / d = a/bd = a/b * 1/d
template<typename K>
inline rational<K>& 
rational<K>::operator/=(const K& o) 
{
    d *= o;
    return *this;
}

template<typename K>
template<typename N>
inline rational<K>& 
rational<K>::operator=(const rational<N>& o) 
{
    n = K(o.numerator());
    d = K(o.denominator());
    return *this;
}


// Using a/b + c/d = (ad + bc)/bd
template<typename K>
template<typename N>
inline rational<K>& 
rational<K>::operator+=(const rational<N>& o) 
{
    n = n * o.denominator() + d * o.numerator();
    d *= o.denominator();
    return *this;
}

// Using a/b - c/d = (ad - bc)/bd
template<typename K>
template<typename N>
inline rational<K>& 
rational<K>::operator-=(const rational<N>& o) 
{
    n = n * o.denominator() - d * o.numerator();
    d *= o.denominator();
    return *this;
}

// Using a/b * c/d = ac/bd        
template<typename K>
template<typename N>
inline rational<K>& 
rational<K>::operator*=(const rational<N>& o) 
{
    n *= o.numerator();        
    d *= o.denominator();        
    return *this;
}

// Using a/b / c/d = ad/bc = a/b * d/c
template<typename K>
template<typename N>
inline rational<K>& 
rational<K>::operator/=(const rational<N>& o) 
{
    n *= o.denominator();
    d *= o.numerator();
    return *this;
}

/////////////////////////////////////////////////////
// Operator+
/////////////////////////////////////////////////////

template<typename K>
inline rational<K> 
operator+(const rational<K>& x, const rational<K>& y) 
{
    rational<K> r = x;
    r += y;
    return r;
}

template<typename K>
inline rational<K> 
operator+(const K& x, const rational<K>& y) 
{
    rational<K> r = x;
    r += y;
    return r;
}

template<typename K>
inline rational<K> 
operator+(const rational<K>& x, const K& y) 
{
    rational<K> r = x;
    r += y;
    return r;
}

template<typename K>
inline rational<K> 
operator+(const rational<K>& x) 
{
    return x;
}

/////////////////////////////////////////////////////
// Operator-
/////////////////////////////////////////////////////

template<typename K>
inline rational<K> 
operator-(const rational<K>& x, const rational<K>& y) 
{
    rational<K> r = x;
    r -= y;
    return r;
}

template<typename K>
inline rational<K> 
operator-(const K& x, const rational<K>& y) 
{
    rational<K> r = x;
    r -= y;
    return r;
}

template<typename K>
inline rational<K> 
operator-(const rational<K>& x, const K& y) 
{
    rational<K> r = x;
    r -= y;
    return r;
}

// Negation
template<typename K>
inline rational<K> 
operator-(const rational<K>& x) 
{
    return rational<K>(-x.numerator(), x.denominator());
}

/////////////////////////////////////////////////////
// Operator*
/////////////////////////////////////////////////////

template<typename K>
inline rational<K> 
operator*(const rational<K>& x, const rational<K>& y) 
{
    rational<K> r = x;
    r *= y;
    return r;
}

template<typename K>
inline rational<K> 
operator*(const K& x, const rational<K>& y) 
{
    rational<K> r = x;
    r *= y;
    return r;
}

template<typename K>
inline rational<K> 
operator*(const rational<K>& x, const K& y) 
{
    rational<K> r = x;
    r *= y;
    return r;
}

/////////////////////////////////////////////////////
// Operator/
/////////////////////////////////////////////////////

template<typename K>
inline rational<K> 
operator/(const rational<K>& x, const rational<K>& y) 
{
    rational<K> r = x;
    r /= y;
    return r;
}

template<typename K>
inline rational<K> 
operator/(const K& x, const rational<K>& y) 
{
    rational<K> r = x;
    r /= y;
    return r;
}

template<typename K>
inline rational<K> 
operator/(const rational<K>& x, const K& y) 
{
    rational<K> r = x;
    r /= y;
    return r;
}

/////////////////////////////////////////////////////
// Operator==
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator==(const rational<K>& x, const rational<K>& y) 
{
    return x.numerator() * y.denominator() == 
           x.denominator() * y.numerator();
}

template<typename K>
inline bool
operator==(const K& x, const rational<K>& y) 
{
    return x * y.denominator() == y.numerator();
}

template<typename K>
inline bool
operator==(const rational<K>& x, const K& y) 
{
    return y * x.denominator() == x.numerator();
}

/////////////////////////////////////////////////////
// Operator!=
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator!=(const rational<K>& x, const rational<K>& y) 
{
   return x.numerator() * y.denominator() != 
           x.denominator() * y.numerator();
}

template<typename K>
inline bool
operator!=(const K& x, const rational<K>& y) 
{
    return x * y.denominator() != y.numerator();
}

template<typename K>
inline bool
operator!=(const rational<K>& x, const K& y) 
{
    return y * x.denominator() != x.numerator();
}

/////////////////////////////////////////////////////
// Operator<
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator<(const rational<K>& x, const rational<K>& y) 
{
   if ((x.denominator() > 0) == (y.denominator() > 0)) {
       return x.numerator() * y.denominator() < 
              x.denominator() * y.numerator();
   } else {
       return x.numerator() * y.denominator() > 
              x.denominator() * y.numerator();
   }
}

template<typename K>
inline bool
operator<(const K& x, const rational<K>& y) 
{
    if (y.denominator() > 0) {        
        return x * y.denominator() < y.numerator();
    } else {
        return x * y.denominator() > y.numerator();
    }
}

template<typename K>
inline bool
operator<(const rational<K>& x, const K& y) 
{
    if (x.denominator() > 0) {
        return x.numerator() < x.denominator() * y;
    } else {
        return x.numerator() > x.denominator() * y;
    }
}

/////////////////////////////////////////////////////
// Operator>
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator>(const rational<K>& x, const rational<K>& y) 
{
   if ((x.denominator() > 0) == (y.denominator() > 0)) {
       return x.numerator() * y.denominator() > 
              x.denominator() * y.numerator();
   } else {
       return x.numerator() * y.denominator() < 
              x.denominator() * y.numerator();
   }
}

template<typename K>
inline bool
operator>(const K& x, const rational<K>& y) 
{
    if (y.denominator() > 0) {        
        return x * y.denominator() > y.numerator();
    } else {
        return x * y.denominator() < y.numerator();
    }
}

template<typename K>
inline bool
operator>(const rational<K>& x, const K& y) 
{
    if (x.denominator() > 0) {
        return x.numerator() > x.denominator() * y;
    } else {
        return x.numerator() < x.denominator() * y;
    }
}

////////////////////////////////////////////////////
// Operator>=
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator>=(const rational<K>& x, const rational<K>& y) 
{
   if ((x.denominator() > 0) == (y.denominator() > 0)) {
       return x.numerator() * y.denominator() >= 
              x.denominator() * y.numerator();
   } else {
       return x.numerator() * y.denominator() <= 
              x.denominator() * y.numerator();
   }
}

template<typename K>
inline bool
operator>=(const K& x, const rational<K>& y) 
{
    if (y.denominator() > 0) {        
        return x * y.denominator() >= y.numerator();
    } else {
        return x * y.denominator() <= y.numerator();
    }
}

template<typename K>
inline bool
operator>=(const rational<K>& x, const K& y) 
{
    if (x.denominator() > 0) {
        return x.numerator() >= x.denominator() * y;
    } else {
        return x.numerator() <= x.denominator() * y;
    }
}

/////////////////////////////////////////////////////
// Operator<=
/////////////////////////////////////////////////////

template<typename K>
inline bool
operator<=(const rational<K>& x, const rational<K>& y) 
{
   if ((x.denominator() > 0) == (y.denominator() > 0)) {
       return x.numerator() * y.denominator() <= 
              x.denominator() * y.numerator();
   } else {
       return x.numerator() * y.denominator() >= 
              x.denominator() * y.numerator();
   }
}

template<typename K>
inline bool
operator<=(const K& x, const rational<K>& y) 
{
    if (y.denominator() > 0) {        
        return x * y.denominator() <= y.numerator();
    } else {
        return x * y.denominator() >= y.numerator();
    }
}

template<typename K>
inline bool
operator<=(const rational<K>& x, const K& y) 
{
    if (x.denominator() > 0) {
        return x.numerator() <= x.denominator() * y;
    } else {
        return x.numerator() >= x.denominator() * y;
    }

}

/////////////////////////////////////////////////////
// Transcendentals
/////////////////////////////////////////////////////

template<typename K> 
inline double 
sin(const rational<K>& z) {
    return ::sin(z.real());            
}

template<typename K> 
inline double 
cos(const rational<K>& z) {
    return ::cos(z.real());            
}

template<typename K> 
inline double 
tan(const rational<K>& z) {
    return ::tan(z.real());            
}

template<typename K> 
inline double 
exp(const rational<K>& z) {
    return ::exp(z.real());            
}

template<typename K> 
inline double 
sqrt(const rational<K>& z) {
    return ::sqrt(z.real());            
}

// Returns a^b
template<typename K> 
inline rational<K> 
pow(const rational<K>& a, const K& b) 
{
   if (b == 0) return rational<K>(1);
   if (a.numerator() == 0) return rational<K>(0);
   
   K iter = b < 0 ? -b : b;
   rational<K> x = a;
   rational<K> result = iter % 2 ? x : 1;
   while (iter >>= 1) {
       x *= x;          
       if (iter % 2) {
           result *= x;
       } 
   }
   if (b > 0) {
       return result;
   } else {
       return result.inverse();           
   }            
}

template<typename K> 
inline rational<K> 
abs(const rational<K>& a) 
{
    return a < K(0) ? -a : a;
}

///  Insertion operator for rational values.
template<typename _Tp, typename _CharT, class _Traits>
std::basic_ostream<_CharT, _Traits>&
operator<<(std::basic_ostream<_CharT, _Traits>& __os, const rational<_Tp>& __x)
  {
    std::basic_ostringstream<_CharT, _Traits> __s;
    __s.flags(__os.flags());
    __s.imbue(__os.getloc());
    __s.precision(__os.precision());
    __s << __x.numerator() << '/' << __x.denominator();
    return __os << __s.str();
}


/////////////////////////////////////////////////////
// Roundings
/////////////////////////////////////////////////////

template<typename K> 
inline K 
floor(const rational<K>& a) {
    rational<K> z = a.normalized();
    if (z.denominator() == 1) return z.numerator();
    K q = z.numerator() / z.denominator();
    if (q >= 0 && z.numerator() > 0) {
        return q;    
    } else {
        return q-1;
    }
}

template<typename K> 
inline K 
ceil(const rational<K>& a) {
    rational<K> z = a.normalized();
    if (z.denominator() == 1) return z.numerator();
    K q = z.numerator() / z.denominator();
    if (q >= 0 && z.numerator() > 0) {
        return q+1;    
    } else {
        return q;
    }
}

// trunc(z) = z < 0 ? -floor(-z) : floor(z)
template<typename K> 
inline K 
trunc(const rational<K>& z) {
    return z < K(0) ? -floor(-z) : floor(z);        
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
template<typename K> 
inline K 
round(const rational<K>& z) {
    K flo = floor(z);        
    K cei = ceil(z);        
    rational<K> dflo = z - flo;        
    rational<K> dcei = cei - z;        
    if (dflo < dcei) {
        return cei;    
    } else if (dcei > dflo) {
        return flo;    
    } else {
        return flo % K(2) == 0 ? flo : cei;
    }        
}

/////////////////////////////////////////////////////
// Specialization of numeric_limits
/////////////////////////////////////////////////////

namespace std {
  template<typename K>
    struct numeric_limits<rational<K> >
    {
      static const bool is_specialized = true;

      static rational<K> min() throw()
      { return rational<K>(numeric_limits<K>::min(), 1); }
      static rational<K> max() throw()
      { return rational<K>(numeric_limits<K>::max(), 1); }

      static const int digits = numeric_limits<K>::digits;
      static const int digits10 = numeric_limits<K>::digits10;
      static const bool is_signed = true;
      static const bool is_integer = false;
      static const bool is_exact = true;
      static const int radix = 2;
      static rational<K> epsilon() throw()
      { return rational<K>(1, numeric_limits<K>::max()); }
      static rational<K> round_error() throw()
      { return rational<K>(0); }

      static const int min_exponent = 0;
      static const int min_exponent10 = 0;
      static const int max_exponent = 0;
      static const int max_exponent10 = 0;

      static const bool has_infinity = false;
      static const bool has_quiet_NaN = false;
      static const bool has_signaling_NaN = false;
      static const float_denorm_style has_denorm = denorm_absent;
      static const bool has_denorm_loss = false;

      static rational<K> infinity() throw()
      { return rational<K>(0); }
      static rational<K> quiet_NaN() throw()
      { return rational<K>(0); }
      static rational<K> signaling_NaN() throw()
      { return rational<K>(0); }
      static rational<K> denorm_min() throw()
      { return rational<K>(0); }

      static const bool is_iec559 = false;
      static const bool is_bounded = true;
      static const bool is_modulo = true;

      static const bool traps = __glibcxx_integral_traps;
      static const bool tinyness_before = false;
        static const float_round_style round_style = round_toward_zero;
    };
}

#endif /* RATIONAL_H */
