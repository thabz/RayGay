
#ifndef RATIONAL_H
#define RATIONAL_H

// Forward declaration
template<typename K> class rational;

template<typename K> double sin(const rational<K>&);
template<typename K> double cos(const rational<K>&);
template<typename K> rational<K> pow(const rational<K>&, const K&);
template<typename K> K floor(const rational<K>&);
template<typename K> K ceil(const rational<K>&);
template<typename K> K round(const rational<K>&);
template<typename K> K trunc(const rational<K>&);

template<typename K>
class rational 
{
    public:
        typedef K value_type;    
        
        rational(const K& n, const K& d);
        rational(const K& z);
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
        
        void normalize();

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

template<typename K> 
inline rational<K>::rational(const K& _n, const K& _d)  
: n(_n), d(_d) {}; 

template<typename K> 
inline rational<K>::rational(const K& _z)  
: n(_z), d(K(1)) {}; 

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
inline void 
rational<K>::normalize() 
{
    K gcd = rational_gcd<K>(n, d);
    if (gcd != K(1)) {
        n /= gcd;
        d /= gcd;
    }
    if (d < 0) {
        n = -n;    
        d = -d;
    }
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
    return x.numerator() == y.numerator() && 
           x.denominator() == y.denominator();
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
    return x.numerator() != y.numerator() || 
           x.denominator() != y.denominator();
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

// Returns a^b
template<typename K> 
inline rational<K> 
pow(const rational<K>& a, const K& b) 
{
   if (b == 0) return rational<K>(1);
   
   K iter = b < 0 ? -b : b;    

   // The following algorithm while slow, is OK for the relative small long values.
   rational<K> result = 1;
   while (iter-- > 0) {
       result *= a;        
   }
   if (b > 0) {
       return result;
   } else {
       return K(1) / result;           
   }            
}

template<typename K> 
inline K 
floor(const rational<K>& z) {
    K q = z.numerator() / z.denominator();
    if (q >= 0 && z.numerator() > 0) {
        return q;    
    } else {
        return q-1;
    }
}

template<typename K> 
inline K 
ceil(const rational<K>& z) {
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
    return z < 0 ? -floor(-z) : floor(z);        
}


#endif /* RATIONAL_H */
