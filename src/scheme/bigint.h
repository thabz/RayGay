
#ifndef SCHEME_BIGINT
#define SCHEME_BIGINT

#include <ostream>
#include <stdint.h>
#include <string>
#include <vector>

class bigint;

bigint abs(const bigint &);

// The code asserts int is 32 bits
// http://cs.marlboro.edu/term/cs-fall02/algorithms/crypto/RSA/bigint/
class bigint {

  friend std::ostream &operator<<(std::ostream &os, const bigint &b);
  friend bigint abs(const bigint &);

public:
  bigint(int32_t n);
  bigint(const std::string &str, uint32_t radix = 10);
  bigint(const bigint &o);
  ~bigint();

  bool fitsInt64() const;
  int64_t toInt64() const;
  bool fitsInt32() const;
  int32_t toInt32() const;
  std::string toString(uint32_t radix = 10) const;

  bigint &operator+=(const bigint &b);
  bigint &operator+=(int32_t n);
  bigint &operator-=(const bigint &b);
  bigint &operator-=(int32_t n);
  bigint &operator*=(int32_t n);

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
  bool operator==(const bigint &o) const;
  bool operator!=(const bigint &o) const;
  bool operator<(const bigint &o) const;
  bool operator>(const bigint &o) const;
  bool operator<=(const bigint &o) const;
  bool operator>=(const bigint &o) const;

  bigint sqrt() const;
  bigint expt(int power) const;
  bigint times_two() const;
  bigint square() const;

  bool is_zero() const;
  bool is_one() const;
  int sizeInBits() const;
  void dump() const;

public:
  static const bigint &ZERO;
  static const bigint &ONE;
  static const bigint &TWO;

private:
  static bigint _ZERO;
  static bigint _ONE;
  static bigint _TWO;
  static int64_t RADIX;

private:
  void normalize();
  static int compare(const bigint &b1, const bigint &b2);
  void resize(int32_t new_digits_num);
  uint32_t size() const;
  uint32_t exp() const;

private:
  std::vector<int64_t> digits;
  int sign;
};

#endif

inline uint32_t bigint::size() const { return digits.size(); }

inline uint32_t bigint::exp() const {
  int i = size() - 1;
  while (digits[i] == 0 && i > 0)
    i--;
  return i;
}

inline bigint abs(const bigint &b) {
  bigint r = b;
  r.sign = 1;
  return r;
}
