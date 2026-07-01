
#include "bigint.h"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <stdexcept>

using namespace std;

#define MININT (1 << (32 - 1))
#define MAXINT (~MININT)
#define INT_BITS 31

int64_t bigint::RADIX = (int64_t(1) << 31);

bigint bigint::_ZERO = bigint(0);
bigint bigint::_ONE = bigint(1);
bigint bigint::_TWO = bigint(2);

const bigint &bigint::ONE = _ONE;
const bigint &bigint::TWO = _TWO;
const bigint &bigint::ZERO = _ZERO;

bigint::~bigint(){};

bigint::bigint(int32_t n) {
  sign = 1;
  digits.push_back(n);
  normalize();
}

bigint::bigint(const string &str, uint32_t radix) {
  if (radix >= 37 || radix == 0)
    throw invalid_argument("Invalid radix");

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
      std::string msg = "Not a valid digit: ";
      msg.push_back(c);
      throw invalid_argument(msg);
    }

    if (digit >= radix) {
      std::string msg = "Not a valid digit: ";
      msg.push_back(c);
      throw invalid_argument(msg);
    }

    *(this) *= radix;
    *(this) += digit;
    i++;
  }
  normalize();
  sign = is_zero() ? 1 : fsign;
}

bigint::bigint(const bigint &o) {
  digits = o.digits;
  sign = o.sign;
}

// TODO: This is very slow
string bigint::toString(uint32_t radix) const {
  char chars[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
  if (radix >= 37 || radix == 0)
    throw invalid_argument("Invalid radix");
  if (is_zero()) {
    return "0";
  }
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

bool bigint::is_zero() const { return digits.size() == 1 && digits[0] == 0; }

bool bigint::is_one() const {
  return digits.size() == 1 && digits[0] == 1 && sign == 1;
}

void bigint::dump() const {
  cout << "Sign: " << sign << endl;
  cout << "Digits: " << digits.size() << endl;
  for (uint32_t i = 0; i < digits.size(); i++) {
    cout << "   digit[" << i << "]: " << digits[i] << endl;
  }
}

bool bigint::operator==(const bigint &o) const {
  if (digits.size() != o.digits.size() || sign != o.sign) {
    return false;
  }
  for (uint32_t i = 0; i < digits.size(); i++) {
    if (digits[i] != o.digits[i])
      return false;
  }
  return true;
}

bool bigint::operator!=(const bigint &o) const { return !(*this == o); }

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

bigint &bigint::operator+=(int32_t n) {
  if (sign == 1) {
    digits[0] += n;
  } else {
    digits[0] -= n;
  }
  normalize();
  return *this;
}

bigint &bigint::operator+=(const bigint &v) {
  resize(max(digits.size(), v.digits.size()));
  for (uint32_t i = 0; i < digits.size(); i++) {
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
  for (uint32_t i = 0; i < r.digits.size(); i++) {
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
  r.normalize();
  return r;
}

bigint &bigint::operator-=(int32_t n) {
  if (sign == 1) {
    digits[0] -= n;
  } else {
    digits[0] += n;
  }
  normalize();
  return *this;
}

bigint &bigint::operator-=(const bigint &v) {
  resize(max(digits.size(), v.digits.size()));
  for (uint32_t i = 0; i < digits.size(); i++) {
    if (sign == v.sign) {
      digits[i] -= v.digits[i];
    } else {
      digits[i] += v.digits[i];
    }
  }
  normalize();
  return *this;
}

bigint bigint::operator*(const bigint &o) const {
  bigint r = ZERO;

  int size = o.digits.size() + digits.size();
  r.resize(size);
  for (uint32_t i = 0; i < digits.size(); i++) {
    for (uint32_t j = 0; j < o.digits.size(); j++) {
      r.digits[i + j] += digits[i] * o.digits[j];
    }
    r.normalize();
    r.resize(size);
  }
  r.normalize();
  r.sign = r.is_zero() ? 1 : sign * o.sign;
  return r;
}

bigint bigint::operator*(int32_t n) const {
  bigint r = *this;

  if (n < 0) {
    n = -n;
    r.sign = -r.sign;
  }
  for (uint32_t i = 0; i < r.digits.size(); i++) {
    r.digits[i] *= n;
  }
  r.normalize();
  return r;
}

bigint &bigint::operator*=(int32_t n) {
  if (n < 0) {
    n = -n;
    sign = -sign;
  }
  for (uint32_t i = 0; i < digits.size(); i++) {
    digits[i] *= n;
  }
  normalize();
  return *this;
}

bigint bigint::operator/(int32_t n) const {
  bigint s = *this;
  if (n == 0)
    throw range_error("Division by zero");
  if (n < 0) {
    n = -n;
    s.sign = -s.sign;
  }
  int64_t r = 0;
  for (int i = s.digits.size() - 1; i >= 0; i--) {
    r = r * RADIX + s.digits[i];
    s.digits[i] = r / n;
    r %= n;
  }
  s.normalize();
  return s;
}

bigint bigint::divmod(const bigint &denom, bigint *remainder_out) const {
  if (denom.is_zero())
    throw range_error("Division by zero");

  bigint numerator = abs(*this);
  bigint divisor = abs(denom);

  if (numerator < divisor) {
    if (remainder_out)
      *remainder_out = *this;
    return bigint(0);
  }

  if (numerator == divisor) {
    if (remainder_out)
      *remainder_out = bigint(0);
    bigint quotient(1);
    quotient.sign = sign * denom.sign;
    quotient.normalize();
    return quotient;
  }

  if (divisor.exp() == 0) {
    int32_t divisor_digit = divisor.digits[0];
    if (remainder_out)
      *remainder_out = bigint(*this % divisor_digit);
    return *this / (denom.sign * divisor_digit);
  }

  bigint remainder(0);
  remainder = numerator;

  int max_shift = int(numerator.exp()) - int(divisor.exp());
  bigint quotient(0);
  quotient.resize(max_shift + 1);

  for (int shift = max_shift; shift >= 0; shift--) {
    bigint shifted_divisor = divisor;
    if (shift > 0)
      shifted_divisor.digits.insert(shifted_divisor.digits.begin(), shift, 0);

    if (shifted_divisor > remainder)
      continue;

    int64_t low = 0;
    int64_t high = RADIX - 1;
    int64_t qdigit = 0;

    while (low <= high) {
      int64_t mid = low + (high - low) / 2;
      bigint product = shifted_divisor * int32_t(mid);
      if (product <= remainder) {
        qdigit = mid;
        low = mid + 1;
      } else {
        high = mid - 1;
      }
    }

    quotient.digits[shift] = qdigit;
    if (qdigit != 0)
      remainder -= shifted_divisor * int32_t(qdigit);
  }

  quotient.sign = sign * denom.sign;
  quotient.normalize();

  if (remainder_out) {
    remainder.sign = sign;
    remainder.normalize();
    *remainder_out = remainder;
  }

  return quotient;
}

bigint bigint::operator/(const bigint &denom) const {
  return divmod(denom, 0);
}

int32_t bigint::operator%(int32_t n) const {
  if (n == 0)
    throw range_error("Division by zero");
  if (n < 0) {
    n = -n;
  }

  int64_t r = 0, rad = 1;
  for (uint32_t i = 0; i < digits.size(); i++) {
    r = (r + digits[i] * rad) % n;
    rad = (rad * RADIX) % n;
  }
  if (sign == -1) {
    r = -r;
  }
  return r;
}

bigint bigint::operator%(const bigint &denom) const {
  bigint remainder(0);
  divmod(denom, &remainder);
  return remainder;
}

int bigint::compare(const bigint &b1, const bigint &b2) {
  if (b1.sign > b2.sign) {
    return 1;
  } else if (b1.sign < b2.sign) {
    return -1;
  } else if (b1.digits.size() > b2.digits.size()) {
    return b1.sign;
  } else if (b1.digits.size() < b2.digits.size()) {
    return -b1.sign;
  } else {
    // Same number of digits and same sign. Compare digits.
    for (int i = b1.digits.size() - 1; i >= 0; i--) {
      if (b1.digits[i] > b2.digits[i]) {
        return b1.sign;
      } else if (b1.digits[i] < b2.digits[i]) {
        return -b1.sign;
      }
    }
    return 0;
  }
}

bool bigint::operator<(const bigint &o) const {
  int c = bigint::compare(*this, o);
  return c == -1;
}

bool bigint::operator>(const bigint &o) const {
  int c = bigint::compare(*this, o);
  return c == 1;
}

bool bigint::operator<=(const bigint &o) const {
  int c = bigint::compare(*this, o);
  return c == -1 || c == 0;
}

bool bigint::operator>=(const bigint &o) const {
  int c = bigint::compare(*this, o);
  return c == 1 || c == 0;
}

// Normalize so that
// 1) All digits are 0 <= d < RADIX
// 2) Sign 1 or -1
// 3) Leading zero digits are removed
// 4) Fix sign for zero, ie. eliminate -0.
void bigint::normalize() {
  for (uint32_t i = 0; i < digits.size() - 1; i++) {
    if (digits[i] < 0) {
      digits[i + 1] += digits[i] / RADIX - 1;
      digits[i] %= RADIX;
      if (digits[i] != 0) {
        digits[i] += RADIX;
      } else {
        digits[i + 1] += 1;
      }
    }
  }

  if (digits[digits.size() - 1] < 0) {
    sign = -sign;
    for (uint64_t i = 0; i < digits.size() - 1; i++) {
      digits[i] = RADIX - digits[i];
      digits[i + 1] += 1;
    }
    digits[digits.size() - 1] = -digits[digits.size() - 1];
  }

  uint64_t dsize = digits.size();
  for (uint32_t i = 0; i < dsize; i++) {
    if (digits[i] >= RADIX) {
      if (i + 1 >= dsize) {
        resize(digits.size() + 1);
      }
      digits[i + 1] += digits[i] / RADIX;
      digits[i] %= RADIX;
    }
  }

  uint32_t i = digits.size();
  for (; i > 1 && digits[i - 1] == 0; i--)
    ;
  resize(i);

  // Fix sign for zero
  if (digits.size() == 1 && digits[0] == 0 && sign == -1) {
    sign = 1;
  }
}

// To get a specific number of digits we pad with zeroes on the left
void bigint::resize(int32_t new_digits_num) {
  // cout << "Resizing: " << new_digits_num << endl;
  assert(new_digits_num > 0);
  digits.resize(new_digits_num, 0);
}

int bigint::sizeInBits() const {
  int c = (digits.size() - 1) * INT_BITS;
  int64_t n = digits[digits.size() - 1];
  while (n != 0) {
    n >>= 1;
    c++;
  }
  return c;
}

// TODO: Respect when the ostream is in dec or hex mode
ostream &operator<<(ostream &os, const bigint &b) {
  os << b.toString();
  return os;
}

bigint bigint::times_two() const {
  bigint r = *this;
  uint32_t size = r.size();
  for (uint32_t i = 0; i < size; i++) {
    r.digits[i] <<= 1;
    if (r.digits[i] >= RADIX) {
      if (i + 1 >= r.size()) {
        r.resize(r.size() + 1);
      }
      r.digits[i] -= RADIX;
      r.digits[i + 1]++;
    }
  }
  return r;
}

bigint bigint::square() const {
  if (digits.size() < 4) {
    return *this * *this;
  }

  bigint r = ZERO;
  int size = digits.size() * 2;
  r.resize(size);

  for (uint32_t i = 0; i < digits.size(); i++) {
    r.digits[i + i] += digits[i] * digits[i];
    for (uint32_t j = i + 1; j < digits.size(); j++) {
      r.digits[i + j] += 2 * digits[i] * digits[j];
    }
    r.normalize();
    r.resize(size);
  }
  r.normalize();
  return r;
}

// Returns this raised to the power p
bigint bigint::expt(int power) const {
  if (power < 0) {
    throw range_error("Negative exponent");
  }

  bigint result = ONE;
  bigint base = *this;
  while (power > 0) {
    if (power % 2 == 1) {
      result = result * base;
    }
    power /= 2;
    if (power > 0) {
      base = base.square();
    }
  }
  return result;
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

  bigint x = TWO.expt((sizeInBits() + 1) / 2);
  while (true) {
    bigint next = (x + (*this) / x) / 2;
    if (next >= x) {
      return x;
    }
    x = next;
  }
}
