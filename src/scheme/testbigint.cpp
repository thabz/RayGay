#include "bigint.h"
#include "testing.h"
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

class construction_test : public Test {
public:
  void run() {
    assertTrue(bigint(123456789) == bigint("123456789"));
    assertTrue(bigint(-100) == bigint("-100"));
    assertTrue(bigint("0") == bigint("-0"));
    assertTrue(bigint("0").is_zero());
    assertTrue(bigint("-0").is_zero());
    assertTrue(bigint::ZERO == bigint(0));
    assertTrue(bigint::ONE == bigint(1));
    assertTrue(bigint::TWO == bigint(2));

    bigint b1("10");
    bigint b2 = b1;
    b1 += 10;
    assertTrue(b2 == bigint("10"));
    assertTrue(b1 == bigint("20"));
  }
};

class radix_test : public Test {
public:
  void run() {
    assertTrue(bigint("ff", 16) == bigint("255"));
    assertTrue(bigint("FF", 16) == bigint("255"));
    assertTrue(bigint("10001", 2) == bigint("17"));
    assertTrue(bigint("z", 36) == bigint("35"));

    vector<int> radices = {2, 3, 8, 10, 16, 36};
    vector<string> numbers = {"0",
                              "1",
                              "-1",
                              "123456789123456789123456789",
                              "-987654321987654321987654321"};
    for (uint32_t i = 0; i < numbers.size(); i++) {
      bigint n(numbers[i]);
      for (uint32_t j = 0; j < radices.size(); j++) {
        int radix = radices[j];
        assertTrue(bigint(n.toString(radix), radix) == n);
      }
    }

    assertTrue(bigint(0).toString() == "0");
    assertTrue(bigint(1000).toString() == "1000");
    assertTrue(bigint(65535).toString(16) == "ffff");
    assertTrue(bigint(-1000).toString() == "-1000");
    assertTrue(bigint("123456789123456789").toString() == "123456789123456789");

    bool invalid_radix_failed = false;
    try {
      bigint("10", 37);
    } catch (invalid_argument &) {
      invalid_radix_failed = true;
    }
    assertTrue(invalid_radix_failed);

    bool invalid_digit_failed = false;
    try {
      bigint("2", 2);
    } catch (invalid_argument &) {
      invalid_digit_failed = true;
    }
    assertTrue(invalid_digit_failed);
  }
};

class sign_test : public Test {
public:
  void run() {
    assertTrue(-bigint(100) == bigint(-100));
    assertTrue(-bigint(-100) == bigint(100));
    assertTrue(-bigint(0) == bigint(0));
    assertTrue(bigint(-1000) + bigint(1000) == bigint(0));
    assertTrue(bigint("-1000") + bigint("1000") == bigint("0"));
    assertTrue(bigint("-1000") - bigint("-1000") == bigint("0"));
  }
};

class arithmetic_test : public Test {
public:
  void run() {
    assertTrue(bigint(100) - 20 == bigint(80));
    assertTrue(bigint(100) - bigint(3) == bigint(97));
    assertTrue(bigint("3333333333333333333") - bigint("2222222222222222222") ==
               bigint("1111111111111111111"));

    bigint accumulator("1000000000000000000000000000000");
    accumulator += bigint("777777777777777777777777777777");
    accumulator -= bigint("123456789123456789123456789123");
    assertTrue(accumulator == bigint("1654320988654320988654320988654"));

    assertTrue((bigint(123456789) + 123456789) + 123456789 ==
               bigint("370370367"));
    assertTrue(bigint("999999999999999999999999999999999") +
                   bigint("999999999999999999999999999999999") ==
               bigint("1999999999999999999999999999999998"));
    assertTrue(abs(bigint("-111111111111111111")) ==
               bigint("111111111111111111"));

    assertTrue(bigint(0) * 10 == bigint(0));
    assertTrue(bigint("1000") * 10 == bigint("10000"));
    assertTrue(bigint("1000") * -10 == bigint("-10000"));
    assertTrue(bigint("-1000") * -10 == bigint("10000"));
    assertTrue(bigint("123456789123456789") * bigint("123456789123456789") ==
               bigint("15241578780673678515622620750190521"));
    assertTrue((bigint("123456789") * 123456789) * 123456789 ==
               bigint("1881676371789154860897069"));

    bigint inplace("123456789123456789");
    inplace *= 1000;
    assertTrue(inplace == bigint("123456789123456789000"));
    assertTrue(bigint(1).times_two() == bigint(2));
    assertTrue(bigint("1073741824").times_two() == bigint("2147483648"));
    assertTrue(bigint("123456789").square() == bigint("15241578750190521"));
  }
};

class power_test : public Test {
public:
  void run() {
    assertTrue(bigint("100").expt(2) == bigint("10000"));
    assertTrue(bigint("2").expt(0) == bigint(1));
    assertTrue(bigint(0).expt(100) == bigint(0));
    assertTrue(bigint(31).expt(19) == bigint("21670662219970396194714277471"));
    assertTrue(bigint(17).expt(1000) * bigint(17).expt(500) ==
               bigint(17).expt(1500));
    assertTrue(bigint(31).expt(1000) * bigint(31).expt(1500) ==
               bigint(31).expt(2500));
  }
};

class division_test : public Test {
public:
  void run() {
    assertTrue(bigint("9999999999999999999") / 3 ==
               bigint("3333333333333333333"));
    assertTrue(bigint(-1000) / 10 == bigint(-100));
    assertTrue(bigint(1000) / (-10) == bigint(-100));
    assertTrue(bigint(-1000) / (-10) == bigint(100));
    assertTrue(bigint(100) / bigint(2) == bigint(50));
    assertTrue(bigint(100) / bigint(-2) == bigint(-50));
    assertTrue(bigint(100) / bigint(1000) == bigint(0));
    assertTrue(bigint(-100) / bigint(1000) == bigint(0));
    assertTrue(bigint(100) / bigint(-1000) == bigint(0));
    assertTrue(bigint("123456789123456789") / bigint("123456789123456789") ==
               bigint(1));
    assertTrue(bigint("10000000000") / bigint("1000000000") == bigint(10));
    assertTrue(bigint("10000000000") / bigint("10000000") == bigint(1000));
    assertTrue(bigint("993850124034") / bigint("1209237") == bigint("821882"));
    assertTrue(bigint("993850124034") / bigint("821882") == bigint("1209237"));
    assertTrue(bigint("123456789123456789") / bigint(1) ==
               bigint("123456789123456789"));
    assertTrue(bigint("10000000000") / bigint("10") == bigint("1000000000"));
    assertTrue(bigint("15241578780673678515622620750190521") / bigint("1") ==
               bigint("15241578780673678515622620750190521"));

    vector<bigint> numerators = {bigint("123456789123456789123456789"),
                                 bigint("-123456789123456789123456789"),
                                 bigint("999999999999999999999999999999")};
    vector<int> denominators = {3, 7, 97, -11};
    for (uint32_t i = 0; i < numerators.size(); i++) {
      for (uint32_t j = 0; j < denominators.size(); j++) {
        bigint n = numerators[i];
        int d = denominators[j];
        bigint q = n / d;
        int r = n % d;
        assertTrue(q * d + r == n);
      }
    }

    assertTrue(bigint(100) % 10 == 0);
    assertTrue(bigint("10000000000000000000000") % 3 == 1);
    assertTrue(bigint("-99999999999999999992") % 3 == -2);

    bool divide_by_zero_failed = false;
    try {
      bigint(1) / 0;
    } catch (range_error &) {
      divide_by_zero_failed = true;
    }
    assertTrue(divide_by_zero_failed);

    bool modulo_by_zero_failed = false;
    try {
      bigint(1) % 0;
    } catch (range_error &) {
      modulo_by_zero_failed = true;
    }
    assertTrue(modulo_by_zero_failed);
  }
};

class sqrt_test : public Test {
public:
  void run() {
    assertTrue(bigint(0).sqrt() == bigint(0));
    assertTrue(bigint(1).sqrt() == bigint(1));
    assertTrue(bigint(2).sqrt() == bigint(1));
    assertTrue(bigint(99).sqrt() == bigint(9));
    assertTrue(bigint(100).sqrt() == bigint(10));
    assertTrue(bigint(10000).sqrt() == bigint(100));
    assertTrue(bigint("10000000000000000").sqrt() == bigint("100000000"));

    bool negative_sqrt_failed = false;
    try {
      bigint(-1).sqrt();
    } catch (range_error &) {
      negative_sqrt_failed = true;
    }
    assertTrue(negative_sqrt_failed);
  }
};

class comparison_test : public Test {
public:
  void run() {
    assertTrue(bigint(50) > bigint(25));
    assertTrue(bigint("999999999999999999") > bigint("888888888888888888"));
    assertTrue(bigint("111111111111111111") < bigint("222222222222222222"));
    assertTrue(bigint("999999999999999999999999999999999") <=
               bigint("999999999999999999999999999999999"));
    assertTrue(bigint("8888888888888888888") <= bigint("9999999999999999999"));
    assertTrue(bigint("3333333333333333333") >= bigint("3333333333333333333"));
    assertTrue(bigint("3333333333333333333") >= bigint("2222222222222222222"));
    assertTrue(bigint("-100") < bigint("-2"));
    assertTrue(bigint("-2") > bigint("-100"));
    assertTrue(bigint("-100") <= bigint("-100"));
    assertTrue(bigint("-2") >= bigint("-100"));
    assertTrue(bigint("-1") < bigint("0"));
    assertTrue(bigint("0") > bigint("-1"));
  }
};

class bit_size_test : public Test {
public:
  void run() {
    assertTrue(bigint("0").sizeInBits() == 0);
    assertTrue(bigint("1").sizeInBits() == 1);
    assertTrue(bigint("1000", 2).sizeInBits() == 4);
    assertTrue(bigint("ffffffffffffffffffff", 16).sizeInBits() == 80);
  }
};

int main(int, char **) {
  TestSuite suite;
  suite.add("Construction", new construction_test());
  suite.add("Radix", new radix_test());
  suite.add("Sign", new sign_test());
  suite.add("Arithmetic", new arithmetic_test());
  suite.add("Power", new power_test());
  suite.add("Division", new division_test());
  suite.add("Square root", new sqrt_test());
  suite.add("Comparison", new comparison_test());
  suite.add("Bit size", new bit_size_test());

  suite.run();
  suite.printStatus();
  return suite.hasFailures() ? EXIT_FAILURE : EXIT_SUCCESS;
}
