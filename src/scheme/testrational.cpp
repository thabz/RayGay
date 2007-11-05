
#include "testing.h"
#include "rational.h"

typedef rational<long> rlong;

class test_operators : public Test {
    public:
	void run() {
             rlong z = 1;
             assertTrue(z == rlong(1));
             assertTrue(z + long(2) == rlong(3));
             
             rlong x = rlong(4,5);
             assertTrue(x.numerator() == 4);
             assertTrue(x.denominator() == 5);
             x *= 2;
             assertTrue(x == rlong(8,5));
             x += 1;
             assertTrue(x == rlong(13,5));
             x -= -2;
             assertTrue(x == rlong(23,5));
             x -= rlong(1,5);
             assertTrue(x == rlong(22,5));
             x /= 11;
             assertTrue(x == rlong(2,5));
        }
};

class test_comparators : public Test {
    public:
	void run() {
            assertTrue(rlong(1,2) == rlong(1,2));        
            assertTrue(rlong(1,2) == rlong(6,12));        
            assertTrue(rlong(1,2) == rlong(-6,-12));        
            assertTrue(rlong(3,3) == 1L);        
            assertTrue(rlong(12,3) == 4L);
            assertTrue(4L == rlong(12,3));
            assertTrue(1L == rlong(3,3));

            assertTrue(rlong(1,2) != rlong(3,2));        
            assertTrue(rlong(1,2) != rlong(7,12));        
            assertTrue(rlong(3,4) != 1L);        
            assertTrue(1L != rlong(3,4));

            assertTrue(rlong(1,2) < rlong(3,4));        
            assertTrue(rlong(-1,-2) < rlong(3,4));        
            assertTrue(rlong(1,-2) < rlong(-1,4));        
            assertTrue(rlong(1,-2) < rlong(1,-4));        
            assertTrue(rlong(1,2) < 1L);        
            assertTrue(0L < rlong(1,2));        
            assertTrue(1L < rlong(-3,-2));        
            assertTrue(0L < rlong(-1,-2));        
            assertTrue(rlong(-1,2) < 0L);        
            assertTrue(rlong(1,-2) < 0L);        

            assertTrue(rlong(3,4) > rlong(2,3));        
            assertTrue(1L > rlong(9,10));        
            assertTrue(rlong(1,2) > 0L);        
            assertTrue(rlong(-1,-2) > 0L);        
            assertTrue(0L > rlong(-1,2));        
            assertTrue(0L > rlong(1,-2));

            assertTrue(rlong(3,4) >= rlong(2,3));        
            assertTrue(rlong(3,4) >= rlong(6,8));        
            assertTrue(1L >= rlong(9,9));        
            assertTrue(1L >= rlong(9,10));        
            assertTrue(1L >= rlong(-9,-10));        
            assertTrue(0L >= rlong(-9,10));        
            assertTrue(0L >= rlong(9,-10));        
            assertTrue(rlong(1,2) >= 0L);        
            assertTrue(rlong(2,2) >= 1L);        
            assertTrue(rlong(-9,-10) >= -1L);        

            assertTrue(rlong(1,2) <= rlong(3,4));        
            assertTrue(rlong(1,2) <= rlong(2,4));        
            assertTrue(rlong(1,2) <= 1L);        
            assertTrue(rlong(2,2) <= 1L);        
            assertTrue(rlong(2,-2) <= 1L);        
            assertTrue(rlong(2,-2) <= -1L);        
            assertTrue(1L <= rlong(3,2));        
            assertTrue(2L <= rlong(6,3));        
        }
};

class test_trancedentals : public Test {
    public:
	void run() {
            rlong z = 0;
            assertTrue(sin(z) == 0.0);
            assertTrue(cos(z) == 1.0);
            assertTrue(tan(z) == 0.0);
            assertTrue(exp(z) == 1.0);
            
            assertTrue(pow(rlong(4,7), -3L) == rlong(343,64));
            assertTrue(pow(rlong(1,3), -3L) == 27L);
            assertTrue(pow(rlong(1,2), 2L) == rlong(1,4));
            assertTrue(pow(rlong(1,3), 3L) == rlong(1,27));
            assertTrue(pow(rlong(2,3), 4L) == rlong(16,81));

            assertTrue(sqrt(rlong(4,1)) == 2.0);
        }
};

class test_rounding : public Test {
    public:
	void run() {
            assertTrue(floor(rlong(1,2)) == 0);
            assertTrue(floor(rlong(-1,2)) == -1);
            assertTrue(floor(rlong(-17,3)) == -6);
            assertTrue(floor(rlong(17,3)) == 5);
            assertTrue(ceil(rlong(-17,3)) == -5);
            assertTrue(ceil(rlong(17,3)) == 6);
            assertTrue(ceil(rlong(-1,2)) == 0);
            assertTrue(trunc(rlong(-17,3)) == -5);
            assertTrue(trunc(rlong(17,3)) == 5);
            assertTrue(trunc(rlong(-1,2)) == 0);
            assertTrue(trunc(rlong(1,2)) == 0);
        }
};

class test_normalization : public Test {
    public:
	void run() {
            rlong z = rlong(2,3).normalized();
            assertTrue(z.numerator() == 2);
            assertTrue(z.denominator() == 3);
            z = rlong(4,2).normalized();
            assertTrue(z.numerator() == 2);
            assertTrue(z.denominator() == 1);
            z = rlong(9,18).normalized();
            assertTrue(z.numerator() == 1);
            assertTrue(z.denominator() == 2);
            z = rlong(9,-18).normalized();
            assertTrue(z.numerator() == -1);
            assertTrue(z.denominator() == 2);
            z = rlong(-9,-18).normalized();
            assertTrue(z.numerator() == 1);
            assertTrue(z.denominator() == 2);
        }
};

int main(int argc, char *argv[]) {
    TestSuite suite;
    suite.add("Arithmetic", new test_operators());
    suite.add("Comparators", new test_comparators());
    suite.add("Trancedentals", new test_trancedentals());
    suite.add("Rounding", new test_rounding());
    suite.add("Normalization", new test_normalization());
    suite.run();
    suite.printStatus();

    return suite.hasFailures() ? EXIT_FAILURE : EXIT_SUCCESS;
}
