
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
            assertTrue(rlong(1,2) < rlong(3,4));        
            assertTrue(rlong(1,2) < 1L);        
            assertTrue(0L < rlong(1,2));        

            assertTrue(rlong(3,4) > rlong(2,3));        
            assertTrue(1L > rlong(9,10));        
            assertTrue(rlong(1,2) > 0L);        

            assertTrue(rlong(3,4) >= rlong(2,3));        
            assertTrue(rlong(3,4) >= rlong(6,8));        
            assertTrue(1L >= rlong(9,9));        
            assertTrue(1L >= rlong(9,10));        
            assertTrue(rlong(1,2) >= 0L);        
            assertTrue(rlong(2,2) >= 1L);        

            assertTrue(rlong(1,2) <= rlong(3,4));        
            assertTrue(rlong(1,2) <= rlong(2,4));        
            assertTrue(rlong(1,2) <= 1L);        
            assertTrue(rlong(2,2) <= 1L);        
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
            
            rlong x = pow(rlong(4,7), -3L);
            assertTrue(x == rlong(343,64));
            
            x = pow(rlong(1,3), long(-3));
            assertTrue(x == 27L);
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
        }
};
int main(int argc, char *argv[]) {
    TestSuite suite;
    suite.add("Arithmetic", new test_operators());
    suite.add("Comparators", new test_comparators());
    suite.add("Trancedentals", new test_trancedentals());
    suite.add("Rounding", new test_rounding());
    suite.run();
    suite.printStatus();

    return suite.hasFailures() ? EXIT_FAILURE : EXIT_SUCCESS;
}
