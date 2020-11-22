
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <cassert>
#include <iostream>
#include <list>

#include "math/functions.h"
#include "math/qmcsequence.h"
#include "math/vector.h"
#include "math/vector2.h"

#ifndef HAVE_LONG_DOUBLE
#define sqrtl sqrt
#define cbrtl cbrt
#endif

/*
#!/usr/bin/perl -w

# Create binomial table

use strict;

my $table_size = 17;
my $num = $table_size - 1;

foreach my $n (0..$num) {
    print "{";
    foreach my $k (0..$num) {
        printf "%1d",bino($n,$k);
        print "," if ($k != $num);
    }
    print "}";
    print "," if ($n != $num);
    print "\n";
}

sub bino {
    my ($n,$k) = @_;
    if ($k == 0 || $k == $n) {
        return 1;
    } elsif ($k < 0 || $k > $n) {
        return 0;
    } else {
        return bino($n-1,$k-1) + bino($n-1,$k);
    }
}
*/
static uint32_t binomialTable[17][17] = {
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 4, 6, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 5, 10, 10, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 6, 15, 20, 15, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 7, 21, 35, 35, 21, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 8, 28, 56, 70, 56, 28, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0},
    {1, 9, 36, 84, 126, 126, 84, 36, 9, 1, 0, 0, 0, 0, 0, 0, 0},
    {1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1, 0, 0, 0, 0, 0, 0},
    {1, 11, 55, 165, 330, 462, 462, 330, 165, 55, 11, 1, 0, 0, 0, 0, 0},
    {1, 12, 66, 220, 495, 792, 924, 792, 495, 220, 66, 12, 1, 0, 0, 0, 0},
    {1, 13, 78, 286, 715, 1287, 1716, 1716, 1287, 715, 286, 78, 13, 1, 0, 0, 0},
    {1, 14, 91, 364, 1001, 2002, 3003, 3432, 3003, 2002, 1001, 364, 91, 14, 1,
     0, 0},
    {1, 15, 105, 455, 1365, 3003, 5005, 6435, 6435, 5005, 3003, 1365, 455, 105,
     15, 1, 0},
    {1, 16, 120, 560, 1820, 4368, 8008, 11440, 12870, 11440, 8008, 4368, 1820,
     560, 120, 16, 1}};

/**
 * The binomial coefficient is defined as
 * \f[ {n \choose k} = \frac{n!}{k!(n-k)!} \quad \mbox{if } n\geq k\geq 0 \qquad
 * \f] \f[ {n \choose k} = 0 \quad \mbox{if } k<0 \mbox{ or } k>n. \f]
 *
 * Using table lookup for n,k < 17.
 *
 * Using the fact that choose(n,k) = n/k * choose(n-1,k-1) for the non-recursive
 * algorithm.
 *
 * Also using that choose(n,k) = choose(n,n-k) which is useful when k > n/2.
 * Calculating choose(50,2) is the same as choose(50,48) but much faster to
 * calculate.
 */
long Math::binomialCoefficient(long n, long k) {
  if (k == 0 || k == n) {
    return 1;
  } else if (k < 0 || k > n) {
    return 0;
  } else if (k < 17 && n < 17) {
    return binomialTable[n][k];
  } else {
    long d, r = 1;
    if (k > n / 2) {
      k = n - k;
    }
    for (d = 1; d <= k; d++) {
      r *= n--;
      r /= d;
    }
    return r;
  }
}

/**
 * The Bernstein polynomial is defined as
 *
 * \f[ B_{i,n}(t) = {n \choose i} t^i (1-t)^{n-i} \quad \mbox{for } t \in
 * {[0,1]} \qquad \f]
 *
 */
double Math::bernsteinPolynomial(uint32_t i, uint32_t n, double t) {
  return binomialCoefficient(n, i) * pow(t, i) * pow((1 - t), n - i);
}

/**
 * Solves the quartic equation
 *
 * \f[ x^4 + Ax^3 + Bx^2 + Cx + D = 0 \f]
 *
 * The returned roots are sorted and duplicates removed.
 *
 * @param A, B, C, D real coefficients of the equation above.
 * @param roots an array of four doubles where the roots are stored
 * @return the number of real roots
 *
 * @see http://www.magic-software.com/ for method used.
 */
int Math::solveQuartic(double A, double B, double C, double D, double *roots) {
  double a = -B;
  double b = A * C - 4 * D;
  double c = 4 * B * D - C * C - A * A * D;
  double cubic_roots[3];
  double tmp1, tmp2;
  solveCubicSingle(a, b, c, cubic_roots);
  double y = cubic_roots[0];
  double R = 0.25 * A * A - B + y;

  if (R < 0.0)
    return 0;

  double D2, E2;
  int num = 0;
  if (IS_ZERO(R)) {
    R = 0.0; // Fixes precision error
    tmp1 = 0.75 * A * A - 2.0 * B;
    tmp2 = 2.0 * sqrt(y * y - 4.0 * D);
    D2 = tmp1 + tmp2;
    E2 = tmp1 - tmp2;
  } else {
    R = sqrt(R);
    tmp1 = 0.75 * A * A - R * R - 2.0 * B;
    tmp2 = (A * B - 2.0 * C - 0.25 * A * A * A) / R;
    D2 = tmp1 + tmp2;
    E2 = tmp1 - tmp2;
  }

  if (D2 >= 0.0) {
    D2 = sqrt(D2);
    roots[num++] = 0.5 * (A * -0.5 + R - D2);
    roots[num++] = 0.5 * (A * -0.5 + R + D2);
  }
  if (E2 >= 0.0) {
    E2 = sqrt(E2);
    roots[num++] = 0.5 * (A * -0.5 - R - E2);
    roots[num++] = 0.5 * (A * -0.5 - R + E2);
  }

  // There are either zero, two or four roots.
  // If zero, we are done now.
  // If two, they are already sorted as per construction above and just needs
  // pruning.
  // If four, they needs sorting and then pruning.
  if (num < 2) {
    return num;
  }
  if (num > 2) {
    // TODO: A specialised sort will be faster. We know that num is 4.
    std::sort(roots, roots + num);
  }

  // Prune the sorted array where we remove duplets.
  int i = 0;
  for (int j = 0; j < num; j++) {
    double root = roots[j];
    if (i == 0) {
      roots[i++] = root;
    } else {
      if (!IS_EQUAL(roots[i - 1], root)) {
        roots[i++] = root;
      }
    }
  }
  return i;
}

/**
 * Solves the quartic equation
 *
 * \f[ x^4 + Ax^3 + Bx^2 + Cx + D = 0 \f]
 *
 * The smallest root greater than cut-off value is returned.
 *
 * @param A, B, C, D real coefficients of the equation above.
 * @param root where to write the roots
 * @param cut the cut value.
 * @return zero if no roots are found, non-zero otherwise.
 *
 * @see http://www.magic-software.com/ for method used.
 */
int Math::solveQuarticSingle(double A, double B, double C, double D, double cut,
                             double *root) {
  double roots[4];
  double a = -B;
  double b = A * C - 4 * D;
  double c = 4 * B * D - C * C - A * A * D;
  double cubic_roots[3];
  double tmp1, tmp2;
  solveCubicSingle(a, b, c, cubic_roots);
  double y = cubic_roots[0];
  double R = 0.25 * A * A - B + y;

  if (R < 0.0)
    return 0;

  double D2, E2;
  int num = 0;
  if (IS_ZERO(R)) {
    R = 0.0; // Fixes precision error
    tmp1 = 0.75 * A * A - 2.0 * B;
    tmp2 = 2.0 * sqrt(y * y - 4.0 * D);
    D2 = tmp1 + tmp2;
    E2 = tmp1 - tmp2;
  } else {
    R = sqrt(R);
    tmp1 = 0.75 * A * A - R * R - 2.0 * B;
    tmp2 = (A * B - 2.0 * C - 0.25 * A * A * A) / R;
    D2 = tmp1 + tmp2;
    E2 = tmp1 - tmp2;
  }

  if (D2 >= 0.0) {
    D2 = sqrt(D2);
    roots[num++] = 0.5 * (A * -0.5 + R - D2);
    roots[num++] = 0.5 * (A * -0.5 + R + D2);
  }
  if (E2 >= 0.0) {
    E2 = sqrt(E2);
    roots[num++] = 0.5 * (A * -0.5 - R - E2);
    roots[num++] = 0.5 * (A * -0.5 - R + E2);
  }

  // There are either zero, two or four roots.
  // If zero, we are done now.
  // If two, they are already sorted as per construction above and just needs
  // pruning.
  // If four, they needs sorting and then pruning.

  double result = HUGE_DOUBLE;
  int found_one = 0;
  for (int j = 0; j < num; j++) {
    if (roots[j] < result && roots[j] > cut) {
      result = roots[j];
      found_one = 1;
    }
  }
  *root = result;
  return found_one;
}

/**
 * Solves the quartic equation
 *
 * \f[ x^4 + Ax^3 + Bx^2 + Cx + D = 0 \f]
 *
 * This method is invalid!
 *
 * @param A, B, C, D real coefficients of the equation above.
 * @param roots an array of four doubles where the roots are stored
 * @return the number of real roots
 * @see http://mathworld.wolfram.com/QuarticEquation.html equations (32) and
 * (33) for method used. Which is the same as Schaum, p.33.
 */
int Math::solveQuartic_Schaum(double A, double B, double C, double D,
                              double *roots) {
  double a = -B;
  double b = A * C - 4 * D;
  double c = 4 * B * D - C * C - A * A * D;
  double cubic_roots[3];
  std::cout << "a b c = " << a << ", " << b << ", " << c << std::endl;

  int n = solveCubic(a, b, c, cubic_roots);
  std::cout << "n = " << n << std::endl;
  double y = cubic_roots[0];
  std::cout << "y = " << y << std::endl;

  std::cout << " sqrt1^2 = " << A * A - 4 * B + 4 * y << std::endl;
  double sqrt1 = sqrt(A * A - 4 * B + 4 * y);
  std::cout << " sqrt1 = " << sqrt1 << std::endl;
  std::cout << " sqrt2^2 = " << y * y - 4 * D << std::endl;
  double sqrt2 = sqrt(y * y - 4 * D);
  std::cout << " sqrt2 = " << sqrt2 << std::endl;

  int num1 = solveQuadratic(1, (A + sqrt1) / 2.0, (y - sqrt2) / 2.0, roots);
  std::cout << "num1 = " << num1 << std::endl;
  std::cout << "root1 = " << roots[0] << std::endl;
  std::cout << "root2 = " << roots[1] << std::endl;
  int num2 =
      solveQuadratic(1, (A - sqrt1) / 2.0, (y + sqrt2) / 2.0, &(roots[num1]));
  std::cout << "num2 = " << num2 << std::endl;
  std::cout << "root1 = " << roots[2] << std::endl;
  std::cout << "root2 = " << roots[3] << std::endl;

  // prune duplicate roots
  if (num1 == 1 && num2 == 1) {
    if (IS_EQUAL(roots[0], roots[1])) {
      num2 = 0;
    }
  } else if (num1 == 1 && num2 == 2) {
    if (IS_EQUAL(roots[0], roots[1] || IS_EQUAL(roots[0], roots[2]))) {
      roots[0] = roots[2];
      num1 = 0;
    }
  } else if (num1 == 2 && num2 == 1) {
    if (IS_EQUAL(roots[0], roots[2]) || IS_EQUAL(roots[1], roots[2])) {
      num2 = 0;
    }
  } else if (num1 == 2 && num2 == 2) {
    if (IS_EQUAL(roots[0], roots[3]) || IS_EQUAL(roots[1], roots[3])) {
      num2--;
    }
    if (IS_EQUAL(roots[0], roots[2]) || IS_EQUAL(roots[1], roots[2])) {
      if (num2 == 1) {
        num2--;
      } else {
        roots[2] = roots[3];
        num2--;
      }
    }
  }

  return num1 + num2;
}

/**
 * Solves the cubic equation
 *
 * \f[ x^3 + Ax^2 + Bx + C = 0 \f]
 *
 * Every cubic equation has at least one real root.
 *
 *
 * @param A, B, C real coefficients of the equation above.
 * @param roots an array of three doubles where the roots are stored
 * @return the number of real roots
 * @see http://mathworld.wolfram.com/CubicEquation.html
 */
int Math::solveCubic(double A, double B, double C, double *roots) {
  double Q = (3.0 * B - A * A) * FRAC_1_9;
  double R = (9.0 * A * B - 27.0 * C - 2.0 * A * A * A) * FRAC_1_54;
  double D = Q * Q * Q + R * R;
  if (IS_ZERO(D)) {
    // D == 0 gives two (maybe equal) real roots.
    if (IS_ZERO(R)) {
      roots[0] = -A / 3.0;
      return 1;
    } else {
      double S = cbrt(R);
      roots[0] = 2 * S - A / 3.0;
      ;
      roots[1] = -S - A / 3.0;
      return 2;
    }
  } else if (D < 0) {
    // D < 0 gives three real roots
    double phi = acos(R / sqrt(-(Q * Q * Q)));
    double G = 2.0 * sqrt(-Q);
    double H = A / 3.0;
    roots[0] = G * cos(phi / 3.0) - H;
    roots[1] = G * cos(phi / 3.0 + M_2PI / 3.0) - H;
    roots[2] = G * cos(phi / 3.0 + 2 * M_2PI / 3.0) - H;
    return 3;
  } else {
    // D > 0 gives only one real root
    double sqrtD, S, T;
    if (IS_ZERO(Q)) {
      S = cbrt(2 * R);
      T = 0.0;
    } else {
      sqrtD = sqrt(D);
      S = cbrt(R + sqrtD);
      T = cbrt(R - sqrtD);
    }
    roots[0] = S + T - A / 3.0;
    return 1;
  }
}

/**
 * Solves the cubic equation and return smallest root
 *
 * \f[ x^3 + Ax^2 + Bx + C = 0 \f]
 *
 * Every cubic equation has at least one real root.
 *
 * @param A, B, C real coefficients of the equation above.
 * @param roots an array of three doubles where the roots are stored
 * @return the number of real roots
 * @see http://mathworld.wolfram.com/CubicEquation.html
 */
double Math::solveCubicSingle(double A, double B, double C, double *roots) {
  double Q = (3.0 * B - A * A) * FRAC_1_9;
  double R = (9.0 * A * B - 27.0 * C - 2.0 * A * A * A) * FRAC_1_54;
  double D = Q * Q * Q + R * R;
  if (IS_ZERO(D)) {
    // D == 0 gives two (maybe equal) real roots.
    if (IS_ZERO(R)) {
      roots[0] = -A / 3.0;
    } else {
      double S = cbrt(R);
      roots[0] = 2 * S - A / 3.0;
      ;
    }
  } else if (D < 0) {
    // D < 0 gives three real roots
    double phi = acos(R / sqrt(-(Q * Q * Q)));
    double G = 2.0 * sqrt(-Q);
    double H = A / 3.0;
    roots[0] = G * cos(phi / 3.0) - H;
  } else {
    // D > 0 gives only one real root
    double sqrtD, S, T;
    if (IS_ZERO(Q)) {
      S = cbrt(2 * R);
      T = 0.0;
    } else {
      sqrtD = sqrt(D);
      S = cbrt(R + sqrtD);
      T = cbrt(R - sqrtD);
    }
    roots[0] = S + T - A / 3.0;
  }
  return roots[0];
}

/**
 * Solves the quadratic equation
 *
 * \f[ Ax^2 + Bx + C = 0 \f]
 *
 * Where A != 0 or B != 0.
 *
 * @param A, B, C real coefficients of the equation above.
 * @param roots an array of two doubles where the roots are stored
 * @return the number of real roots
 */
int Math::solveQuadratic(double A, double B, double C, double *roots) {
  if (IS_ZERO(A) && IS_NZERO(B)) {
    roots[0] = -C / B;
    return 1;
  }
  assert(IS_NZERO(A));
  double D = B * B - 4 * A * C;
  if (D < 0) {
    return 0;
  } else if (IS_ZERO(D)) {
    roots[0] = -B / (2.0 * A);
    return 1;
  } else {
    double sqrtD = sqrt(D);
    roots[0] = (-B + sqrtD) / (2.0 * A);
    roots[1] = (-B - sqrtD) / (2.0 * A);
    return 2;
  }
}

/**
 * Pertubes a vector around another vector, that is finds a random
 * vection within a cone.
 *
 * TODO: http://www.acm.org/tog/resources/RTNews/html/rtnv20n1.html#art11 maybe
 * faster method...
 *
 * @param axis normalized axis of the cone
 * @param angle angle of the cone in radians
 */
Vector Math::perturbVector(const Vector &axis, const double angle) {
  Vector result;
  Vector axisP = axis.toPolar();
  assert(IS_EQUAL(axisP[0], 1)); // Check that axis was a unit-vector
  do {
    result = axisP;
    result[1] += RANDOM(-angle, angle);
    result[2] += RANDOM(-angle, angle);
    result = result.toRectangular();
  } while (acos(result * axis) > angle);

  return result;
}

/**
 * Pertubes a vector around another vector, that is finds a random
 * vection within a cone.
 *
 * @param axis normalized axis of the cone
 * @param angle angle of the cone in radians
 * @param qmc_sequence a 2-dimensional quasi-Monto-Carlo sequence to use for
 * random numbers
 */
Vector Math::perturbVector(const Vector &axis, const double angle,
                           QMCSequence *qmc_sequence) {
  Vector result;
  Vector axisP = axis.toPolar();
  assert(IS_EQUAL(axisP[0], 1)); // Check that axis was a unit-vector
  do {
    result = axisP;
    double *rnds = qmc_sequence->getNext();
    result[1] += (2 * rnds[0] - 1) * angle;
    result[2] += (2 * rnds[1] - 1) * angle;
    result = result.toRectangular();
  } while (acos(result * axis) > angle);

  return result;
}
// FIXME: This works better than the above but distribution is not uniform.
/*
Vector Math::perturbVector(const Vector& axis, const double angle, QMCSequence*
qmc_sequence) { Vector result; double* rnds; do { rnds =
qmc_sequence->getNext(); result = axis.randomHemisphere(rnds[0],rnds[1]);; }
while (acos(result*axis) > angle);

    return result;
}
*/

/**
 * Low distortion maps of a point on [0,1]^2 to the unit disc.
 *
 * @see http://www.acm.org/jgt/papers/ShirleyChiu97/
 */
Vector2 Math::shirleyDisc(double seedx, double seedy) {
  double phi, r;

  double a = 2 * seedx - 1; /* (a,b) is now on [-1,1]^2 */
  double b = 2 * seedy - 1;

  if (a > -b) {  /* region 1 or 2 */
    if (a > b) { /* region 1, also |a| > |b| */
      r = a;
      phi = (M_PI / 4) * (b / a);
    } else { /* region 2, also |b| > |a| */
      r = b;
      phi = (M_PI / 4) * (2 - (a / b));
    }
  } else {       /* region 3 or 4 */
    if (a < b) { /* region 3, also |a| >= |b|, a != 0 */
      r = -a;
      phi = (M_PI / 4) * (4 + (b / a));
    } else { /* region 4, |b| >= |a|, but a==0 and b==0 could occur. */
      r = -b;
      if (b != 0)
        phi = (M_PI / 4) * (6 - (a / b));
      else
        phi = 0;
    }
  }
  return Vector2(r * cos(phi), r * sin(phi));
}

/**
 * If points is a collection of uniformly-distributed points in [0,1]x[0,1]
 * this method will return a collection of points on the unit-sphere S^2.
 * It depends on the slightly counterintuitive fact (see
 * proof below) that each of the three coordinates of a uniformly
 * distributed point on S^2 is uniformly distributed on [-1,1] (but
 * the three are not independent, obviously).  Therefore, it
 * suffices to choose one axis (Z, say) and generate a uniformly
 * distributed value on that axis.  This constrains the chosen point
 * to lie on a circle parallel to the X-Y plane, and the obvious
 * trig method may be used to obtain the remaining coordinates.
 *
 * See http://www.math.niu.edu/~rusin/known-math/96/sph.rand
 * TODO: Optimize by not throwing std::vectors around
 * TODO: Test!
 */
std::vector<Vector> Math::toUnitSphere(const std::vector<Vector2> &points) {
  std::vector<Vector> result = std::vector<Vector>(points.size());
  result.reserve(points.size());
  for (uint32_t i = 0; i < points.size(); i++) {
    Vector2 p = points[i];
    double z = p[0] * 2 - 1;
    double t = p[1] * M_2PI;
    double r = sqrt(1 - z * z);
    Vector v = Vector(r * cos(t), r * sin(t), z);
    result.push_back(v);
  }
  return result;
}

/*
Notes to the above

If h is the cosine of the half-angle of the cone, then changing the z line
above to

double z = h + (1-h)*p[0]

will produce unit vectors constrained to be as close to the +Z axis as desired,
depending on h. Then a simple change of basis will re-orient them as needed. The
nice thing about this approach is that the cost is constant regardless of how
tight the cone is.
*/
