/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Copyright 2003 by Jesper Christensen <jesper@kalliope.org>
 *
 * Added Fri Apr 18 2003
 */

#ifndef CONSTANTS_H
#define CONSTANTS_H

#include <cmath>
#include <stdint.h>
#include <cstdlib>

#define Float double

#ifndef M_PI
#define M_PI            3.1415926535897932384626433832795029
#endif
		        
#define M_2PI           6.283185307179586476925286766552  /* 2 * M_PI */
#define M_PI_DEG	0.017453292519943295769236907684  /* M_PI/180.0 */
#define M_DEG_PI	35.342917352885173932704738061855 /* 180.0/M_PI */

#ifndef M_SQRT3
#define M_SQRT3		1.73205080756887729352744634151   /* sqrt(3.0) */
#endif

/* Some useful fractions */
#define FRAC_1_27 	0.03703703703703703703703703703703703703703703703
#define FRAC_1_6 	0.16666666666666666666666666666666666666666666666
#define FRAC_1_54 	0.01851851851851851851851851851851851851851851851
#define FRAC_1_3	0.33333333333333333333333333333333333333333333333
#define FRAC_1_9	0.11111111111111111111111111111111111111111111111
#define FRAC_2_3	0.66666666666666666666666666666666666666666666666

#define HUGE_DOUBLE	double(1e+20)
#define EPSILON		double(1e-9)

#define SIGN(x) 	((x >= 0) ? 1 : -1)
#define SAME_SIGN(x,y) 	(SIGN(y) == SIGN(x)) 

#ifndef MIN
#define MIN(x,y)	((x) < (y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y)	((x) > (y) ? (x) : (y))
#endif

#define IS_POSITIVE(p)	((p) > -EPSILON)
#define IS_NEGATIVE(p)	((p) < EPSILON)

#define IS_ZERO(p)	(fabs(p) < EPSILON)
#define IS_NZERO(p)	(fabs(p) >= EPSILON)
#define IS_EQUAL(x,y)	(IS_ZERO((x)-(y)))
#define IS_NEQUAL(x,y)	(IS_NZERO((x)-(y)))

#define IS_LESS_THAN(a,b) 	IS_POSITIVE(((b) - (a)))
#define IS_GREATER_THAN(a,b) 	IS_POSITIVE(((a) - (b)))

// Convert x from degrees [0,360] to radians [0,2*PI]
#define DEG2RAD(x)	((x)*M_PI_DEG)

// A random double in [a,b]
#define RANDOM(a,b) 	(((b)-(a))*(double(rand())/RAND_MAX) + (a))
#endif
