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

#include <math.h>

//#define M_PI            3.141592653589793238462643383276
#define M_2PI           6.283185307179586476925286766552  /* 2 * M_PI */
#define M_PI_DEG	0.017453292519943295769236907684  /* M_PI/180.0 */
#define M_DEG_PI	35.342917352885173932704738061855 /* 180.0/M_PI */

#define HUGE_DOUBLE	double(1e+20)
#define EPSILON		double(1e-5)

#define MIN(x,y)	((x) < (y) ? (x) : (y))
#define MAX(x,y)	((x) > (y) ? (x) : (y))

#define IS_POSITIVE(p)	(p > -EPSILON)
#define IS_NEGATIVE(p)	(p < EPSILON)

#define IS_ZERO(p)	((p) < EPSILON && (p) > -EPSILON)
#define IS_EQUAL(x,y)	(IS_ZERO((x)-(y)))

#define DEG2RAD(x)	((x/360.0)*M_2PI)

#endif
