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

#include "math.h"

#define M_PI		3.14159265358979323846
#define M_PI_DEG	(M_PI/180.0)
#define M_DEG_PI	(180.0/M_PI)
#define M_2PI           (2.0*M_PI)

#define HUGE_DOUBLE	double(1e+20)

#define min(x,y)		((x) < (y) ? (x) : (y))
#define max(x,y)		((x) > (y) ? (x) : (y))

#define EPSILON		1e-5
#define IS_ZERO(p)	(abs(p) < EPSILON)
#define IS_POSITIVE(p)	(p > -EPSILON)
#define IS_NEGATIVE(p)	(p < EPSILON)
#define IS_EQUAL(x,y)	(abs((x)-(y)) < EPSILON)

#define DEG2RAD(x)	((x/360.0)*M_2PI)

#endif
