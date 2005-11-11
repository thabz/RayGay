
#ifndef RAYGAY_TYPES_H
#define RAYGAY_TYPES_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
    #include <sys/types.h>
#else
    #define uint unsigned int
    #define ushort unsigned short
    #define uchar unsigned char
    #define ulong unsigned long
#endif

#endif
