
%{

#include <string>    
#include "math/vector.h"
#include "image/rgba.h"
#include "image/texture.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "objects/sphere.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "materials/material.h"    
#include "parser.h"
using namespace std;

%}
alpha	[a-zA-Z]
digit	[0-9]
special	[\.\_-]
string	{alpha}({alpha}|{digit}|{special})*
%%
[ \t\n]		;
aa		return tAA;
background	return tBACKGROUND;
bicubic 	return tBICUBIC;
bilinear 	return tBILINEAR;
camera		return tCAMERA;
circle		return tCIRCLE;
cylinder	return tCYLINDER;
difference 	return tDIFFERENCE;
dof		return tDOF;
extrusion	return tEXTRUSION;
fov		return tFOV;
intersection	return tINTERSECTION;
lookat		return tLOOKAT;
linesegment	return tLINESEGMENT;
material	return tMATERIAL;
name		return tNAME;
none		return tNONE;
necklace	return tNECKLACE;
photontracer	return tPHOTONTRACER;
print		return tPRINT;
position	return tPOSITION;
raytracer	return tRAYTRACER;
renderer	return tRENDERER;
rotate		return tROTATE;
solidbox	return tSOLIDBOX;
sphere		return tSPHERE;
spiral		return tSPIRAL;
texture		return tTEXTURE;
translate	return tTRANSLATE;
torus		return tTORUS;
union		return tUNION;
up		return tUP;

"\#.*\n"		/* Eat up comments */
{digit}+ |
{digit}+"."{digit}+ { yylval.d = atof(yytext); return tFLOAT;}
{string}	{ yylval.c = new string(yytext); return tSTRING; }
.		return yytext[0];

%%
int yywrap() {
    return 1;
}

