
%{

#include <string>    
#include "math/vector.h"
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
circle		return tCIRCLE;
cylinder	return tCYLINDER;
difference 	return tDIFFERENCE;
intersection	return tINTERSECTION;
linesegment	return tLINESEGMENT;
material	return tMATERIAL;
name		return tNAME;
necklace	return tNECKLACE;
print		return tPRINT;
rotate		return tROTATE;
solidbox	return tSOLIDBOX;
sphere		return tSPHERE;
spiral		return tSPIRAL;
translate	return tTRANSLATE;
torus		return tTORUS;
union		return tUNION;
"\#.*\n"		/* Eat up comments */
{digit}+ |
{digit}+"."{digit}+ { yylval.d = atof(yytext); return tFLOAT;}
{string}	{ yylval.c = new string(yytext); return tSTRING; }
.		return yytext[0];

%%
int yywrap() {
    return 1;
}

