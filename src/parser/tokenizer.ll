
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

%}
alpha	[a-zA-Z]
digit	[0-9]
special	[\.\_-]
string	{alpha}({alpha}|{digit}|{special})*
%%
[ \t\n]		;
material	return tMATERIAL;
solidbox	return tSOLIDBOX;
sphere		return tSPHERE;
{digit}+ |
{digit}+"."{digit}+ { yylval.d = atof(yytext); return tFLOAT;}
{string}	{ return tSTRING; }

%%


