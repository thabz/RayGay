
%{

#include <string>    
#include "math/vector.h"
#include "lights/lightsource.h"
#include "image/rgba.h"
#include "image/texture.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "objects/objectgroup.h"    
#include "objects/sphere.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "materials/material.h"    
#include "parser.h"
using namespace std;

int line_num = 1;
%}

alpha	[a-zA-Z]
digit	[0-9]
special	[\.\_-]
string	{alpha}({alpha}|{digit}|{special})*
%x comment

%%
[ \t]		;
"/*"		BEGIN(comment);
<comment>[^*\n]*        /* eat anything that's not a '*' */
<comment>"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
<comment>\n             line_num++;
<comment>"*"+"/"        BEGIN(INITIAL);
\n		line_num++;
aa		return tAA;
abs		return tABS;
area		return tAREA;
aspect		return tASPECT;
background	return tBACKGROUND;
bicubic 	return tBICUBIC;
bilinear 	return tBILINEAR;
box		return tBOX;
bump		return tBUMP;
cachetolerance	return tCACHETOLERANCE; 
camera		return tCAMERA;
causticphotons 	return tCAUSTICPHOTONS; 
circle		return tCIRCLE;
cos		return tCOS;
cylinder	return tCYLINDER;
difference 	return tDIFFERENCE;
diffuse		return tDIFFUSE;
dof		return tDOF;
estimateradius 	return tESTIMATERADIUS; 
estimatesamples return tESTIMATESAMPLES;
eta		return tETA;
extrusion	return tEXTRUSION;
finalgatherrays return tFINALGATHERRAYS;
fov		return tFOV;
globalphotons  	return tGLOBALPHOTONS;
gloss		return tGLOSS;
group		return tGROUP;
height		return tHEIGHT;
intersection	return tINTERSECTION;
image		return tIMAGE;
lookat		return tLOOKAT;
light		return tLIGHT;
linesegment	return tLINESEGMENT;
kd		return tKD;
ks		return tKS;
kt		return tKT;
material	return tMATERIAL;
name		return tNAME;
none		return tNONE;
noshadow	return tNOSHADOW;
num		return tNUM;
necklace	return tNECKLACE;
object		return tOBJECT;
photonmap	return tPHOTONMAP;
photonrenderer	return tPHOTONRENDERER;
PI		return tPI;
power		return tPOWER;
print		return tPRINT;
point		return tPOINT;
position	return tPOSITION;
radius		return tRADIUS;
raytracer	return tRAYTRACER;
renderer	return tRENDERER;
rotate		return tROTATE;
sin		return tSIN;
sky		return tSKY;
solidbox	return tSOLIDBOX;
specpow		return tSPECPOW;
specular	return tSPECULAR;
sphere		return tSPHERE;
spot		return tSPOT;
spiral		return tSPIRAL;
texture		return tTEXTURE;
translate	return tTRANSLATE;
torus		return tTORUS;
union		return tUNION;
up		return tUP;
width		return tWIDTH;
wireframe	return tWIREFRAME;

{digit}+ |
{digit}+"."{digit}+ { yylval.d = atof(yytext); return tFLOAT;}
{string}	{ yylval.c = new string(yytext); return tSTRING; }
.		return yytext[0];

%%
int yywrap() {
    return 1;
}

