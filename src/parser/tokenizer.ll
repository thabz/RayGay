
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
#include "parser/floatnodes.h"    
#include "parser/syntaxnode.h"    
#include "parser/vectornodes.h"    
#include "parser/rgbnodes.h"    
#include "parser/langnodes.h"    
#include "parser/transformationnodes.h"    
#include "parser/lightnodes.h"    
#include "parser/cameranode.h"    
#include "parser/boolnodes.h"    
#include "parser.h"
using namespace std;

int line_num = 1;
%}

alpha	[a-zA-Z]
digit	[0-9]
special	[\.\_-]
varspecial	[\_]
string	{alpha}({alpha}|{digit}|{special})*
varstring	{alpha}({alpha}|{digit}|_)*
qstring \"([^\n\"])*\"
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
catmullromspline return tCATMULLROMSPLINE;
causticphotons 	return tCAUSTICPHOTONS; 
circle		return tCIRCLE;
cos		return tCOS;
cylinder	return tCYLINDER;
difference 	return tDIFFERENCE;
diffuse		return tDIFFUSE;
do		return tDO;
dof		return tDOF;
ellipsoid	return tELLIPSOID;
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
if		return tIF;
image		return tIMAGE;
length		return tLENGTH;
light		return tLIGHT;
linesegment	return tLINESEGMENT;
lookat		return tLOOKAT;
kd		return tKD;
ks		return tKS;
kt		return tKT;
material	return tMATERIAL;
name		return tNAME;
none		return tNONE;
noshadow	return tNOSHADOW;
num		return tNUM;
necklace	return tNECKLACE;
normalize	return tNORMALIZE;
object		return tOBJECT;
photonmap	return tPHOTONMAP;
photonrenderer	return tPHOTONRENDERER;
PI		return tPI;
2PI		return t2PI;
power		return tPOWER;
print		return tPRINT;
point		return tPOINT;
position	return tPOSITION;
radius		return tRADIUS;
raytracer	return tRAYTRACER;
renderer	return tRENDERER;
repeat		return tREPEAT;
rotate		return tROTATE;
random		return tRANDOM;
scale		return tSCALE;
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
while		return tWHILE;
width		return tWIDTH;
wireframe	return tWIREFRAME;
"||"		return tBOOL_OR;
"&&"		return tBOOL_AND;
"++"		return tPLUSPLUS;
"--"		return tMINUSMINUS;
"!"		return tBOOL_NOT;
"=="		return tEQUALEQUAL;
"="		return tEQUAL;
"+="		return tPLUSEQUAL;
"-="		return tMINUSEQUAL;
"*="		return tMULTEQUAL;
"/="		return tDIVEQUAL;

{digit}+ |
{digit}+"."{digit}+ { yylval.d = atof(yytext); return tFLOAT;}
"$"{varstring}	{ yylval.c = new string(yytext+sizeof(char),yyleng-1); 
                  return tVARNAME;
                }
{qstring}	{ yylval.c = new string(yytext+sizeof(char),yyleng-2); return tQSTRING; }
{string}	{ yylval.c = new string(yytext); return tSTRING; }
.		return yytext[0];

%%
int yywrap() {
    return 1;
}

