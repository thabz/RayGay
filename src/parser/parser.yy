
%{
    /* C declarations */
#include <string>    
#include <map>    
#include <cstdio>    
#include "math/vector.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "paths/linesegment.h"
#include "objects/sphere.h"    
#include "objects/csg.h"    
#include "objects/solid.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "objects/torus.h"    
#include "materials/material.h"    

using namespace std;

map<string,double> doubleMap;
map<string,Path*> pathMap;
map<string,Material*> materialMap;

void yyerror(string s);
extern int yylex(void);

Path* getNamedPath(string* name);
void setNamedPath(string* name, Path* path);
double getNamedDouble(string* name);
void setNamedDouble(string* name, double val);
Material* getNamedMaterial(string* name);
void setNamedMaterial(string* name, Material* material);

%}
    /* Bison declarations */
%union {
        double d;
	Vector* vector;
	RGB* rgb;
	Material* material;
	SceneObject* object;
	Path* path;
	string* c;
}
%token <d> tFLOAT
%token <c> tSTRING
%token tCIRCLE
%token tDIFFERENCE
%token tLINESEGMENT
%token tMATERIAL
%token tNAME
%token tNECKLACE
%token tPRINT
%token tSOLIDBOX
%token tSPHERE
%token tSPIRAL
%token tTORUS

%type <d> Expr 
%type <rgb> RGB
%type <vector> Vector
%type <object> Sphere SolidBox Necklace Difference SolidObject Torus
%type <material> MaterialDef NamedMaterial Material
%type <path> NamedPath Circle Spiral Path PathDef LineSegment

%left '+' '-'
%left '*' '/'
%%
    /* Grammar rules */
	
Items		: /* Empty */
                | Items Item
		;

Item		: Object
                | AssignName
		| Print
		;

AssignName	: tSTRING '=' PathDef
                {
                    setNamedPath($1, $3);
		}
                | tSTRING '=' Expr
                {
		    setNamedDouble($1, $3);
                }
                | tSTRING '=' MaterialDef 
                {
		    setNamedMaterial($1, $3);
                }
                ;
 
Material 	: NamedMaterial 
                | MaterialDef
		;

NamedMaterial   : tSTRING
                {
                   $$ = getNamedMaterial($1);
		}
                ;

MaterialDef     : tMATERIAL '{' RGB '}'
                {
		    $$ = new Material();
		    $$->setDiffuseColor(*$3);
		}
                | tMATERIAL '{' RGB RGB '}'
                {
		    $$ = new Material();
		    $$->setDiffuseColor(*$3);
		    $$->setSpecularColor(*$4);
		}
                ;

Object		: SolidObject
                | Necklace 
		;

SolidObject	: Sphere
                | SolidBox
                | Difference 
		| Torus
		;
		
SolidBox	: tSOLIDBOX NamedMaterial Vector Vector
                {
		    $$ = new SolidBox(*$3,*$4,$2);
		}
                ;

Necklace 	: tNECKLACE '{' NamedMaterial Path Expr Expr '}'
                {
		    $$ = new Necklace($4,int($5),$6,$3);
		}
                ;

Sphere		: tSPHERE '{' Material Expr Vector '}'
                {
		    $$ = new Sphere(*$5,$4,$3);
                }
                | tSPHERE '{' Expr Vector '}'
                {
		    $$ = new Sphere(*$4,$3,NULL);
		}
                ;

Torus		: tTORUS '{' Expr Expr '}' 
                {
		    $$ = new Torus($3,$4,NULL);
		}
                |  tTORUS '{' Material Expr Expr '}' 
                {
		    $$ = new Torus($4,$5,$3);
		}

Difference 	: tDIFFERENCE '{' NamedMaterial SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($4);
		    Solid* s2 = dynamic_cast<Solid*>($5);
		    $$ = new CSG(s1,CSG::DIFFERENCE,s2,$3);
		}
                | tDIFFERENCE '{' SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($3);
		    Solid* s2 = dynamic_cast<Solid*>($4);
		    $$ = new CSG(s1,CSG::DIFFERENCE,s2,NULL);
		}
                ;

Path		: NamedPath
                | PathDef
		;

NamedPath	: tSTRING
                {
		    $$ = getNamedPath($1);   
		}
                ;

PathDef		: Circle
                | Spiral
		| LineSegment
		;

LineSegment	: tLINESEGMENT '{' Vector Vector '}'
                {
		    $$ = new Linesegment(*$3,*$4);
		}
                ;

Circle		: tCIRCLE '{' Vector Expr Vector '}'
                {
		    $$ = new Circle(*$3,$4,*$5);
		}
                ;

Spiral		: tSPIRAL '{' Path Expr Expr '}'
                {
		    $$ = new Spiral($3,$4,$5);
		}
                | tSPIRAL '{' Path Expr Expr Expr '}'
                {
		    $$ = new Spiral($3,$4,$5,$6);
		}
		;

Vector		: '<' Expr ',' Expr ',' Expr '>' 
                { 
		    $$ = new Vector($2,$4,$6); 
		}
                ;

RGB		: '<' Expr ',' Expr ',' Expr '>' 
                { 
		    $$ = new RGB($2,$4,$6); 
		}
                ; 		

Print		: tPRINT Expr
                {
		    printf("%f\n",$2);
		}
                ;

Expr		: tFLOAT 
                {
                   $$ = $1;
                }
                | tSTRING
                {
		    $$ = getNamedDouble($1);
                }
		| '(' Expr ')' 
                {
                   $$ = $2;
		}
		| Expr '+' Expr 
                {
                    $$ = $1 + $3;
		}
		| Expr '-' Expr 
                {
                    $$ = $1 - $3;
		}
		| Expr '*' Expr 
                {
                    $$ = $1 * $3;
		}
		| Expr '/' Expr 
                {
                    $$ = $1 / $3;
		}
                ;
%%

    /* Additional C code */

void yyerror(string s) {
    cout << "Error: " << s << endl;
    exit(1);
}

Path* getNamedPath(string* name) {
    return pathMap[*name];
}

void setNamedPath(string* name, Path* path) {
    pathMap[*name] = path;
}

double getNamedDouble(string* name) {
    return doubleMap[*name];
}

void setNamedDouble(string* name, double val) {
    doubleMap[*name] = val;
}

Material* getNamedMaterial(string* name) {
    return materialMap[*name];
}

void setNamedMaterial(string* name, Material* material) {
    materialMap[*name] = material;
}


