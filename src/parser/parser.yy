
%{
    /* C declarations */
#include <string>    
#include "math/vector.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "objects/sphere.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "materials/material.h"    
using namespace std;

void yyerror(string s);
Path* getNamedPath(string* name);
void setNamedPath(string* name, Path* path);
double getNamedDouble(string* name);
void setNamedDouble(string* name, double path);
extern int yylex(void);

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
%token tMATERIAL
%token tNAME
%token tNECKLACE
%token tSPHERE
%token tSPIRAL
%token tSOLIDBOX

%type <d> Expr 
%type <rgb> RGB
%type <vector> Vector
%type <object> Sphere SolidBox Necklace
%type <material> MaterialDef NamedMaterial 
%type <path> NamedPath Circle Spiral Path PathDef

%left '+' '-'
%left '*' '/'
%%
    /* Grammar rules */
	
Items		: /* Empty */
                | Items Item
		;

Item		: Object
                | AssignName
		;

AssignName	: tNAME tSTRING PathDef
                {
                    setNamedPath($2, $3);
		}
                | tNAME tSTRING Expr
                {
		    setNamedDouble($2, $3);
                }
                ;
 

Object		: Sphere
                | SolidBox
                | Necklace 
		;

NamedMaterial   : tSTRING
                {
                   $$ = new Material();
		}
                ;

MaterialDef     : tMATERIAL tSTRING RGB
                {

		}
                ;

SolidBox	: tSOLIDBOX NamedMaterial Vector Vector
                {
		    $$ = new SolidBox(*$3,*$4,$2);
		}
                ;

Necklace 	: tNECKLACE NamedMaterial Path Expr Expr 
                {
		    $$ = new Necklace($3,int($4),$5,$2);
		}
                ;

Sphere		: tSPHERE NamedMaterial Expr Vector 
                {
		    $$ = new Sphere(*$4,$3,$2);
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
    exit(1);
}

Path* getNamedPath(string* name) {

}

void setNamedPath(string* name, Path* path) {

}

double getNamedDouble(string* name) {

}

void setNamedDouble(string* name, double path) {

}
