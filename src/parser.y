
%{
    /* C declarations */
#include "math/vector.h"
#include "objects/sphere.h"    
#include "objects/solidbox.h"    
#include "materials/material.h"    
%}
    /* Bison declarations */
%union {
        double d;
	Vector vector;
	RGB rgb;
	Material* material;
	Object* object;
}
%token <d> tFLOAT
%token <c> tSTRING
%token tMATERIAL
%token tSPHERE
%token tSOLIDBOX

%type <d> Expr 
%type <rgb> RGB
%type <vector> Vector
%type <object> Sphere SolidBox
%type <material> MaterialDef NamedMaterial 

%left '+' '-'
%left '*' '/'
%%
    /* Grammar rules */
	
Items		: /* Empty */
                | Items Item
		;

Item		: Object
                | MaterialDef;

Object		: Sphere
                | SolidBox
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
		    $$ = new SolidBox($3,$4,$2);
		}
                ;

Sphere		: tSPHERE NamedMaterial Expr Vector 
                {
		    $$ = new Sphere($4,$3,$2);
                }
                ;

Vector		: '<' Expr ',' Expr ',' Expr '>' { $$ = Vector($2,$4,$6); }
                ;

RGB		: '<' Expr ',' Expr ',' Expr '>' { $$ = RGB($2,$4,$6); }
                ; 		

Expr		: tFLOAT 
                {
                   $$ = $1;
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
