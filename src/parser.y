
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
	Vector v;
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
%type <obj> Sphere SolidBox
%type <material> MaterialDef NamedMaterial 
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

Vector		: Expr Expr Expr { $$ = Vector($1,$2,$3); }
                ;
RGB		: Expr Expr Expr { $$ = RGB($1,$2,$3); }
                ; 		
Expr		: tFLOAT
                ;
%%

    /* Additional C code */
