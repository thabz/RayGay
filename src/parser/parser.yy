
%{
    /* C declarations */
#include <string>    
#include <map>    
#include <cstdio>    
#include "math/matrix.h"
#include "math/vector.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "paths/linesegment.h"
#include "objects/cylinder.h"    
#include "objects/sphere.h"    
#include "objects/csg.h"    
#include "objects/solid.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "objects/torus.h"    
#include "materials/material.h"    

#include "camera.h"    
#include "scene.h"    
#include "renderersettings.h"

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

Camera* camera;
Scene* scene;
RendererSettings* renderer_settings;

%}
    /* Bison declarations */
%union {
        double d;
	Vector* vector;
	RGB* rgb;
	Material* material;
	SceneObject* object;
	Matrix* matrix;
	Path* path;
	string* c;
}
%token <d> tFLOAT
%token <c> tSTRING
%token tAA
%token tCAMERA
%token tCIRCLE
%token tCYLINDER
%token tDOF
%token tDIFFERENCE
%token tFOV
%token tINTERSECTION
%token tLINESEGMENT
%token tLOOKAT
%token tMATERIAL
%token tNAME
%token tNECKLACE
%token tPOSITION
%token tPRINT
%token tROTATE
%token tSOLIDBOX
%token tSPHERE
%token tSPIRAL
%token tTORUS
%token tTRANSLATE
%token tUNION
%token tUP

%type <d> Expr 
%type <rgb> RGB
%type <vector> Vector 
%type <matrix> Rotate Translate Transformation Transformations
%type <object> Sphere SolidBox Necklace Difference SolidObject Torus Cylinder
%type <object> Intersection Union TransObject Object
%type <material> MaterialDef NamedMaterial Material
%type <path> NamedPath Circle Spiral Path PathDef LineSegment

%left '+' '-'
%left '*' '/'
%%
    /* Grammar rules */
	
Items		: /* Empty */
                | Items Item
		;

Item		: TransObject
                {
		    scene->addObject($1);
		}
                | AssignName
		| Print
		| Camera
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

Camera		: tCAMERA '{' CameraSettings '}'
                ;

CameraSettings  : /* Empty */
                | CameraSettings CameraSetting
		;

CameraSetting   : tPOSITION Vector
                {
		    camera->setPosition(*$2);
		}
                | tLOOKAT Vector
                {
		    camera->setLookAt(*$2);
		}
                | tUP Vector
                {
		    camera->setUp(*$2);
		}
                | tFOV Expr 
                {
		    camera->setFieldOfView($2);
		}
                | tDOF Expr Expr
                {
		    camera->enableDoF($2,int($3));
		}
                | tAA Expr 
                {
		    camera->enableAdaptiveSupersampling(int($2));
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
                | Intersection
                | Union
		| Torus
		| Cylinder
		;

TransObject 	: Object Transformations
                {
		    $$ = $1;
		    if ($2 != (Matrix*)NULL) {
			$$->transform(*$2);
		    }
		}
                | Object
		;


Cylinder	: tCYLINDER '{' Material Expr Vector Vector '}'
                {
		    $$ = new Cylinder(*$5,*$6,$4,true,$3);
		}
                | tCYLINDER '{' Expr Vector Vector '}'
                {
		    $$ = new Cylinder(*$4,*$5,$3,true,NULL);
		};
		
SolidBox	: tSOLIDBOX '{' Material Vector Vector '}'
                {
		    $$ = new SolidBox(*$4,*$5,$3);
		}
                | tSOLIDBOX '{' Vector Vector '}'
                {
		    $$ = new SolidBox(*$3,*$4,NULL);
		};

Necklace 	: tNECKLACE '{' Material Path Expr Expr '}'
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

Difference 	: tDIFFERENCE '{' Material SolidObject SolidObject '}'
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

Intersection	: tINTERSECTION '{' Material SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($4);
		    Solid* s2 = dynamic_cast<Solid*>($5);
		    $$ = new CSG(s1,CSG::INTERSECTION,s2,$3);
		}
                | tINTERSECTION '{' SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($3);
		    Solid* s2 = dynamic_cast<Solid*>($4);
		    $$ = new CSG(s1,CSG::INTERSECTION,s2,NULL);
		}
                ;

Union		: tUNION '{' Material SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($4);
		    Solid* s2 = dynamic_cast<Solid*>($5);
		    $$ = new CSG(s1,CSG::UNION,s2,$3);
		}
                | tUNION '{' SolidObject SolidObject '}'
                {
		    Solid* s1 = dynamic_cast<Solid*>($3);
		    Solid* s2 = dynamic_cast<Solid*>($4);
		    $$ = new CSG(s1,CSG::UNION,s2,NULL);
		}
                ;

Transformations	: /* Empty */
                | Transformations Transformation
                {
		    Matrix m = (*$1) * (*$2);
		    $$ = new Matrix(m);
		}
		;

Transformation  : Rotate
                | Translate
		;

Rotate		: tROTATE '{' Vector Expr '}'
                {
		    Matrix m = Matrix::matrixRotate(*$3,$4);
		    $$ = new Matrix(m);
		}
                ;

Translate	: tTRANSLATE '{' Vector '}'
                {
		    Matrix m  = Matrix::matrixTranslate(*$3);
		    $$ = new Matrix(m);
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

void init_parser() {
    camera = new Camera();
    scene = new Scene();
    scene->setCamera(camera);


}
