
%{
    /* C declarations */
#include <string>    
#include <map>    
#include <cstdio>    
#include "image/rgba.h"
#include "image/rgb.h"
#include "image/image.h"
#include "image/texture.h"
#include "lights/lightsource.h"
#include "lights/arealight.h"
#include "lights/pointlight.h"
#include "lights/spotlight.h"
#include "lights/skylight.h"
#include "math/matrix.h"
#include "math/vector.h"
#include "paths/spiral.h"
#include "paths/circle.h"
#include "paths/linesegment.h"
#include "objects/objectgroup.h"    
#include "objects/cylinder.h"    
#include "objects/extrusion.h"    
#include "objects/sphere.h"    
#include "objects/box.h"    
#include "objects/csg.h"    
#include "objects/solid.h"    
#include "objects/solidbox.h"    
#include "objects/necklace.h"    
#include "objects/wireframe.h"    
#include "objects/torus.h"    
#include "materials/material.h"    
#include "parser/floatnodes.h"    
#include "parser/syntaxnode.h"    
#include "parser/vectornodes.h"    
#include "parser/rgbnodes.h"    
#include "parser/langnodes.h"    
#include "parser/transformationnodes.h"    
#include "parser/lightnodes.h"    

#include "camera.h"    
#include "exception.h"    
#include "scene.h"    
#include "renderersettings.h"

using namespace std;

map<string,double> doubleMap;
map<string,Path*> pathMap;
map<string,Material*> materialMap;
map<string,SceneObject*> objectMap;

void yyerror(string s);
void yywarning(string s);
extern int yylex(void);
extern int line_num;

Path* getNamedPath(string* name);
void setNamedPath(string* name, Path* path);
double getNamedDouble(string* name);
void setNamedDouble(string* name, double val);
Material* getNamedMaterial(string* name);
void setNamedMaterial(string* name, Material* material);
SceneObject* getNamedObject(string* name);
void setNamedObject(string* name, SceneObject* obj);

Camera* camera;
Scene* scene;
RendererSettings* renderer_settings;
Material* tmpMaterial;
Vector2 image_size = Vector2(640,480);

%}
    /* Bison declarations */
%union {
        double d;
	Vector* vector;
	RGB* rgb;
	RGBA* rgba;
	Texture* texture;
	Material* material;
	SceneObject* object;
	Lightsource* light;
	Matrix* matrix;
	Path* path;
	string* c;
	Texture::InterpolationType it;
}
%token <d> tFLOAT
%token <i> tINTEGER
%token <c> tSTRING
%token tAA
%token tBACKGROUND
%token tBICUBIC
%token tBILINEAR
%token tBOX
%token tCAMERA
%token tCYLINDER
%token tDOF
%token tDIFFUSE tSPECULAR tBUMP 
%token tETA
%token tEXTRUSION
%token tFOV
%token tGROUP
%token tIMAGE tWIDTH tHEIGHT tASPECT
%token tLINESEGMENT tSPIRAL tCIRCLE
%token tKD tKS tKT tSPECPOW tGLOSS
%token tLIGHT tAREA tSPOT tPOINT tSKY tPOWER
%token tMATERIAL
%token tNAME
%token tNONE
%token tNUM
%token tNOSHADOW
%token tNECKLACE
%token tOBJECT
%token tPHOTONMAP
%token tPOSITION tLOOKAT tUP
%token tPRINT
%token tRADIUS
%token tRENDERER tRAYTRACER tPHOTONRENDERER
%token tROTATE tTRANSLATE
%token tSIN tCOS tABS tPI
%token tSOLIDBOX
%token tSPHERE
%token tTEXTURE
%token tTORUS
%token tWIREFRAME
%token tUNION tDIFFERENCE tINTERSECTION
%token tGLOBALPHOTONS 
%token tCAUSTICPHOTONS 
%token tESTIMATERADIUS 
%token tESTIMATESAMPLES
%token tFINALGATHERRAYS
%token tCACHETOLERANCE 


%type <d> Expr 
%type <c> Filename
%type <rgb> RGB
%type <rgba> RGBA
%type <texture> Texture
%type <vector> Vector 
%type <it> InterpolationType 
%type <matrix> Rotate Translate Transformation Transformations
%type <object> Sphere SolidBox Necklace Difference SolidObject Torus Cylinder
%type <object> Intersection Union Object Extrusion MeshObject Wireframe Box
%type <object> ObjectGroup GroupItems GroupItem
%type <object> NamedObject
%type <material> MaterialDef NamedMaterial Material
%type <light> LightDef Lightsource Arealight Spotlight Pointlight Skylight
%type <path> NamedPath Circle Spiral Path PathDef LineSegment

%left '+' '-'
%left '*' '/'
%left UMINUS
%%
    /* Grammar rules */
	
Items		: /* Empty */
                | Items Item
		;

Item		: Object
                {
		    Object* o = dynamic_cast<Object*>($1);
		    if (o != NULL) {
			if (o->getMaterial() == NULL) {
			    yyerror("object with no material added to scene.");
			}
		    }
		    scene->addObject($1);
		}
                | LightDef
		{
		    scene->addLight($1);
		}
                | Assignment
		| Print
		| Camera
		| Image
		| Renderer
		| Background
		| Photonmap
		;

Assignment	: tSTRING '=' PathDef
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
                | tSTRING '=' Object
                {
		    setNamedObject($1, $3);
                }
                ;

Renderer	: tRENDERER tRAYTRACER
                {
                    renderer_settings->renderertype = RendererSettings::RAYTRACER;
		}
                | tRENDERER tPHOTONRENDERER
                {
                    renderer_settings->renderertype = RendererSettings::PHOTON_RENDERER;
		}
		;

Background	: tBACKGROUND RGBA
                {
		    scene->setBackground(*$2);
		}
                | tBACKGROUND Texture
		{
		    scene->setBackground($2);
		}
                ;

Photonmap 	: tPHOTONMAP '{' PhotonSettings '}'
                ;

PhotonSettings  : /* Empty */
                | PhotonSettings PhotonSetting
		;

PhotonSetting   : tGLOBALPHOTONS tFLOAT
		{
		    renderer_settings->global_photons_num = int($2);
		}
                | tCAUSTICPHOTONS tFLOAT
		{
		    renderer_settings->caustic_photons_num = int($2);
		}
		| tESTIMATERADIUS tFLOAT
		{
		    renderer_settings->estimate_radius = int($2);
		}
		| tESTIMATESAMPLES tFLOAT
		{
		    renderer_settings->estimate_samples = int($2);
		}
		| tFINALGATHERRAYS tFLOAT
		{
		    renderer_settings->final_gather_rays = int($2);
		}
		| tCACHETOLERANCE tFLOAT
		{
		    renderer_settings->cache_tolerance = int($2);
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

Image		: tIMAGE '{' tWIDTH tFLOAT tHEIGHT tFLOAT '}'
                {
		    image_size = Vector2($4,$6);
		}
                | tIMAGE '{' tWIDTH tFLOAT tASPECT tFLOAT tFLOAT '}'
                {
		    image_size = Vector2($4,$4 * ($7/$6));
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

MaterialDef     : tMATERIAL '{' MaterialProps '}'
                {
		    $$ = tmpMaterial;
		    // Sanity check some material parameters
		    if ($$->getKs() > 0 && $$->getSc() == 0) {
			yywarning("ks > 0 but specpow = 0");
		    }
		    tmpMaterial = new Material();
		}
                ;

MaterialProps	: /*Empty*/
                | MaterialProps MaterialProp
		;

MaterialProp 	: tDIFFUSE RGB
                {
		    tmpMaterial->setDiffuseColor(*$2);
		}
                | tDIFFUSE Texture
                {
		    tmpMaterial->setDiffuseTexture($2);
		}
                | tBUMP Expr Texture
                {
		    tmpMaterial->setBumpTexture($3,$2);
		}
		| tSPECULAR RGB
                {
		    tmpMaterial->setSpecularColor(*$2);
		}
		| tKD Expr
                {
		    tmpMaterial->setKd($2);
		}
		| tKS Expr
                {
		    tmpMaterial->setKs($2);
		}
		| tKT Expr
                {
		    tmpMaterial->setKt($2);
		}
		| tETA Expr
                {
		    tmpMaterial->setEta($2);
		}
		| tSPECPOW Expr
                {
		    tmpMaterial->setSc(int($2));
		}
		| tGLOSS Expr Expr
                {
		    tmpMaterial->enableGloss(int($2),$3);
		}
		| tNOSHADOW
                {
		    tmpMaterial->setNoShadow(true);
		}
		;

LightDef	: tLIGHT '{' Lightsource '}'
                {
		    $$ = $3;
		}
                ;

Lightsource	: Arealight
                | Spotlight
		| Pointlight
		| Skylight
		;

Pointlight	: tPOINT Vector tPOWER RGB
                {
		    $$ = new Pointlight(*$2);
		    $$->setPower(*$4);
		}
                | tPOINT Vector
                {
		    $$ = new Pointlight(*$2);
		}
                ;

Skylight 	: tSKY Expr Expr tPOWER RGB
                {
		    $$ = new Skylight($2,int($3));
		    $$->setPower(*$5);
		}
                | tSKY Expr Expr
                {
		    $$ = new Skylight($2,int($3));
		}
                ;
		
Spotlight	: tSPOT Vector Vector Expr Expr tPOWER RGB
                {
		    $$ = new Spotlight(*$2,*$3,$4,$5);
		    $$->setPower(*$7);
		}
                | tSPOT Vector Vector Expr Expr 
                {
		    $$ = new Spotlight(*$2,*$3,$4,$5);
		}
                ;
		
Arealight	: tAREA Vector Vector Expr Expr Expr tPOWER RGB
                {
		    $$ = new Arealight(*$2,*$3,$4,int($5),$6);
		    $$->setPower(*$8);
		}
                | tAREA Vector Vector Expr Expr Expr
                {
		    $$ = new Arealight(*$2,*$3,$4,int($5),$6);
		}
                ;

Object		: SolidObject
                | Necklace 
		| Wireframe
		| MeshObject 
		| ObjectGroup
		| NamedObject
		| Object Transformations
                {
		    $$ = $1;
		    $$->transform(*$2);
                }
		;

NamedObject	: tOBJECT tSTRING
                {
		    $$ = getNamedObject($2)->clone();
		}
                ;

MeshObject	: Extrusion
                | Box
		;

SolidObject	: Sphere
                | SolidBox
                | Difference 
                | Intersection
                | Union
		| Torus
		| Cylinder
		| SolidObject Transformations
                {
		    $$ = $1;
		    $$->transform(*$2);
		}
		;

ObjectGroup	: tGROUP '{' GroupItems '}'
                {
		    $$ = $3;
		}
                ;

GroupItems	: GroupItem
                {
		    ObjectGroup* og = new ObjectGroup();
		    og->addObject($1);
		    $$ = og;
		}
                | GroupItems GroupItem
                {
		    ObjectGroup* og = dynamic_cast<ObjectGroup*>($1);
		    og->addObject($2);
		    $$ = og;
		}
		;

GroupItem	: Object;

Extrusion	: tEXTRUSION '{' Material Path Expr Expr Expr '}'
                {
		    $$ = new Extrusion(*$4,$5,(int)$6,(int)$7,$3);
		}
                ;

Box		: tBOX '{' Material Vector Vector '}'
                {
		    $$ = new Box(*$4,*$5,$3);
		}
		;

Wireframe	: tWIREFRAME '{' Material MeshObject Expr '}'
                {
                     Mesh* mesh = dynamic_cast<Mesh*>($4);
		     $$ = new Wireframe(mesh,$5,$3);
		}
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

Necklace 	: tNECKLACE '{' Material Path tNUM Expr tRADIUS Expr '}'
                {
		    $$ = new Necklace($4,int($6),$8,$3);
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
                {
		    $$ = new Matrix();
		}
                | Transformations Transformation
                {
		    Matrix m = (*$1) * (*$2);
		    $$ = new Matrix(m);
		    delete $1;
		    delete $2;
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
                | tROTATE Vector Expr
                {
		    Matrix m = Matrix::matrixRotate(*$2,$3);
		    $$ = new Matrix(m);
		}
                ;

Translate	: tTRANSLATE '{' Vector '}'
                {
		    Matrix m  = Matrix::matrixTranslate(*$3);
		    $$ = new Matrix(m);
		}
                | tTRANSLATE Vector
                {
		    Matrix m  = Matrix::matrixTranslate(*$2);
		    $$ = new Matrix(m);
		};

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

Texture		: tTEXTURE '{' Filename Expr Expr InterpolationType '}'
                {
		    Image* img = new Image(*$3);
		    $$ = new Texture(img,Vector2($4,$5),$6);
		}
                ;

Filename	: '"' tSTRING '"'
                {
                    $$ = $2;
		}
                ;

InterpolationType
                : tNONE
                {
		    $$ = Texture::INTERPOLATION_NONE;
		}
                | tBILINEAR
                {
		    $$ = Texture::INTERPOLATION_BILINEAR;
		}
		| tBICUBIC
                {
		    $$ = Texture::INTERPOLATION_BICUBIC;
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
		
RGBA		: '<' Expr ',' Expr ',' Expr ',' Expr '>' 
                { 
		    $$ = new RGBA($2,$4,$6,$8); 
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
		| '-' Expr %prec UMINUS
                {
                    $$ = -$2;
		}
		| '+' Expr %prec UMINUS
                {
                    $$ = $2;
		}
                ;

Expr		: tSIN '(' Expr ')'
                {
		    $$ = sin($3);
		}
                | tCOS '(' Expr ')'
                {
		    $$ = cos($3);
		}
                | tABS '(' Expr ')'
                {
		    $$ = fabs($3);
		}
                | tPI 
                {
		    $$ = M_PI;
		}
                ;

%%

    /* Additional C code */

void yyerror(string s) {
    string filename = "";
    cout << filename << ":" << line_num << ":"
	 << " error: " << s << endl;
    exit(1);
}

void yywarning(string s) {
    string filename = "";
    cout << filename << ":" << line_num << ":"
	 << " warning: " << s << endl;
}

Path* getNamedPath(string* name) {
    Path* result = pathMap[*name];
    if (result == NULL) {
	yyerror("path '" + *name + "' not defined.");
    } 
    return result;
   
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

SceneObject* getNamedObject(string* name) {
    SceneObject* result = objectMap[*name];
    if (result == NULL) {
	yyerror("scene-object '" + *name + "' not defined.");
    }
    return result;
}

void setNamedObject(string* name, SceneObject* obj) {
    objectMap[*name] = obj;
}

Material* getNamedMaterial(string* name) {
    Material* result = materialMap[*name];
    if (result == NULL) {
	yyerror("material '" + *name + "' not defined.");
    }
    return result;
}

void setNamedMaterial(string* name, Material* material) {
    materialMap[*name] = material;
}

void init_parser() {
    camera = new Camera();
    scene = new Scene();
    scene->setCamera(camera);
    renderer_settings = new RendererSettings();
    tmpMaterial = new Material();
}

Vector2 getImageSize() {
    return image_size;
}

Scene* getScene() {
    return scene;
}

RendererSettings* getRendererSettings() {
    return renderer_settings;
}

