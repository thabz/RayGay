
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
#include "parser/assignments.h"
#include "parser/floatnodes.h"    
#include "parser/syntaxnode.h"    
#include "parser/vectornodes.h"    
#include "parser/rgbnodes.h"    
#include "parser/langnodes.h"    
#include "parser/transformationnodes.h"    
#include "parser/lightnodes.h"    
#include "parser/cameranode.h"    
#include "parser/boolnodes.h"    

#include "camera.h"    
#include "exception.h"    
#include "scene.h"    
#include "renderersettings.h"

using namespace std;

void yyerror(string s);
void yywarning(string s);
extern int yylex(void);
extern int line_num;

CameraNode* camera;
Scene* scene;
RendererSettings* renderer_settings;
MaterialNode* tmpMaterial;
Vector2 image_size = Vector2(640,480);

ActionListNode* top_actions;

%}
    /* Bison declarations */
%union {
        double d;
	VectorNode* vector;
	RGBNode* rgb;
	RGBANode* rgba;
	ActionNode* action;
	ActionListNode* actionlist;
	Texture* texture;
	MaterialNode* material;
	CameraNode* camera;
	SceneObjectNode* object;
	LightNode* light;
	TransformationNode* matrix;
	FloatNode* expr;
	PathNode* path;
	VectorListNode* vectorlist;
	BoolNode* boolean;
	string* c;
	Texture::InterpolationType it;
}
%token <d> tFLOAT
%token <i> tINTEGER
%token <c> tSTRING tQSTRING
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
%token tLINESEGMENT tSPIRAL tCIRCLE tCATMULLROMSPLINE
%token tKD tKS tKT tSPECPOW tGLOSS
%token tLIGHT tAREA tSPOT tPOINT tSKY tPOWER
%token tMATERIAL
%token tNAME
%token tNONE
%token tNUM
%token tNORMALIZE
%token tLENGTH
%token tNOSHADOW
%token tNECKLACE
%token tOBJECT
%token tPHOTONMAP
%token tPOSITION tLOOKAT tUP
%token tPRINT
%token tRADIUS
%token tRANDOM
%token tRENDERER tRAYTRACER tPHOTONRENDERER
%token tROTATE tTRANSLATE
%token tREPEAT
%token tSIN tCOS tABS tPI t2PI
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
%token tCACHETOLERANCE 


%token tIF tWHILE tDO
%token tFALSE tTRUE
%token tBOOL_OR tBOOL_NOT tBOOL_AND tEQUALS

//%type <c> QuotedString
%type <rgb> RGB
%type <rgba> RGBA
%type <texture> Texture
%type <vector> Vector
%type <vectorlist> VectorList
%type <boolean> Bool 
%type <expr> Expr Random 
%type <it> InterpolationType 
%type <matrix> Rotate Translate Transformation Transformations
%type <object> Sphere SolidBox Necklace Difference SolidObject Torus Cylinder
%type <object> Intersection Union Object Extrusion MeshObject Wireframe Box
%type <object> ObjectGroup GroupItems GroupItem
%type <object> NamedObject 
%type <material> MaterialDef NamedMaterial Material
%type <light> LightDef Lightsource 
%type <light> Arealight Spotlight Pointlight Skylight
%type <path> NamedPath Circle Spiral Path PathDef LineSegment CatmullRomSpline
%type <camera> Camera
%type <action> MainAddAction MainAction Assignment Renderer ConfAction
%type <action> RepeatStmt IfStmt WhileStmt Action
%type <action> AddCamera AddObject AddLight Background Photonmap Print Image
%type <actionlist> ActionList

%left '+' '-'
%left '*' '/'
%left UMINUS
%left tBOOL_AND tBOOL_OR
%left tBOOL_NOT
%%
    /* Grammar rules */
	
MainActions	: /* Empty */
                | MainActions MainAddAction
		;

MainAddAction	: MainAction 
                {
		    top_actions->addAction($1);
		}
                ;

MainAction	: Action
                | ConfAction
		;

Action		: AddObject
                | AddLight
                | Assignment
		| RepeatStmt
		| Print
		| WhileStmt
		| WhileStmt
		| IfStmt
		;

ConfAction	: AddCamera
		| Image
		| Renderer
		| Background
		| Photonmap
		;

ActionList	: Action
                {
		    ActionListNode* list = new ActionListNode();
		    list->addAction($1);
		    $$ = list;
		}
                | ActionList Action
		{
		    $1->addAction($2);
		    $$ = $1;
		}
		;

RepeatStmt	: tREPEAT '(' Expr ')' '{' ActionList '}'
                {
		    $$ = new RepeatActionNode($3,$6);
		}
                ;

AddObject	: Object 
                {
		    $$ = new AddSceneObjectToSceneNode($1);
		}
                ;

AddLight	: LightDef
                {
		    $$ = new AddLightToSceneNode($1);
		}
                ;

Assignment	: tSTRING '=' PathDef
                {
		    $$ = new AssignPathNode(*$1,$3);
		}
                | tSTRING '=' Expr
                {
		    $$ = new AssignFloatNode(*$1,$3);
                }
                | tSTRING '=' MaterialDef 
                {
		    $$ = new AssignMaterialNode(*$1,$3);
                }
                | tSTRING '=' Object
                {
		    $$ = new AssignSceneObjectNode(*$1,$3);
                }
                ;

Renderer	: tRENDERER tRAYTRACER
                {
                    renderer_settings->renderertype = RendererSettings::RAYTRACER;
		    $$ = new NOPAction();
		}
                | tRENDERER tPHOTONRENDERER
                {
                    renderer_settings->renderertype = RendererSettings::PHOTON_RENDERER;
		    $$ = new NOPAction();
		}
		;

Background	: tBACKGROUND RGBA
                {
		    scene->setBackground($2->eval()); // TODO: Hack
		    $$ = new NOPAction();
		}
                | tBACKGROUND Texture
		{
		    scene->setBackground($2);
		    $$ = new NOPAction();
		}
                ;

Photonmap 	: tPHOTONMAP '{' PhotonSettings '}'
                {
		    $$ = new NOPAction();
		}
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

AddCamera	: Camera 
                {
		    $$ = new AddCameraToSceneNode($1);
		}
                ;
		      

Camera		: tCAMERA '{' 
                {
		    camera = new CameraNode();
		}
                CameraSettings '}'
                {
		    $$ = camera;
                }
                ;

CameraSettings  : /* Empty */
                | CameraSettings CameraSetting
		;

CameraSetting   : tPOSITION Vector
                {
		    camera->setPosition($2);
		}
                | tLOOKAT Vector
                {
		    camera->setLookAt($2);
		}
                | tUP Vector
                {
		    camera->setUp($2);
		}
                | tFOV Expr 
                {
		    camera->setFieldOfView($2);
		}
                | tDOF Expr Expr
                {
		    camera->enableDoF($2,$3);
		}
                | tAA Expr 
                {
		    camera->enableAA($2);
		}
                ;

Image		: tIMAGE '{' tWIDTH tFLOAT tHEIGHT tFLOAT '}'
                {
		    image_size = Vector2($4,$6);
		    $$ = new NOPAction();
		}
                | tIMAGE '{' tWIDTH tFLOAT tASPECT tFLOAT tFLOAT '}'
                {
		    image_size = Vector2($4,$4 * ($7/$6));
		    $$ = new NOPAction();
		}
		;
 
Material 	: NamedMaterial 
                | MaterialDef
		;

NamedMaterial   : tSTRING
                {
		    $$ = new NamedMaterialNode(*$1);
		}
                ;

MaterialDef     : tMATERIAL '{' 
                {
		    tmpMaterial = new MaterialNode();
		} 
                MaterialProps '}'
                {
		    $$ = tmpMaterial;
		}
                ;

MaterialProps	: /*Empty*/
                | MaterialProps MaterialProp
		;

MaterialProp 	: tDIFFUSE RGB
                {
		    tmpMaterial->setDiffuseColor($2);
		}
                | tDIFFUSE Texture
                {
		    tmpMaterial->setDiffuseColor($2);
		}
                | tBUMP Expr Texture
                {
		    tmpMaterial->setBumpTexture($3,$2);
		}
		| tSPECULAR RGB
                {
		    tmpMaterial->setSpecularColor($2);
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
		    tmpMaterial->setSpecpow($2);
		}
		| tGLOSS Expr Expr
                {
		    tmpMaterial->enableGloss($2,$3);
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
		    $$ = new PointlightNode($2,$4);
		}
                ;

Skylight 	: tSKY Expr Expr tPOWER RGB
                {
		    $$ = new SkylightNode($2,$3,$5);
		}
                ;
		
Spotlight	: tSPOT Vector Vector Expr Expr tPOWER RGB
                {
		    $$ = new SpotlightNode($2,$3,$4,$5,$7);
		}
                ;
		
Arealight	: tAREA Vector Vector Expr Expr Expr tPOWER RGB
                {
		    $$ = new ArealightNode($2,$3,$4,$5,$6,$8);
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
		    $$ = new TransformedSceneObjectNode($1,$2);
                }
		;

NamedObject	: tOBJECT tSTRING
                {
		    $$ = new NamedSceneObjectNode(*$2);
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
		    $$ = new TransformedSceneObjectNode($1,$2);
		}
		;

ObjectGroup	: tGROUP '{' GroupItems '}'
                {
		    $$ = $3;
		}
                ;

GroupItems	: GroupItem
                {
		    ObjectGroupNode* og = new ObjectGroupNode();
		    og->addSceneObjectNode($1);
		    $$ = og;
		}
                | GroupItems GroupItem
                {
		    ObjectGroupNode* og = dynamic_cast<ObjectGroupNode*>($1);
		    og->addSceneObjectNode($2);
		    $$ = og;
		}
		;

GroupItem	: Object;

Extrusion	: tEXTRUSION '{' Material Path Expr Expr Expr '}'
                {
		    $$ = new ExtrusionNode($4,$5,$6,$7,$3);
		}
                ;

Box		: tBOX '{' Material Vector Vector '}'
                {
		    $$ = new BoxNode($4,$5,$3);
		}
		;

Wireframe	: tWIREFRAME '{' Material MeshObject Expr '}'
                {
		     $$ = new WireframeNode($4,$5,$3);
		}
                ;

Cylinder	: tCYLINDER '{' Material Expr Vector Vector '}'
                {
		    $$ = new CylinderNode($5,$6,$4,$3);
		}
                | tCYLINDER '{' Expr Vector Vector '}'
                {
		    $$ = new CylinderNode($4,$5,$3,new MaterialNullNode());
		};
		
SolidBox	: tSOLIDBOX '{' Material Vector Vector '}'
                {
		    $$ = new SolidBoxNode($4,$5,$3);
		}
                | tSOLIDBOX '{' Vector Vector '}'
                {
		    $$ = new SolidBoxNode($3,$4,new MaterialNullNode());
		};

Necklace 	: tNECKLACE '{' Material Path tNUM Expr tRADIUS Expr '}'
                {
		    $$ = new NecklaceNode($4,$6,$8,$3);
		}
                ;

Sphere		: tSPHERE '{' Material Expr Vector '}'
                {
		    $$ = new SphereNode($5,$4,$3);
                }
                | tSPHERE '{' Expr Vector '}'
                {
		    $$ = new SphereNode($4,$3,new MaterialNullNode());
		}
                ;

Torus		: tTORUS '{' Expr Expr '}' 
                {
		    $$ = new TorusNode($3,$4,new MaterialNullNode());
		}
                |  tTORUS '{' Material Expr Expr '}' 
                {
		    $$ = new TorusNode($4,$5,$3);
		}

Difference 	: tDIFFERENCE '{' Material SolidObject SolidObject '}'
                {
		    $$ = new DifferenceNode($4,$5,$3);
		}
                | tDIFFERENCE '{' SolidObject SolidObject '}'
                {
		    $$ = new DifferenceNode($3,$4,new MaterialNullNode());
		}
                ;

Intersection	: tINTERSECTION '{' Material SolidObject SolidObject '}'
                {
		    $$ = new IntersectionNode($4,$5,$3);
		}
                | tINTERSECTION '{' SolidObject SolidObject '}'
                {
		    $$ = new IntersectionNode($3,$4,new MaterialNullNode());
		}
                ;

Union		: tUNION '{' Material SolidObject SolidObject '}'
                {
		    $$ = new UnionNode($4,$5,$3);
		}
                | tUNION '{' SolidObject SolidObject '}'
                {
		    $$ = new UnionNode($3,$4,new MaterialNullNode());
		}
                ;

Transformations	: /* Empty */
                {
		    $$ = new TransformationNode();
		}
                | Transformations Transformation
                {
		    $$ = new TransformationsMultNode($1,$2);
		}
		;

Transformation  : Rotate
                | Translate
		;

Rotate		: tROTATE '{' Vector Expr '}'
                {
		    $$ = new RotateNode($3,$4);
		}
                | tROTATE Vector Expr
                {
		    $$ = new RotateNode($2,$3);
		}
                ;

Translate	: tTRANSLATE '{' Vector '}'
                {
		    $$ = new TranslateNode($3);
		}
                | tTRANSLATE Vector
                {
		    $$ = new TranslateNode($2);
		};

Path		: NamedPath
                | PathDef
		;

NamedPath	: tSTRING
                {
		    $$ = new NamedPathNode(*$1);   
		}
                ;

PathDef		: Circle
                | Spiral
		| CatmullRomSpline
		| LineSegment
		;

CatmullRomSpline : tCATMULLROMSPLINE '{' VectorList '}'
                {
		    $$ = new CatmullRomSplineNode($3);
		}
                ;

LineSegment	: tLINESEGMENT '{' Vector Vector '}'
                {
		    $$ = new LinesegmentNode($3,$4);
		}
                ;

Circle		: tCIRCLE '{' Vector Expr Vector '}'
                {
		    $$ = new CircleNode($3,$4,$5);
		}
                ;

Spiral		: tSPIRAL '{' Path Expr Expr '}'
                {
		    $$ = new SpiralNode($3,$4,$5, new FloatConstNode(0));
		}
                | tSPIRAL '{' Path Expr Expr Expr '}'
                {
		    $$ = new SpiralNode($3,$4,$5,$6);
		}
		;

Texture		: tTEXTURE '{' tQSTRING tFLOAT tFLOAT InterpolationType '}'
                {
		    Image* img = new Image(*$3);
		    $$ = new Texture(img,Vector2($4,$5),$6);
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
		    $$ = new VectorNode($2,$4,$6); 
		}
                | tNORMALIZE '(' Vector ')'
		{
		    $$ = new VectorNormalizeNode($3);
		}
                ;

VectorList	: Vector
                {
		    VectorListNode* list = new VectorListNode();
		    list->add($1);
		    $$ = list;

		}
                | VectorList Vector
                {
		    $1->add($2);
		    $$ = $1;
		}
		;

RGB		: '<' Expr ',' Expr ',' Expr '>' 
                { 
		    $$ = new RGBNode($2,$4,$6); 
		}
                ; 		
		
RGBA		: '<' Expr ',' Expr ',' Expr ',' Expr '>' 
                { 
		    $$ = new RGBANode($2,$4,$6,$8); 
		}
                ; 		

Print		: tPRINT Expr
                {
		    $$ = new FloatPrintNode($2);
		}
                | tPRINT tQSTRING
                {
		    $$ = new StringPrintNode(*$2);
		}
                ;

Expr		: tFLOAT 
                {
                   $$ = new FloatConstNode($1);
                }
                | tSTRING
                {
		    $$ = new NamedFloatNode(*$1);
                }
		| '(' Expr ')' 
                {
                   $$ = $2;
		}
		| Expr '+' Expr 
                {
		    $$ = new FloatPlusNode($1,$3);
		}
		| Expr '-' Expr 
                {
		    $$ = new FloatMinusNode($1,$3);
		}
		| Expr '*' Expr 
                {
		    $$ = new FloatMultNode($1,$3);
		}
		| Expr '/' Expr 
                {
		    $$ = new FloatDivNode($1,$3);
		}
		| '-' Expr %prec UMINUS
                {
		    $$ = new FloatNegNode($2);
		}
		| '+' Expr %prec UMINUS
                {
                    $$ = $2;
		}
                ;

Expr		: tSIN '(' Expr ')'
                {
		    $$ = new FloatSinNode($3);
		}
                | tCOS '(' Expr ')'
                {
		    $$ = new FloatCosNode($3);
		}
                | tABS '(' Expr ')'
                {
		    $$ = new FloatAbsNode($3);
		}
                | tLENGTH '(' Vector ')'
		{
		    $$ = new VectorLengthNode($3);
		}
                | tPI 
                {
		    $$ = new FloatConstNode(M_PI);
		}
                | t2PI 
                {
		    $$ = new FloatConstNode(M_2PI);
		}
                | Random
                ;

Random		: tRANDOM '(' Expr ')'
                {
		    $$ = new FloatRandomNode(new FloatConstNode(0),$3);
		}
                | tRANDOM '(' Expr ',' Expr ')'
                {
		    $$ = new FloatRandomNode($3,$5);
		}
		;

WhileStmt	: tWHILE '(' Bool ')' '{' ActionList '}'
                {
		    $$ = new WhileActionNode($3,$6);
		}
                | tDO '{' ActionList '}' tWHILE '(' Bool ')'
                {
		    $$ = new DoWhileActionNode($3,$7);
		}
                ;

IfStmt		: tIF '(' Bool ')' '{' ActionList '}'
                {
		    $$ = new IfActionNode($3,$6);
		}
                ;

Bool		: Expr '<' Expr
		{
		    $$ = new BoolLessThanFNode($1,$3);
		}
                | Expr '>' Expr
		{
		    $$ = new BoolGreaterThanFNode($1,$3);
		}
		| Expr tEQUALS Expr
		{
		    $$ = new BoolEqualsFNode($1,$3);
		}
		| '(' Bool ')'
		{
		    $$ = $2;
		}
                | Bool tBOOL_AND Bool 
		{
		    $$ = new BoolAndNode($1,$3);
		}
                | Bool tBOOL_OR Bool
		{
		    $$ = new BoolOrNode($1,$3);
		}
                | tBOOL_NOT Bool
		{
		    $$ = new BoolNotNode($2);
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

void init_parser() {
    scene = new Scene();
    renderer_settings = new RendererSettings();
    top_actions = new ActionListNode();
    InterpreterEnv::getUniqueInstance()->setScene(scene);
}

void run_interpreter() {
    top_actions->eval();
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

