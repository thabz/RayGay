
%{
    /* C declarations */
#include <string>    
#include <vector>    
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
#include "objects/ellipsoid.h"    
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
#include "parser/function.h"    
#include "parser/runtimeexception.h"    

#include "camera.h"    
#include "exception.h"    
#include "renderersettings.h"
#include "environment.h"
#include "parser/objectcollector.h"
#include "parser/fileposition.h"

using namespace std;

void yyerror(string s);
void yywarning(string s);
FilePosition curPos();
extern int yylex(void);
extern vector<FilePosition> fileposition_stack;

CameraNode* camera;
RendererSettings* renderer_settings;
MaterialNode* tmpMaterial;
Vector2 image_size = Vector2(640,480);
Function* tmpFunction;

ActionListNode* top_actions;

#define YYERROR_VERBOSE

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
	FuncCallArgs* funccallargs;
	FuncArgsDecls* funcargsdecls;
	ValueNode* value;
}
%token <d> tFLOAT
%token <i> tINTEGER
%token <c> tSTRING tQSTRING tVARNAME tVECTORVARNAME
%token tAA
%token tBACKGROUND
%token tBICUBIC
%token tBILINEAR
%token tBLOB
%token tBOX
%token tCAMERA
%token tCONE
%token tCYLINDER
%token tDOF
%token tDIFFUSE tSPECULAR tBUMP 
%token tETA
%token tELLIPSOID
%token tEXTRUSION
%token tFOV
%token tFRAMES
%token tFUNCTION
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
%token tMESH tTRIANGLES tVERTICES
%token tNOSHADOW
%token tNECKLACE
%token tOBJECT
%token tPATHS
%token tPHOTONMAP
%token tPOSITION tLOOKAT tUP
%token tPRINT
%token tRADIUS
%token tRANDOM
%token tRENDERER tRAYTRACER tPHOTONRENDERER tPATHTRACER
%token tROTATE tTRANSLATE tSCALE
%token tREPEAT
%token tSIN tCOS tABS tPI t2PI
%token tSETTINGS
%token tSOLIDBOX
%token tSPHERE
%token tSUPERELLIPSOID
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
%token tEQUAL
%token tPLUSEQUAL tMINUSEQUAL tMULTEQUAL tDIVEQUAL
%token tPLUSPLUS tMINUSMINUS 
%token tIF tELSE tWHILE tDO
%token tFALSE tTRUE
%token tMINUS tPLUS
%token tBOOL_OR tBOOL_NOT tBOOL_AND tEQUALEQUAL

//%type <c> QuotedString
%type <rgb> RGB
%type <rgba> RGBA
%type <texture> Texture
%type <vector> Vector
%type <vectorlist> VectorList Vertices Triangles
%type <boolean> Bool 
%type <expr> Expr Random  ExprMod
%type <it> InterpolationType 
%type <matrix> Rotate Translate Scale Transformation Transformations
%type <object> Sphere SolidBox Necklace Difference SolidObject Torus Cylinder
%type <object> Intersection Union Object Extrusion MeshObject Wireframe Box
%type <object> ObjectGroup Ellipsoid Mesh Cone SuperEllipsoid Blob
%type <object> NamedObject 
%type <material> MaterialDef NamedMaterial Material
%type <light> LightDef Lightsource 
%type <light> Arealight Spotlight Pointlight Skylight
%type <path> NamedPath Circle Spiral Path PathDef LineSegment CatmullRomSpline
%type <camera> Camera
%type <action> MainAddAction MainAction Assignment Renderer ConfAction
%type <action> RepeatStmt IfStmt WhileStmt Action ModStmt OpAssignment
%type <action> AddCamera AddObject AddLight Background Settings Print Image
%type <action> FuncCall FuncDecl
%type <actionlist> ActionList
%type <funccallargs> FuncCallArgs
%type <value> FuncCallArg
%type <funcargsdecls> FuncArgsDecls

%left tPLUS tMINUS
%left '*' '/'
%left tPLUSPLUS tMINUSMINUS
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
		| FuncDecl
		;

Action		: AddObject
                | AddLight
                | Assignment
		| RepeatStmt
		| Print
		| WhileStmt
		| IfStmt
		| ModStmt 		/* $x++ */
		| OpAssignment 		/* $x += 1 */
		| FuncCall
		;

ConfAction	: AddCamera
		| Image
		| Renderer
		| Background
		| Settings
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
		    $$ = new AddSceneObjectToCollectorNode($1);
		}
                ;

AddLight	: LightDef
                {
		    $$ = new AddLightToSceneNode($1);
		}
                ;

FuncDecl	: tFUNCTION tSTRING '(' FuncArgsDecls ')'
                {
		    tmpFunction = new Function($4);
		    Assignments::getUniqueInstance()->setNamedFunction(*$2,tmpFunction);
		}
                '{' ActionList '}'
		{
		    tmpFunction->setActionList($8);
		    $$ = new NOPAction();
		}
                ;

FuncCall	: tVARNAME '(' FuncCallArgs ')'
                {
		    Function* f = Assignments::getUniqueInstance()->getNamedFunction(*$1,curPos());
		    if (f == NULL) {
			yyerror("Function '"+(*$1)+"' not declared.");
		    }
		    unsigned int nargs = $3->size();
		    unsigned int fargs = f->getArgsNum();
		    if (fargs != nargs) {
			char line[1000];
			sprintf(line,"Function %s required %d arguments. Caller only has %d",$1->c_str(),fargs,nargs);
			yyerror(line);
		    }
		    $$ = new FuncCallNode(f,$3);
		}
                ;

		
		
FuncArgsDecls	: tVARNAME
                {
		    $$ = new FuncArgsDecls();
		    $$->addArg(ValueNode::FLOAT,*$1);
		    delete $1;
		}
                | tVECTORVARNAME
                {
		    $$ = new FuncArgsDecls();
		    $$->addArg(ValueNode::VECTOR,*$1);
		    delete $1;
		}
		| FuncArgsDecls tVARNAME
                {
		    FuncArgsDecls* f = dynamic_cast<FuncArgsDecls*>($1);
		    f->addArg(ValueNode::FLOAT,*$2);
		    $$ = f;
		}
		| FuncArgsDecls tVECTORVARNAME
                {
		    FuncArgsDecls* f = dynamic_cast<FuncArgsDecls*>($1);
		    f->addArg(ValueNode::VECTOR,*$2);
		    $$ = f;
		}
                ;

FuncCallArgs	: FuncCallArg
                {
		    $$ = new FuncCallArgs();
		    $$->addArg($1);
		}
                | FuncCallArgs FuncCallArg
		{
		    FuncCallArgs* f = dynamic_cast<FuncCallArgs*>($1);
		    f->addArg($2);
		    $$ = f;
		}
		;

FuncCallArg	: Vector
                {
		    $$ = $1;
		}
                | Expr
                {
		    $$ = $1;
		}
		;

Assignment	: tVARNAME tEQUAL PathDef
                {
		    $$ = new AssignPathNode(*$1,$3);
		    delete $1;
		}
                | tVARNAME tEQUAL Expr
                {
		    $$ = new AssignFloatNode(*$1,$3);
		    delete $1;
                }
                | tVECTORVARNAME tEQUAL Vector
                {
		    $$ = new AssignVectorNode(*$1,$3);
		    delete $1;
                }
                | tVARNAME tEQUAL MaterialDef 
                {
		    $$ = new AssignMaterialNode(*$1,$3);
		    delete $1;
                }
                | tVARNAME tEQUAL Object
                {
		    $$ = new AssignSceneObjectNode(*$1,$3);
		    delete $1;
                }
                ;

OpAssignment	: tVARNAME tPLUSEQUAL Expr
                {
		    $$ = new FloatOpEqualsNode(*$1,'+',$3,curPos());
		    delete $1;
		}
                | tVARNAME tMINUSEQUAL Expr
                {
		    $$ = new FloatOpEqualsNode(*$1,'-',$3,curPos());
		    delete $1;
		}
                | tVARNAME tMULTEQUAL Expr
                {
		    $$ = new FloatOpEqualsNode(*$1,'*',$3,curPos());
		    delete $1;
		}
                | tVARNAME tDIVEQUAL Expr
                {
		    $$ = new FloatOpEqualsNode(*$1,'/',$3,curPos());
		    delete $1;
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
                | tRENDERER tPATHTRACER
                {
                    renderer_settings->renderertype = RendererSettings::PATHTRACER;
		    $$ = new NOPAction();
		}
		;

Background	: tBACKGROUND RGBA
                {
		    $$ = new SetBackgroundNode($2->eval());
		}
                | tBACKGROUND Texture
		{
		    $$ = new SetBackgroundNode($2);
		}
                ;

Settings 	: tSETTINGS '{' SettingsList '}'
                {
		    $$ = new NOPAction();
		}
                ;

SettingsList	: /* Empty */
                | SettingsList Setting
		;

Setting		: tGLOBALPHOTONS tFLOAT
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
		| tPATHS tFLOAT
		{
		    renderer_settings->camera_paths = int($2);
		}
		| tFRAMES tFLOAT
		{
		    renderer_settings->anim_frames = int($2);
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

NamedMaterial   : tVARNAME
                {
		    $$ = new NamedMaterialNode(*$1,curPos());
		    delete $1;
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
		| SuperEllipsoid
		| Blob
		| Object Transformations
                {
		    $$ = new TransformedSceneObjectNode($1,$2);
                }
		;

NamedObject	: tOBJECT tVARNAME
                {
		    $$ = new NamedSceneObjectNode(*$2,curPos());
		    delete $2;
		}
                ;

MeshObject	: Extrusion
                | Box
		| Mesh
		;

Mesh		: tMESH '{' Material Vertices Triangles '}'
                {
		    $$ = new MeshNode($4,$5,$3);
		}
                ;

Vertices	: tVERTICES '{' VectorList '}'
                {
		    $$ = $3;
		}
                ;
                
Triangles	: tTRIANGLES '{' VectorList '}'
                {
		    $$ = $3;
		}
                ;

		
SolidObject	: Sphere
		| Ellipsoid
                | SolidBox
                | Difference 
                | Intersection
                | Union
		| Torus
		| Cylinder
		| Cone
		| SolidObject Transformations
                {
		    $$ = new TransformedSceneObjectNode($1,$2);
		}
		;

ObjectGroup	: tGROUP '{' ActionList '}'
                {
		    $$ = new ObjectGroupNode($3);
		}
                ;

Blob		: tBLOB '{' Material Expr Expr Expr Expr ObjectGroup '}'
                {
		    ObjectGroupNode* n = dynamic_cast<ObjectGroupNode*>($8);
		    $$ = new BlobNode($4,$5,$6,$7,n,$3);
		}
                ;

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

Cone		: tCONE '{' Material Expr Expr Vector Vector '}'
                {
		    $$ = new ConeNode($6,$7,$4,$5,$3);
		}
                | tCONE '{' Expr Expr Vector Vector '}'
                {
		    $$ = new ConeNode($5,$6,$3,$4,new MaterialNullNode());
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

Ellipsoid	: tELLIPSOID '{' Material Vector Vector '}'
                {
		    $$ = new EllipsoidNode($5,$4,$3);
                }
                | tELLIPSOID '{' Vector Vector '}'
                {
		    $$ = new EllipsoidNode($4,$3,new MaterialNullNode());
		}
                ;

SuperEllipsoid 	: tSUPERELLIPSOID '{' Material Expr Expr Expr Expr '}'
                {
		    $$ = new SuperEllipsoidNode($4,$5,$6,$7,$3);
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

Union		: tUNION '{' Material ActionList '}'
                {
		    $$ = new UnionNode($4,$3);
		}
                | tUNION '{' ActionList '}'
                {
		    $$ = new UnionNode($3,new MaterialNullNode());
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
		| Scale
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

Scale		: tSCALE '{' Vector '}'
                {
		    $$ = new ScaleNode($3);
		}
                | tSCALE Vector
                {
		    $$ = new ScaleNode($2);
		};
		;

Path		: NamedPath
                | PathDef
		;

NamedPath	: tVARNAME
                {
		    $$ = new NamedPathNode(*$1,curPos());   
		    delete $1;
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
		    delete $3;
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
                | '<' Expr Expr Expr '>'
                {
		    $$ = new VectorNode($2,$3,$4); 
		}
                | tNORMALIZE '(' Vector ')'
		{
		    $$ = new VectorNormalizeNode($3,curPos());
		}
                | Vector '*' Expr
		{
		    $$ = new VectorMultNode($1,$3);
		}
                | Expr '*' Vector 
		{
		    $$ = new VectorMultNode($3,$1);
		}
                | Vector tPLUS Vector 
		{
		    $$ = new VectorPlusNode($1,$3);
		}
                | Vector tMINUS Vector 
		{
		    $$ = new VectorMinusNode($1,$3);
		}
                | tVECTORVARNAME 
		{
		    $$ = new NamedVectorNode(*$1,curPos());
		    delete $1;
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
                | tPRINT Vector
                {
		    $$ = new VectorPrintNode($2);
		}
                | tPRINT tQSTRING
                {
		    $$ = new StringPrintNode(*$2);
		    delete $2;
		}
                ;

Expr		: tFLOAT 
                {
                   $$ = new FloatConstNode($1);
                }
                | tVARNAME
                {
		    $$ = new NamedFloatNode(*$1,curPos());
		    delete $1;
                }
		| '(' Expr ')' 
                {
                   $$ = $2;
		}
		| Expr tPLUS Expr 
                {
		    $$ = new FloatPlusNode($1,$3);
		}
		| Expr tMINUS Expr 
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
		| tMINUS Expr %prec UMINUS
                {
		    $$ = new FloatNegNode($2);
		}
		| '+' Expr %prec UMINUS
                {
                    $$ = $2;
		}
                | ExprMod
                ;

		// These are called "compound assignments" in the literature.
ModStmt		: ExprMod
                {
		    $$ = new ModifyNamedFloatActionNode($1);
		}
                ;

ExprMod		: tVARNAME tPLUSPLUS
                {
		    $$ = new ModifyNamedFloatNode(*$1,'+',false,curPos());
		    delete $1;
		}
                | tVARNAME tMINUSMINUS
                {
		    $$ = new ModifyNamedFloatNode(*$1,'-',false,curPos());
		    delete $1;
		}
                | tPLUSPLUS tVARNAME
                {
		    $$ = new ModifyNamedFloatNode(*$2,'+',true,curPos());
		    delete $2;
		}
                | tMINUSMINUS tVARNAME
                {
		    $$ = new ModifyNamedFloatNode(*$2,'-',true,curPos());
		    delete $2;
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
		    $$ = new IfActionNode($3,$6,NULL);
		}
                | tIF '(' Bool ')' '{' ActionList '}' tELSE '{' ActionList '}'
                {
		    $$ = new IfActionNode($3,$6,$10);
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
		| Expr tEQUALEQUAL Expr
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
    FilePosition f = curPos();
    cout << f.getFilename() << ":" << f.getLineNum() << ":"
	 << " error: " << s << endl;
    exit(1);
}

void yywarning(string s) {
    FilePosition f = curPos();
    cout << f.getFilename() << ":" << f.getLineNum() << ":"
	 << " warning: " << s << endl;
}

extern FILE* yyin;

void openfile(string filename) {
    yyin = fopen(filename.c_str(),"r");
    if (yyin == NULL) {
	throw_exception("File not found: " + filename);
    }

    // Change cwd to this files parent folder
    char original_working_dir[1024];
    getcwd(original_working_dir,1024);
    string original_cwds = string(original_working_dir);
    string cwds = string(original_working_dir) + "/" + filename;
    cout << "Reading " << cwds << endl;
    int idx = cwds.find_last_of('/');
    cwds.resize(idx);
    cout << "CWD: " << cwds << endl;
    chdir(cwds.c_str());
}

void init_parser(string scenefile) {
    openfile(scenefile);
    fileposition_stack.clear();
    fileposition_stack.push_back(scenefile);
    renderer_settings = new RendererSettings();
    top_actions = new ActionListNode();
}

void run_interpreter() {
    Environment::getUniqueInstance()->getObjectCollector()->reset();
    try {
	top_actions->eval();
    } catch (RuntimeException e) {
	FilePosition f = e.getFilePosition();
	cout << f.getFilename() << ":" << f.getLineNum() << ":"
	    << " runtime error: " << e.getMessage() << endl;
	exit(1);
    }
    // Insert objects in object collector into scene
    Scene* scene = Environment::getUniqueInstance()->getScene();
    ObjectCollector* oc = Environment::getUniqueInstance()->getObjectCollector();
    vector<SceneObject*> list = oc->pop();
    for(unsigned int i = 0; i < list.size(); i++) {
	scene->addObject(list[i]);
    }
}

FilePosition curPos() {
    return fileposition_stack.front();
}

void delete_interpreter() {
    delete top_actions;
}

Vector2 getImageSize() {
    return image_size;
}

RendererSettings* getRendererSettings() {
    return renderer_settings;
}


