
#include <iostream>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <sys/stat.h>

#include "3ds.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "mesh.h"
#include "image/rgb.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

using namespace std;

ThreeDS::ThreeDS(const string& filename, const double scale, const Material* material) {
    this->force_my_material = true;
    this->material = material;
    init(filename,scale);  
}

ThreeDS::ThreeDS(const string& filename, const double scale) {
    this->force_my_material = false;
    init(filename,scale);
}

void ThreeDS::init(const string& filename, const double scale) {
    this->scale = scale;
    load3ds(filename);
}

void ThreeDS::createMesh() {

    unsigned long faces_size = faces.size();
    unsigned long vertices_size = vertices.size();
    if (faces_size == 0 || vertices_size == 0) {
	cout << "Ignored submesh" << endl;
	return;
    }

    assert(material != NULL);

    Mesh* mesh = new Mesh(Mesh::MESH_FLAT,material);
    assert(vertices_size % 3 == 0);
    assert(faces_size % 3 == 0);

    // Transform all vertices
    for(unsigned int i = 0; i < vertices_size/3; i++) {
	Vector v = getVertex(i);
	//v = mesh_matrix * v;
	v = scale * v;
	vertices[i*3+0] = float(v[0]);
	vertices[i*3+1] = float(v[1]);
	vertices[i*3+2] = float(v[2]);
    }
    
    bool hasMapcoords = map_coords.size() > 0;
    // Add the faces to this Mesh
    Vector verts[3]; 
    Vector2 uvs[3]; 
    for(unsigned int i = 0; i < faces_size/3; i++) {
	for(int j = 0; j < 3; j++) {
	    unsigned short idx = faces[i * 3 +j];
	    verts[j] = getVertex(idx);
	    if (hasMapcoords)
		uvs[j] = getUV(idx);
	}
	if (hasMapcoords) {
	    mesh->addTriangle(verts,uvs);
	} else {
	    mesh->addTriangle(verts[0],verts[1],verts[2]);
	}
    }
    this->addObject(mesh);

    vertices.clear();
    faces.clear();
    map_coords.clear();
    mesh_matrix = Matrix();
}

Vector ThreeDS::getVertex(const unsigned short index) const {
    unsigned short i = index * 3;
    return Vector(vertices[i+0],vertices[i+1],vertices[i+2]);
}

Vector2 ThreeDS::getUV(const unsigned short index) const {
    unsigned short i = index * 2;
    return Vector2(map_coords[i+0],map_coords[i+1]);
}

long filelength(int f) {
    struct stat buf;
    fstat(f, &buf);
    return buf.st_size;
}

unsigned int readUInt(FILE* file) {
    unsigned int dest = 0;
#ifdef WORDS_BIGENDIAN
    dest  =  (unsigned int) (fgetc(file) & 0xff);
    dest |= ((unsigned int) (fgetc(file) & 0xff)) << 0x08;
    dest |= ((unsigned int) (fgetc(file) & 0xff)) << 0x10;
    dest |= ((unsigned int) (fgetc(file) & 0xff)) << 0x18;
#else
    fread (&dest, sizeof(unsigned int), 1, file);
#endif
    return dest;
}

unsigned short readUShort(FILE* file) {
    unsigned short dest;
#ifdef WORDS_BIGENDIAN
    dest  =  (unsigned short) (fgetc(file) & 0xff);
    dest |= ((unsigned short) (fgetc(file) & 0xff)) << 0x08;
#else
    fread (&dest, sizeof(unsigned short), 1, file);
#endif    
    return dest;
}

unsigned char readUChar(FILE* file) {
    unsigned char dest;
    fread (&dest, sizeof(unsigned char), 1, file);
    return dest;
}

float readFloat(FILE* file) {
    float dest;
    fread (&dest, sizeof(float), 1, file);
#ifdef WORDS_BIGENDIAN    
    union {
	float f;
	unsigned char b[4];
    } dat1, dat2;
    dat1.f = dest;
    dat2.b[0] = dat1.b[3];
    dat2.b[1] = dat1.b[2];
    dat2.b[2] = dat1.b[1];
    dat2.b[3] = dat1.b[0];
    dest = dat2.f;
#endif
    return dest;
}

void ThreeDS::load3ds(const string& filename) {
    bool state_inside_object_block = false;
    bool state_inside_material_block = false;
    Material cur_material;
    RGB cur_color;
    char state_inside_color_type = ' ';

    int i; //Index variable

    FILE *l_file; //File pointer

    unsigned short l_chunk_id; //Chunk identifier
    unsigned int l_chunk_lenght; //Chunk lenght

    unsigned char l_char; //Char variable
    unsigned short l_qty; //Number of elements in each chunk

    unsigned short l_face_flags; //Flag that stores some face information

    l_file = fopen (filename.c_str(), "rb");
    if (l_file == NULL) {
        cout << "Error opening " << filename << endl;
        exit(EXIT_FAILURE);
    }

    while (ftell (l_file) < filelength (fileno (l_file))) //Loop to scan the whole file
	//while(!EOF)
    {
	//getch(); //Insert this command for debug (to wait for keypress for each chuck reading)

	l_chunk_id = readUShort(l_file);
	//fread (&l_chunk_id, sizeof(unsigned short), 1, l_file); //Read the chunk header
	//printf("ChunkID: %04x\n",l_chunk_id); 
	l_chunk_lenght = readUInt(l_file);
	//fread (&l_chunk_lenght, sizeof(unsigned int), 1, l_file); //Read the lenght of the chunk
	//printf("ChunkLenght: %x\n",l_chunk_lenght);

	switch (l_chunk_id)
	{
	    //----------------- MAIN3DS -----------------
	    // Description: Main chunk, contains all the other chunks
	    // Chunk ID: 4d4d 
	    // Chunk Lenght: 0 + sub chunks
	    //-------------------------------------------
	    case 0x4d4d: 
		break;    

		//----------------- EDIT3DS -----------------
		// Description: 3D Editor chunk, objects layout info 
		// Chunk ID: 3d3d (hex)
		// Chunk Lenght: 0 + sub chunks
		//-------------------------------------------
	    case 0x3d3d:
		break;

		//--------------- EDIT_OBJECT ---------------
		// Description: Object block, info for each object
		// Chunk ID: 4000 (hex)
		// Chunk Lenght: len(object name) + sub chunks
		//-------------------------------------------
	    case 0x4000: 
		if (state_inside_object_block) {
		    createMesh();
		}
		state_inside_object_block = true;
		mesh_matrix = Matrix();

		i = 0;
		do {
		    fread (&l_char, 1, 1, l_file);
		    //p_object->name[i]=l_char;
		    i++;
		} while (l_char != '\0' && i<20);
		break;

		//--------------- OBJ_TRIMESH ---------------
		// Description: Triangular mesh, contains chunks for 3d mesh info
		// Chunk ID: 4100 (hex)
		// Chunk Lenght: 0 + sub chunks
		//-------------------------------------------
	    case 0x4100:
		break;

		//--------------- TRI_VERTEXL ---------------
		// Description: Vertices list
		// Chunk ID: 4110 (hex)
		// Chunk Lenght: 1 x unsigned short (number of vertices) 
		//             + 3 x float (vertex coordinates) x (number of vertices)
		//             + sub chunks
		//-------------------------------------------
	    case 0x4110: 
		l_qty = readUShort(l_file);
		//fread (&l_qty, sizeof (unsigned short), 1, l_file);
		//p_object->vertices_qty = l_qty;
		printf("Number of vertices: %d\n",l_qty);
		for (i=0; i<l_qty; i++)
		{
		    for (int j = 0; j < 3; j++) {
			float val = readFloat(l_file);
			//fread(&val,sizeof(float),1,l_file);
			vertices.push_back(val);
		    }
		    /*
		    fread (&p_object->vertex[i].x, sizeof(float), 1, l_file);
		    printf("Vertices list x: %f\n",p_object->vertex[i].x);
		    fread (&p_object->vertex[i].y, sizeof(float), 1, l_file);
		    printf("Vertices list y: %f\n",p_object->vertex[i].y);
		    fread (&p_object->vertex[i].z, sizeof(float), 1, l_file);
		    printf("Vertices list z: %f\n",p_object->vertex[i].z);
		    */
		}
		break;

		//--------------- TRI_FACEL1 ----------------
		// Description: Polygons (faces) list
		// Chunk ID: 4120 (hex)
		// Chunk Lenght: 1 x unsigned short (number of polygons) 
		//             + 3 x unsigned short (polygon points) x (number of polygons)
		//             + sub chunks
		//-------------------------------------------
	    case 0x4120:
		l_qty = readUShort(l_file);
		//fread (&l_qty, sizeof (unsigned short), 1, l_file);
		//p_object->polygons_qty = l_qty;
		printf("Number of polygons: %d\n",l_qty); 
		for (i = 0; i < l_qty; i++)
		{
		    for (int j = 0; j < 3; j++) {
			unsigned short val = readUShort(l_file);
			faces.push_back(val);

		    }
		    /*
		    fread (&p_object->polygon[i].a, sizeof (unsigned short), 1, l_file);
		    printf("Polygon point a: %d\n",p_object->polygon[i].a);
		    fread (&p_object->polygon[i].b, sizeof (unsigned short), 1, l_file);
		    printf("Polygon point b: %d\n",p_object->polygon[i].b);
		    fread (&p_object->polygon[i].c, sizeof (unsigned short), 1, l_file);
		    printf("Polygon point c: %d\n",p_object->polygon[i].c);
		    */
		    l_face_flags = readUShort(l_file);
		    //fread (&l_face_flags, sizeof (unsigned short), 1, l_file);
		    //printf("Face flags: %x\n",l_face_flags);
		}
		break;

		//------------- TRI_MAPPINGCOORS ------------
		// Description: Vertices list
		// Chunk ID: 4140 (hex)
		// Chunk Lenght: 1 x unsigned short (number of mapping points) 
		//             + 2 x float (mapping coordinates) x (number of mapping points)
		//             + sub chunks
		//-------------------------------------------
	    case 0x4140:
		l_qty = readUShort(l_file);
		//fread (&l_qty, sizeof (unsigned short), 1, l_file);
		for (i=0; i < l_qty; i++)
		{
		    for (int j = 0; j < 2; j++) {
			float val = readFloat(l_file);
			//fread(&val, sizeof(float), 1, l_file);
			map_coords.push_back(val);
		    }
		    /*
		    fread (&p_object->mapcoord[i].u, sizeof (float), 1, l_file);
		    printf("Mapping list u: %f\n",p_object->mapcoord[i].u);
		    fread (&p_object->mapcoord[i].v, sizeof (float), 1, l_file);
		    printf("Mapping list v: %f\n",p_object->mapcoord[i].v);
		    */
		}
		break;
		
		//------------- TRI_MAPPINGCOORS ------------
		// Description: MESH MATRIX
		// Chunk ID: 4160 (hex)
		// Chunk Lenght: 9 x float a 3x3 rotation matrix 
		//             + 3 x float a translation vector
		//-------------------------------------------
	    case 0x4160:
		// Read 
		for (i=0; i < 3; i++) {
		    for (int j = 0; j < 3; j++) {
			float val = readFloat(l_file);
			//fread(&val, sizeof(float), 1, l_file);
			mesh_matrix.set(j,i,val);
		    }
		}
		for (i=0; i < 3; i++) {
		    float val = readFloat(l_file);
		    //fread(&val, sizeof(float), 1, l_file);
		    mesh_matrix.set(3,i,val);
		}
		//cout << mesh_matrix << endl;

		break;

	    case 0xAFFF:
		break;

	    case 0xA000:
                if (state_inside_material_block) {
		    materials.push_back(cur_material);
		}
		state_inside_material_block = true;
		state_inside_color_type = ' ';

		// TODO: Read material name
		fseek(l_file, l_chunk_lenght-6, SEEK_CUR); // Skip name
		break;
		
		// Ambient color
	    case 0xA010:
		state_inside_color_type = 'A';
		break;

		// Diffuse color
	    case 0xA020:
		state_inside_color_type = 'D';
		break;

		// Specular color
	    case 0xA030:
		state_inside_color_type = 'S';
		break;
		
		// A color found
	    case 0x0011:
		unsigned char rgb[3];
		fread(rgb,sizeof(rgb),1,l_file);
		cur_color = RGB(rgb[0],rgb[1],rgb[2]);
		cout << "24bit color found: " << cur_color << endl;
		switch (state_inside_color_type) {
		    case 'A':
			// TODO: Set ambient color
			break;
		    case 'D':
			cur_material.setDiffuseColor(cur_color);
			break;
		    case 'S':
			cur_material.setSpecularColor(cur_color);
			break;
		}
		break;
	    case 0x0010:
		float rgbf[3];
		rgbf[0] = readFloat(l_file);
		rgbf[1] = readFloat(l_file);
		rgbf[2] = readFloat(l_file);
		//fread(rgbf,sizeof(rgbf),1,l_file);
		cur_color = RGB(rgbf[0],rgbf[1],rgbf[2]);
		cout << "True color found: " << cur_color << endl;
		switch (state_inside_color_type) {
		    case 'A':
			// TODO: Set ambient color
			break;
		    case 'D':
			cur_material.setDiffuseColor(cur_color);
			break;
		    case 'S':
			cur_material.setSpecularColor(cur_color);
			break;
		}
		break;



		//----------- Skip unknown chunks ------------
		//We need to skip all the chunks that currently we don't use
		//We use the chunk lenght information to set the file pointer
		//to the same level next chunk
		//-------------------------------------------
	    default:
		fseek(l_file, l_chunk_lenght-6, SEEK_CUR);
	} 
    }
    createMesh();
    fclose (l_file); // Closes the file stream
 //   return (1); // Returns ok
}

/*
The following are the most important chunks in a 3ds file. Please note the hierarchy among the various elements:

MAIN CHUNK 0x4D4D
   3D EDITOR CHUNK 0x3D3D
      OBJECT BLOCK 0x4000
         TRIANGULAR MESH 0x4100
            VERTICES LIST 0x4110
            FACES DESCRIPTION 0x4120
               FACES MATERIAL 0x4130
            MAPPING COORDINATES LIST 0x4140
               SMOOTHING GROUP LIST 0x4150
            LOCAL COORDINATES SYSTEM 0x4160
         LIGHT 0x4600
            SPOTLIGHT 0x4610
         CAMERA 0x4700
      MATERIAL BLOCK 0xAFFF
         MATERIAL NAME 0xA000
         AMBIENT COLOR 0xA010
         DIFFUSE COLOR 0xA020
         SPECULAR COLOR 0xA030
         TEXTURE MAP 1 0xA200
         BUMP MAP 0xA230
         REFLECTION MAP 0xA220
         [SUB CHUNKS FOR EACH MAP]
            MAPPING FILENAME 0xA300
            MAPPING PARAMETERS 0xA351
      KEYFRAMER CHUNK 0xB000
         MESH INFORMATION BLOCK 0xB002
         SPOT LIGHT INFORMATION BLOCK 0xB007
         FRAMES (START AND END) 0xB008
            OBJECT NAME 0xB010
            OBJECT PIVOT POINT 0xB013
            POSITION TRACK 0xB020
            ROTATION TRACK 0xB021
            SCALE TRACK 0xB022
            HIERARCHY POSITION 0xB030

*/	    
