
#ifndef MATERIALS_H
#define MATERIALS_H

#include "materials/material.h"

#define MATERIAL_SHINY_RED Material(RGB(1.0,0.2,0.2),0.75,RGB(1.0,1.0,1.0),0.20,30)
#define MATERIAL_SHINY_GREEN Material(RGB(0.2,1.0,0.2),0.75,RGB(1.0,1.0,1.0),0.75,30)
#define MATERIAL_SHINY_BLUE Material(RGB(0.2,0.2,1.0),0.75,RGB(1.0,1.0,1.0),0.20,30)
#define MATERIAL_DULL_BLUE Material(RGB(0.2,0.2,1.0),1.00,RGB(1.0,1.0,1.0),0.0,30)
#define MATERIAL_PORCELAIN Material(RGB(0.9,0.9,0.9),0.60,RGB(0.5,0.5,1.0),0.40,50)
#define MATERIAL_CHROME Material(RGB(0.8,0.8,0.8),0.2,RGB(1.0,1.0,1.0),0.80,50)

#define MATERIAL_SHINY(rgb) Material(rgb,0.75,RGB(1.0,1.0,1.0),0.30,30)

#endif
