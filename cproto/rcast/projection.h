#ifndef PROJECTION_H
#define PROJECTION_H

#include "settings.h"

extern float focalLength;
extern int viewWidth, viewHeight;
extern int pixelAngle[MAXVIEWWIDTH];
extern int focalTx, focalTy;

void newViewSize(int width);

#endif // PROJECTION_H