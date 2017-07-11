#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "projection.h"

float focalLength;
int viewWidth, viewHeight;
int pixelAngle[MAXVIEWWIDTH] = { 0 };

int focalTx, focalTy;

static void calcProjection(float focal)
{
    int halfView;
    float faceDist;

    focalLength = focal;
    faceDist    = focal + MINDIST;
    halfView    = viewWidth / 2; // half view in pixels

    /* calculate the angle offset from view angle of each pixel's ray */
    for (int i = 0; i < halfView; ++i) {
        float  tang   = i * VIEWGLOBAL / viewWidth / faceDist;
        float  angle  = (float)atan(tang);
        int    intang = RAD2DEG(angle);

        pixelAngle[halfView - 1 - i] =  intang;
        pixelAngle[halfView + i]     = -intang;
    }
}

static void setViewSize(int width, int height)
{
    assert(width % 16 == 0);  // must be divisable by 16
    assert(height % 1 == 0);  // must be even

    viewWidth  = width;
    viewHeight = height;

    calcProjection(FOCALLENGTH);
}

void newViewSize(int width)
{
    assert(width > 1 && width <= 20);
    setViewSize(width * 16, (int)(width * 16 * HEIGHTRATIO));
}
