#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "projection.h"
#include "refresh.h"

#include <inttypes.h>

int viewAngle;
int midAngle; // center of view area

float viewX, viewY;

int viewTx, viewTy;
int focalTx, focalTy;

float xPartialUp, xPartialDown;
float yPartialUp, yPartialDown;

int pixX = 0;
int lastSide = -1;

float viewCos, viewSin;

#define DEG90   ( 900)
#define DEG180  (1800)
#define DEG270  (2700)
#define DEG360  (3600)

float wallHeight[MAXVIEWWIDTH] = { .0F };
unsigned char hitList[MAXVIEWWIDTH] = { HIT_EMPTY };
unsigned char spotVis[MAPSIZE][MAPSIZE] = { 0 };

static int lessOrEqual(int x, int y)
{
    return (x <= y) ? 1 : 0;
}

static int greaterOrEqual(int x, int y)
{
    return (x >= y) ? 1 : 0;
}

float xIntercept = 0.0f, yIntercept = 0.0f;
int xTileStep = 0, yTileStep = 0;

static float calcHeight(void)
{
    float gx = xIntercept - viewX;
    float gxt = gx * viewCos;

    float gy = yIntercept - viewY;
    float gyt = gy * viewSin;

    float nx = gxt - gyt;

    if (nx < MINDIST)
    {
        nx = MINDIST;
    }

    return nx;
}

static hitVertWall(void)
{
    if (xTileStep == -1)
    {
        xIntercept += TILEGLOBAL;
    }
    wallHeight[pixX] = calcHeight();
}

static hitHorizWall(void)
{
    if (yTileStep == -1)
    {
        yIntercept += TILEGLOBAL;
    }
    wallHeight[pixX] = calcHeight();
}

static void asmRefresh(void)
{
    int currAngle; // angle of the ray through pixx
    int (*checkHoriz)(int, int) = 0;
    int (*checkVert) (int, int) = 0;
    float xStep, yStep;
    float xPartial, yPartial;

    pixX = 0;

    do {
        currAngle = midAngle + pixelAngle[pixX];

        if (currAngle < 0) {
            // -90 ~ -1
            currAngle += FINEANGLES;
        }

        if (currAngle >= DEG360) {
            // 360 ~ 449
            currAngle -= FINEANGLES;
        }

        if (currAngle >= 0 && currAngle < DEG90) {
            // 0 ~ 89
            xTileStep   =  1;
            yTileStep   = -1;
            checkHoriz  = greaterOrEqual;
            checkVert   = lessOrEqual;
            xStep       =  (float)tan(DEG2RAD(DEG90 - 1 - currAngle + 0.5));
            yStep       = -(float)tan(DEG2RAD(            currAngle + 0.5));
            xPartial    = xPartialUp;
            yPartial    = yPartialDown;
        }
        else if (currAngle >= DEG90 && currAngle < DEG180) {
            // 90 ~ 179
            xTileStep   = -1;
            yTileStep   = -1;
            checkHoriz  = lessOrEqual;
            checkVert   = lessOrEqual;
            xStep       = -(float)tan(DEG2RAD(currAngle + 0.5 - DEG90));
            yStep       = -(float)tan(DEG2RAD(DEG180 - 1 - currAngle + 0.5));
            xPartial    = xPartialDown;
            yPartial    = yPartialDown;
        }
        else if (currAngle >= DEG180 && currAngle < DEG270) {
            // 180 ~ 269
            xTileStep   = -1;
            yTileStep   =  1;
            checkHoriz  = lessOrEqual;
            checkVert   = greaterOrEqual;
            xStep       = -(float)tan(DEG2RAD(DEG270 - 1 - currAngle + 0.5));
            yStep       =  (float)tan(DEG2RAD(currAngle + 0.5 - DEG180));
            xPartial    = xPartialDown;
            yPartial    = yPartialUp;
        }
        else if (currAngle >= DEG270 && currAngle < DEG360) {
            // 270 ~ 359
            xTileStep   = 1;
            yTileStep   = 1;
            checkHoriz  = greaterOrEqual;
            checkVert   = greaterOrEqual;
            xStep       =  (float)tan(DEG2RAD(currAngle + 0.5 - DEG270));
            yStep       =  (float)tan(DEG2RAD(DEG360 - 1 - currAngle + 0.5));
            xPartial    = xPartialUp;
            yPartial    = yPartialUp;
        }
        else {
            assert(0);
        }

        assert(checkHoriz);
        assert(checkVert);

        /* init vars */
        xIntercept = viewX + yPartial * xStep;
        yIntercept = viewY + xPartial * yStep;

        int xIntTile = MAGNITUDE(xIntercept);
        int yIntTile = MAGNITUDE(yIntercept);

        int xTile = focalTx + xTileStep;
        int yTile = focalTy + yTileStep;

        assert(xTile < MAPSIZE);
        assert(yTile < MAPSIZE);

        int xSpot = xTile * MAPSIZE + yIntTile; // ACHTUNG: column-major arrangement detected! X selects column and Y offset in it
        int ySpot = xIntTile * MAPSIZE + yTile;

        int hit = 0;

        do {
            /* vertical wall check */
            while (!checkVert(MAGNITUDE(yIntercept), yTile)) { // while yIcept not reached yTile

                assert(xSpot >= 0 && xSpot < MAPSIZE * MAPSIZE);
                
                unsigned char tile = ((char *)tileMap)[xSpot];

                if (tile != 0) {

                    if (IS_DOOR(tile)) {
                        // ...
                    }
                    else { // wall
                        xIntercept  = xTile * 1.F;
                        yTile       = MAGNITUDE(yIntercept);
                        hitVertWall();
                    }

                    hit = 1;
                    hitList[pixX] = xSpot;
                    break;
                }
                else {
                    // we reached the neigbor `xTile` and go further

                    ((char *)spotVis)[xSpot] = 1;
                    xTile       += xTileStep;
                    yIntercept  += yStep;
                    xSpot = (xTile * MAPSIZE) + MAGNITUDE(yIntercept);
                }
            }

            if (hit) {
                break;
            }

            /* horizontal wall check */
            while (!checkHoriz(MAGNITUDE(xIntercept), xTile)) // while xIcept not reached xTile
            {
                assert(ySpot >= 0 && ySpot < MAPSIZE * MAPSIZE);

                unsigned char tile = ((char *)tileMap)[ySpot];

                if (tile != 0) {

                    if (IS_DOOR(tile)) {
                        // ...
                    }
                    else { // wall
                        yIntercept  = yTile * 1.F;
                        xTile       = MAGNITUDE(xIntercept);
                        hitHorizWall();
                    }

                    hit = 1;
                    hitList[pixX] = ySpot | HIT_HORIZ;
                    break;
                }
                else {
                    // we reached the neigbor `yTile` and go further

                    ((char *)spotVis)[ySpot] = 1;
                    yTile += yTileStep;
                    xIntercept += xStep;
                    ySpot = (MAGNITUDE(xIntercept) * MAPSIZE) + yTile;
                }
            }

            // pretty deadlock condition (out of map with no intersection)
            assert(xTile < MAPSIZE && yTile < MAPSIZE);

        } while (hit == 0);
    } while (++pixX < viewWidth);
}

void wallRefresh()
{
    memset(hitList, HIT_EMPTY, sizeof (hitList));
    memset(spotVis, 0, sizeof (spotVis));

    viewAngle = p.angle;
    midAngle  = viewAngle * (FINEANGLES / ANGLES); // the fineangle 10x times more precise

    viewCos = (float)cos(DEG2RAD(midAngle));
    viewSin = (float)sin(DEG2RAD(midAngle));

    viewX   = p.x - focalLength * viewCos;
    viewY   = p.y + focalLength * viewSin;

    focalTx = MAGNITUDE(viewX);
    focalTy = MAGNITUDE(viewY);

    xPartialDown = viewX - focalTx; // WORLD -> TILE normalization
    xPartialUp   = TILEGLOBAL - xPartialDown;
    yPartialDown = viewY - focalTy;
    yPartialUp   = TILEGLOBAL - yPartialDown;

    lastSide = -1; // the first pixel is on a new wall

    asmRefresh();
}
