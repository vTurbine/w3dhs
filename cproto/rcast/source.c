#include <stdio.h>
#include <stdlib.h>

#include <direct.h>

#include "projection.h"
#include "refresh.h"

#include "settings.h"

// actor data
actor_t p = {
    .angle = 0,
    .x     = 2.5f,
    .y     = 2.5f,
};

// test map
unsigned char tileMap[MAPSIZE][MAPSIZE] = {
    { 1, 1, 1, 1, },
    { 1, 0, 0, 1, },
    { 1, 0, 0, 1, },
    { 1, 1, 1, 1, },
};

#define SCALE   (25U)

static dump_data(FILE *f)
{
    fprintf_s(f, "P2\n");
    fprintf_s(f, "# View size = {%d x %d}, angle = %.3f (deg)\n",
        viewWidth, viewHeight, midAngle / 10.F);
    fprintf_s(f, "# FOV: %.2f\n",
        (abs(pixelAngle[0]) + abs(pixelAngle[viewWidth - 1])) / 10.F);
    {
        // tile hit list
        fprintf_s(f, "# Hit list = {");

        int prev = HIT_EMPTY;

        for (int i = 0; i < viewWidth; ++i)
        {
            if (hitList[i] == HIT_EMPTY)
            {
                break;
            }

            int curr = hitList[i];
            if (curr != prev)
            {
                fprintf_s(f, i ? ", %d" : "%d", curr & 0x7F);
                prev = curr;
            }
        }

        fprintf_s(f, "}\n");
    }

    {
        int isFst = 1;

        // spots visited
        fprintf_s(f, "# Spots visited = {");

        for (int i = 0; i < MAPSIZE * MAPSIZE; ++i)
        {
            if (((char *)spotVis)[i] == 1)
            {
                fprintf_s(f, isFst ? "%d" : ", %d", i);
                isFst = 0;
            }
        }

        fprintf_s(f, "}\n");
    }

    fprintf_s(f, "%d %d\n", viewWidth, viewHeight);
    fprintf_s(f, "15\n"); // max color

    // make walls
    for (int h = 0; h < viewHeight; h++)
    {
        for (int w = 0; w < viewWidth; w++)
        {
            char *fmt = w ? " %2d" : "%2d";
            int wh = MAGNITUDE((float)fabs(SCALE / wallHeight[w]));
            char color = IS_HORIZ(hitList[w]) ? 10 : 15;

            fprintf(f, fmt, (  (h > (viewHeight - wh) / 2)
                            && (h < (viewHeight + wh) / 2)) ? color : 5);
        }
        fprintf_s(f, "\n");
    }
}

void main()
{
    unsigned char fname[_MAX_PATH] = { 0 };
    FILE *f;

    _mkdir("out");

    // set viewport
    newViewSize(CFG_VIEWSIZE);

    printf("BJ is looking around. Please stand by...\n");

    // juat look around
    for (p.angle = 0; p.angle <= ANGLES; p.angle++)
    {
        sprintf_s(fname, sizeof (fname), "out\\%03dDEG.PGM", p.angle);
        fopen_s(&f, fname, "wb");
        
        // render view from current angle
        wallRefresh();
        // dump render data into file
        dump_data(f);

        fclose(f);
    }
}