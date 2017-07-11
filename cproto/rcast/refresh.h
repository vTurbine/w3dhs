#ifndef REFRESH_H
#define REFRESH_H

#include <math.h>

#include "settings.h"

#define HIT_EMPTY           (0xFF)
#define HIT_HORIZ           (0x80)
#define IS_HORIZ(x)         (!!((x) & HIT_HORIZ))

#define MAGNITUDE(x)        ((int)floor(x))

extern int midAngle;
extern float wallHeight[MAXVIEWWIDTH];
extern unsigned char hitList[MAXVIEWWIDTH];

void wallRefresh(void);

#endif // REFRESH_H