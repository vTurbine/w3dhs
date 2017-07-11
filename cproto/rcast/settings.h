#ifndef SETTINGS_H
#define SETTINGS_H

#define MAXVIEWWIDTH               (320U)
#define CFG_VIEWSIZE               ( 15U)   // can be changed from 1 to 20
#define HEIGHTRATIO                ( .5F)
#define MAPSIZE                    (  4U)   // 64 IRL

#define FINEANGLES                 (3600) 
#define ANGLES                     ( 360)
#define FOCALLENGTH                (.33F)
#define MINDIST                    (.34F)

#define GLOBAL1		                (1.F)
#define TILEGLOBAL                GLOBAL1
#define VIEWGLOBAL                 (10.F) // globals visable flush to wall

#define PI                      (3.1415F)

#define RAD2INT                 ((float)FINEANGLES / 2 / PI)

#define DEG2RAD(x)              ((x) / RAD2INT)
#define RAD2DEG(x)              ((int)((x) * RAD2INT))

#define DOOR_MASK               (0x80)
#define IS_DOOR(x)              (!!((x) & DOOR_MASK))

typedef struct {
    int     angle;  // in degress
    float   x, y;   /* fixed */
} actor_t;

/* Globals */
extern unsigned char tileMap[MAPSIZE][MAPSIZE];
extern unsigned char spotVis[MAPSIZE][MAPSIZE];
extern actor_t p;

#endif // SETTINGS_H