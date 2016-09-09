module Resources.Gfxv_wl6 where

data GrChunks = STRUCTPIC
              | STARTFONT
              deriving (Show, Enum)

numChunks :: Int
numChunks = 149

numPics :: Int
numPics = 132

data GraphicNums =  DUMMY0
                    | DUMMY1
                    | DUMMY2
                    -- Lump start (Readme)
                    | H_BJPIC -- =3
                    | H_CASTLEPIC
                    | H_BLAZEPIC
                    | H_TOPWINDOWPIC
                    | H_LEFTWINDOWPIC
                    | H_RIGHTWINDOWPIC
                    | H_BOTTOMINFOPIC
                    -- Lump start
                    | C_OPTIONSPIC
                    | C_CURSOR1PIC
                    | C_CURSOR2PIC
                    | C_NOTSELECTEDPIC
                    | C_SELECTEDPIC
                    | C_FXTITLEPIC
                    | C_DIGITITLEPIC
                    | C_MUSICTITLEPIC
                    | C_MOUSELBACKPIC
                    | C_BABYMODEPIC
                    | C_EASYPIC
                    | C_NORMALPIC
                    | C_HARDPIC
                    | C_LOADSAVEDISKPIC
                    | C_DISKLOADING1PIC
                    | C_DISKLOADING2PIC
                    | C_CONTROLPIC
                    | C_CUSTOMIZEPIC
                    | C_LOADGAMEPIC
                    | C_SAVEGAMEPIC
                    | C_EPISODE1PIC
                    | C_EPISODE2PIC
                    | C_EPISODE3PIC
                    | C_EPISODE4PIC
                    | C_EPISODE5PIC
                    | C_EPISODE6PIC
                    | C_CODEPIC
                    | C_TIMECODEPIC
                    | C_LEVELPIC
                    | C_NAMEPIC
                    | C_SCOREPIC
                    | C_JOY1PIC
                    | C_JOY2PIC
                    -- Lump start (LevelEnd)
                    | L_GUYPIC
                    | L_COLONPIC
                    | L_NUM0PIC
                    | L_NUM1PIC
                    | L_NUM2PIC
                    | L_NUM3PIC
                    | L_NUM4PIC
                    | L_NUM5PIC
                    | L_NUM6PIC
                    | L_NUM7PIC
                    | L_NUM8PIC
                    | L_NUM9PIC
                    | L_PERCENTPIC
                    | L_APIC
                    | L_BPIC
                    | L_CPIC
                    | L_DPIC
                    | L_EPIC
                    | L_FPIC
                    | L_GPIC
                    | L_HPIC
                    | L_IPIC
                    | L_JPIC
                    | L_KPIC
                    | L_LPIC
                    | L_MPIC
                    | L_NPIC
                    | L_OPIC
                    | L_PPIC
                    | L_QPIC
                    | L_RPIC
                    | L_SPIC
                    | L_TPIC
                    | L_UPIC
                    | L_VPIC
                    | L_WPIC
                    | L_XPIC
                    | L_YPIC
                    | L_ZPIC
                    | L_EXPOINTPIC
                    | L_APOSTROPHEPIC
                    | L_GUY2PIC
                    | L_BJWINSPIC
                    -- Lump start (Logo)
                    | STATUSBARPIC
                    | TITLEPIC
                    | PG13PIC
                    | CREDITSPIC
                    | HIGHSCORESPIC
                    deriving (Enum)

{- LatchPics
        KNIFEPIC,                            // 91
        GUNPIC,                              // 92
        MACHINEGUNPIC,                       // 93
        GATLINGGUNPIC,                       // 94
        NOKEYPIC,                            // 95
        GOLDKEYPIC,                          // 96
        SILVERKEYPIC,                        // 97
        N_BLANKPIC,                          // 98
        N_0PIC,                              // 99
        N_1PIC,                              // 100
        N_2PIC,                              // 101
        N_3PIC,                              // 102
        N_4PIC,                              // 103
        N_5PIC,                              // 104
        N_6PIC,                              // 105
        N_7PIC,                              // 106
        N_8PIC,                              // 107
        N_9PIC,                              // 108
        FACE1APIC,                           // 109
        FACE1BPIC,                           // 110
        FACE1CPIC,                           // 111
        FACE2APIC,                           // 112
        FACE2BPIC,                           // 113
        FACE2CPIC,                           // 114
        FACE3APIC,                           // 115
        FACE3BPIC,                           // 116
        FACE3CPIC,                           // 117
        FACE4APIC,                           // 118
        FACE4BPIC,                           // 119
        FACE4CPIC,                           // 120
        FACE5APIC,                           // 121
        FACE5BPIC,                           // 122
        FACE5CPIC,                           // 123
        FACE6APIC,                           // 124
        FACE6BPIC,                           // 125
        FACE6CPIC,                           // 126
        FACE7APIC,                           // 127
        FACE7BPIC,                           // 128
        FACE7CPIC,                           // 129
        FACE8APIC,                           // 130
        GOTGATLINGPIC,                       // 131
        MUTANTBJPIC,                         // 132
        PAUSEDPIC,                           // 133
        GETPSYCHEDPIC,                       // 134
-}