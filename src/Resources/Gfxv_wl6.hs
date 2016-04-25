module Resources.Gfxv_wl6 where

data GrChunks = STRUCTPIC
              | STARTFONT
              deriving (Show, Enum)

numChunks :: Int
numChunks = 149

numPics :: Int
numPics = 132

data ReadmeLump = H_BJPIC
                | H_CASTLEPIC
                | H_BLAZEPIC
                | H_TOPWINDOWPIC
                | H_LEFTWINDOWPIC
                | H_RIGHTWINDOWPIC
                | H_BOTTOMINFOPIC
                deriving (Enum)

data ControlsLump   = C_OPTIONSPIC
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
                    deriving (Enum)

{- LevelEnd
        L_GUYPIC,                            // 43
        L_COLONPIC,                          // 44
        L_NUM0PIC,                           // 45
        L_NUM1PIC,                           // 46
        L_NUM2PIC,                           // 47
        L_NUM3PIC,                           // 48
        L_NUM4PIC,                           // 49
        L_NUM5PIC,                           // 50
        L_NUM6PIC,                           // 51
        L_NUM7PIC,                           // 52
        L_NUM8PIC,                           // 53
        L_NUM9PIC,                           // 54
        L_PERCENTPIC,                        // 55
        L_APIC,                              // 56
        L_BPIC,                              // 57
        L_CPIC,                              // 58
        L_DPIC,                              // 59
        L_EPIC,                              // 60
        L_FPIC,                              // 61
        L_GPIC,                              // 62
        L_HPIC,                              // 63
        L_IPIC,                              // 64
        L_JPIC,                              // 65
        L_KPIC,                              // 66
        L_LPIC,                              // 67
        L_MPIC,                              // 68
        L_NPIC,                              // 69
        L_OPIC,                              // 70
        L_PPIC,                              // 71
        L_QPIC,                              // 72
        L_RPIC,                              // 73
        L_SPIC,                              // 74
        L_TPIC,                              // 75
        L_UPIC,                              // 76
        L_VPIC,                              // 77
        L_WPIC,                              // 78
        L_XPIC,                              // 79
        L_YPIC,                              // 80
        L_ZPIC,                              // 81
        L_EXPOINTPIC,                        // 82
        L_APOSTROPHEPIC,                     // 83
        L_GUY2PIC,                           // 84
        L_BJWINSPIC,                         // 85
-}

data LogoLumps  = STATUSBARPIC
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