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
                    -- Lump start (LatchPics)
                    | KNIFEPIC
                    | GUNPIC
                    | MACHINEGUNPIC
                    | GATLINGGUNPIC
                    | NOKEYPIC
                    | GOLDKEYPIC
                    | SILVERKEYPIC
                    | N_BLANKPIC
                    | N_0PIC
                    | N_1PIC
                    | N_2PIC
                    | N_3PIC
                    | N_4PIC
                    | N_5PIC
                    | N_6PIC
                    | N_7PIC
                    | N_8PIC
                    | N_9PIC
                    | FACE1APIC
                    | FACE1BPIC
                    | FACE1CPIC
                    | FACE2APIC
                    | FACE2BPIC
                    | FACE2CPIC
                    | FACE3APIC
                    | FACE3BPIC
                    | FACE3CPIC
                    | FACE4APIC
                    | FACE4BPIC
                    | FACE4CPIC
                    | FACE5APIC
                    | FACE5BPIC
                    | FACE5CPIC
                    | FACE6APIC
                    | FACE6BPIC
                    | FACE6CPIC
                    | FACE7APIC
                    | FACE7BPIC
                    | FACE7CPIC
                    | FACE8APIC
                    | GOTGATLINGPIC
                    | MUTANTBJPIC
                    | PAUSEDPIC
                    | GETPSYCHEDPIC
                    deriving (Enum)
