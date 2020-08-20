/*
 * << Haru Free PDF Library >> -- hpdf_fontdef_kr.c
 *
 * URL: http://libharu.org
 *
 * Copyright (c) 1999-2006 Takeshi Kanno <takeshi_kanno@est.hi-ho.ne.jp>
 * Copyright (c) 2007-2009 Antony Dovgal <tony@daylessday.org>
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.
 * It is provided "as is" without express or implied warranty.
 *
 */

#include "hpdf_conf.h"
#include "hpdf_utils.h"
#include "hpdf.h"

/*----------------------------------------------------------------------------*/

static const HPDF_CID_Width DotumChe_W_ARRAY[] = {
    {1, 500},
    {2, 500},
    {3, 500},
    {4, 500},
    {5, 500},
    {6, 500},
    {7, 500},
    {8, 500},
    {9, 500},
    {10, 500},
    {11, 500},
    {12, 500},
    {13, 500},
    {14, 500},
    {15, 500},
    {16, 500},
    {17, 500},
    {18, 500},
    {19, 500},
    {20, 500},
    {21, 500},
    {22, 500},
    {23, 500},
    {24, 500},
    {25, 500},
    {26, 500},
    {27, 500},
    {28, 500},
    {29, 500},
    {30, 500},
    {31, 500},
    {32, 500},
    {33, 500},
    {34, 500},
    {35, 500},
    {36, 500},
    {37, 500},
    {38, 500},
    {39, 500},
    {40, 500},
    {41, 500},
    {42, 500},
    {43, 500},
    {44, 500},
    {45, 500},
    {46, 500},
    {47, 500},
    {48, 500},
    {49, 500},
    {50, 500},
    {51, 500},
    {52, 500},
    {53, 500},
    {54, 500},
    {55, 500},
    {56, 500},
    {57, 500},
    {58, 500},
    {59, 500},
    {60, 500},
    {61, 500},
    {62, 500},
    {63, 500},
    {64, 500},
    {65, 500},
    {66, 500},
    {67, 500},
    {68, 500},
    {69, 500},
    {70, 500},
    {71, 500},
    {72, 500},
    {73, 500},
    {74, 500},
    {75, 500},
    {76, 500},
    {77, 500},
    {78, 500},
    {79, 500},
    {80, 500},
    {81, 500},
    {82, 500},
    {83, 500},
    {84, 500},
    {85, 500},
    {86, 500},
    {87, 500},
    {88, 500},
    {89, 500},
    {90, 500},
    {91, 500},
    {92, 500},
    {93, 500},
    {94, 500},
    {95, 500},
    {8094, 500},
    {8095, 500},
    {8096, 500},
    {8097, 500},
    {8098, 500},
    {8099, 500},
    {8100, 500},
    {8101, 500},
    {8102, 500},
    {8103, 500},
    {8104, 500},
    {8105, 500},
    {8106, 500},
    {8107, 500},
    {8108, 500},
    {8109, 500},
    {8110, 500},
    {8111, 500},
    {8112, 500},
    {8113, 500},
    {8114, 500},
    {8115, 500},
    {8116, 500},
    {8117, 500},
    {8118, 500},
    {8119, 500},
    {8120, 500},
    {8121, 500},
    {8122, 500},
    {8123, 500},
    {8124, 500},
    {8125, 500},
    {8126, 500},
    {8127, 500},
    {8128, 500},
    {8129, 500},
    {8130, 500},
    {8131, 500},
    {8132, 500},
    {8133, 500},
    {8134, 500},
    {8135, 500},
    {8136, 500},
    {8137, 500},
    {8138, 500},
    {8139, 500},
    {8140, 500},
    {8141, 500},
    {8142, 500},
    {8143, 500},
    {8144, 500},
    {8145, 500},
    {8146, 500},
    {8147, 500},
    {8148, 500},
    {8149, 500},
    {8150, 500},
    {8151, 500},
    {8152, 500},
    {8153, 500},
    {8154, 500},
    {8155, 500},
    {8156, 500},
    {8157, 500},
    {8158, 500},
    {8159, 500},
    {8160, 500},
    {8161, 500},
    {8162, 500},
    {8163, 500},
    {8164, 500},
    {8165, 500},
    {8166, 500},
    {8167, 500},
    {8168, 500},
    {8169, 500},
    {8170, 500},
    {8171, 500},
    {8172, 500},
    {8173, 500},
    {8174, 500},
    {8175, 500},
    {8176, 500},
    {8177, 500},
    {8178, 500},
    {8179, 500},
    {8180, 500},
    {8181, 500},
    {8182, 500},
    {8183, 500},
    {8184, 500},
    {8185, 500},
    {8186, 500},
    {8187, 500},
    {8188, 500},
    {0xFFFF, 0}
};


static const HPDF_CID_Width Dotum_W_ARRAY[] = {
    {1, 333},
    {2, 333},
    {3, 416},
    {4, 625},
    {5, 500},
    {6, 916},
    {7, 708},
    {8, 291},
    {9, 375},
    {10, 375},
    {11, 583},
    {12, 582},
    {13, 375},
    {14, 589},
    {15, 375},
    {16, 416},
    {17, 583},
    {18, 583},
    {19, 583},
    {20, 583},
    {21, 583},
    {22, 583},
    {23, 583},
    {24, 583},
    {25, 583},
    {26, 583},
    {27, 339},
    {28, 339},
    {29, 625},
    {30, 583},
    {31, 625},
    {32, 583},
    {34, 666},
    {35, 679},
    {36, 720},
    {37, 724},
    {38, 628},
    {39, 599},
    {40, 750},
    {41, 722},
    {42, 257},
    {43, 484},
    {44, 656},
    {45, 541},
    {46, 808},
    {47, 697},
    {48, 750},
    {49, 642},
    {50, 750},
    {51, 667},
    {52, 638},
    {53, 583},
    {54, 724},
    {55, 601},
    {56, 892},
    {57, 603},
    {58, 601},
    {59, 603},
    {60, 500},
    {61, 958},
    {62, 500},
    {63, 625},
    {64, 500},
    {65, 332},
    {66, 583},
    {67, 599},
    {68, 558},
    {69, 603},
    {70, 558},
    {71, 343},
    {72, 597},
    {73, 560},
    {74, 228},
    {75, 228},
    {76, 509},
    {77, 230},
    {78, 916},
    {79, 568},
    {80, 599},
    {81, 601},
    {82, 603},
    {83, 320},
    {84, 515},
    {85, 312},
    {86, 554},
    {87, 474},
    {88, 724},
    {89, 478},
    {90, 480},
    {91, 482},
    {92, 500},
    {93, 500},
    {94, 500},
    {95, 791},
    {104, 332},
    {107, 500},
    {109, 500},
    {110, 687},
    {114, 291},
    {115, 291},
    {116, 458},
    {117, 458},
    {130, 833},
    {131, 833},
    {132, 833},
    {133, 833},
    {134, 833},
    {135, 833},
    {136, 911},
    {138, 405},
    {139, 335},
    {140, 563},
    {146, 625},
    {147, 625},
    {151, 562},
    {153, 833},
    {155, 562},
    {160, 937},
    {164, 889},
    {165, 889},
    {167, 914},
    {169, 914},
    {172, 750},
    {173, 750},
    {178, 500},
    {182, 375},
    {191, 718},
    {199, 333},
    {201, 437},
    {202, 375},
    {203, 437},
    {204, 312},
    {205, 317},
    {206, 333},
    {207, 312},
    {208, 416},
    {209, 583},
    {212, 812},
    {213, 687},
    {214, 562},
    {222, 750},
    {224, 792},
    {226, 843},
    {244, 562},
    {245, 562},
    {246, 562},
    {247, 750},
    {254, 687},
    {471, 666},
    {472, 708},
    {473, 594},
    {474, 705},
    {475, 666},
    {476, 625},
    {477, 750},
    {478, 791},
    {479, 291},
    {480, 708},
    {481, 666},
    {482, 875},
    {483, 750},
    {484, 750},
    {485, 791},
    {486, 702},
    {487, 666},
    {488, 674},
    {489, 583},
    {490, 625},
    {491, 770},
    {492, 625},
    {493, 794},
    {494, 750},
    {495, 625},
    {496, 563},
    {497, 562},
    {498, 571},
    {499, 562},
    {500, 500},
    {501, 625},
    {502, 687},
    {503, 375},
    {504, 562},
    {505, 687},
    {506, 567},
    {507, 500},
    {508, 500},
    {509, 625},
    {510, 687},
    {511, 625},
    {512, 625},
    {513, 583},
    {514, 583},
    {515, 625},
    {516, 583},
    {517, 731},
    {518, 750},
    {590, 500},
    {643, 750},
    {666, 937},
    {667, 751},
    {668, 403},
    {669, 750},
    {670, 750},
    {671, 583},
    {672, 583},
    {673, 791},
    {674, 934},
    {675, 375},
    {676, 666},
    {677, 583},
    {678, 750},
    {748, 500},
    {751, 500},
    {752, 500},
    {753, 562},
    {754, 561},
    {755, 562},
    {756, 562},
    {757, 812},
    {758, 625},
    {759, 625},
    {760, 583},
    {761, 250},
    {762, 500},
    {763, 562},
    {764, 312},
    {765, 250},
    {766, 625},
    {767, 875},
    {768, 625},
    {769, 625},
    {770, 333},
    {771, 625},
    {772, 687},
    {842, 375},
    {843, 322},
    {844, 322},
    {846, 500},
    {1020, 666},
    {1021, 708},
    {1022, 708},
    {1023, 594},
    {1024, 812},
    {1025, 666},
    {1026, 666},
    {1027, 937},
    {1028, 583},
    {1029, 750},
    {1030, 750},
    {1031, 708},
    {1032, 714},
    {1033, 875},
    {1034, 750},
    {1035, 791},
    {1036, 750},
    {1037, 666},
    {1038, 750},
    {1039, 583},
    {1040, 500},
    {1041, 770},
    {1042, 625},
    {1043, 742},
    {1044, 583},
    {1045, 966},
    {1046, 966},
    {1047, 787},
    {1048, 825},
    {1049, 666},
    {1050, 738},
    {1051, 991},
    {1052, 708},
    {1053, 583},
    {1054, 583},
    {1055, 610},
    {1056, 583},
    {1057, 653},
    {1058, 583},
    {1059, 583},
    {1060, 768},
    {1061, 563},
    {1062, 658},
    {1063, 662},
    {1064, 562},
    {1065, 641},
    {1066, 740},
    {1067, 640},
    {1068, 625},
    {1069, 645},
    {1070, 625},
    {1071, 583},
    {1072, 595},
    {1073, 500},
    {1074, 763},
    {1075, 500},
    {1076, 619},
    {1077, 583},
    {1078, 840},
    {1079, 838},
    {1080, 609},
    {1081, 779},
    {1082, 512},
    {1083, 574},
    {1084, 770},
    {1085, 581},
    {8094, 333},
    {8095, 333},
    {8096, 416},
    {8097, 625},
    {8098, 500},
    {8099, 916},
    {8100, 708},
    {8101, 291},
    {8102, 375},
    {8103, 375},
    {8104, 583},
    {8105, 582},
    {8106, 375},
    {8107, 589},
    {8108, 375},
    {8109, 416},
    {8110, 583},
    {8111, 583},
    {8112, 583},
    {8113, 583},
    {8114, 583},
    {8115, 583},
    {8116, 583},
    {8117, 583},
    {8118, 583},
    {8119, 583},
    {8120, 339},
    {8121, 339},
    {8122, 625},
    {8123, 583},
    {8124, 625},
    {8125, 583},
    {8127, 666},
    {8128, 679},
    {8129, 720},
    {8130, 724},
    {8131, 628},
    {8132, 599},
    {8133, 750},
    {8134, 722},
    {8135, 257},
    {8136, 484},
    {8137, 656},
    {8138, 541},
    {8139, 808},
    {8140, 697},
    {8141, 750},
    {8142, 642},
    {8143, 750},
    {8144, 667},
    {8145, 638},
    {8146, 583},
    {8147, 724},
    {8148, 601},
    {8149, 892},
    {8150, 603},
    {8151, 601},
    {8152, 603},
    {8153, 500},
    {8154, 958},
    {8155, 500},
    {8156, 625},
    {8157, 500},
    {8158, 332},
    {8159, 583},
    {8160, 599},
    {8161, 558},
    {8162, 603},
    {8163, 558},
    {8164, 343},
    {8165, 597},
    {8166, 560},
    {8167, 228},
    {8168, 228},
    {8169, 509},
    {8170, 230},
    {8171, 916},
    {8172, 568},
    {8173, 599},
    {8174, 601},
    {8175, 603},
    {8176, 320},
    {8177, 515},
    {8178, 312},
    {8179, 554},
    {8180, 474},
    {8181, 724},
    {8182, 478},
    {8183, 480},
    {8184, 482},
    {8185, 500},
    {8186, 500},
    {8187, 500},
    {8188, 791},
    {0xFFFF, 0}
};

/*---------------------------------------------------------------------------*/
/*----- BatangChe Font ---------------------------------------------------------*/


static HPDF_STATUS
DotumChe_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_DotumChe_Init\n"));

    fontdef->ascent = 858;
    fontdef->descent = -141;
    fontdef->cap_height = 679;
    fontdef->font_bbox = HPDF_ToBox(0, -150, 1000, 863);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_FIXED_WIDTH;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, DotumChe_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
DotumChe_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = DotumChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
DotumChe_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = DotumChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
DotumChe_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = DotumChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
Dotum_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_Dotum_Init\n"));

    fontdef->ascent = 858;
    fontdef->descent = -141;
    fontdef->cap_height = 679;
    fontdef->font_bbox = HPDF_ToBox(0, -150, 1000, 863);
    fontdef->flags = HPDF_FONT_SYMBOLIC;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, Dotum_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
Dotum_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Dotum_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
Dotum_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Dotum_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
Dotum_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Dotum_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
BatangChe_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_BatangChe_Init\n"));

    fontdef->ascent = 858;
    fontdef->descent = -141;
    fontdef->cap_height = 769;
    fontdef->font_bbox = HPDF_ToBox(-0, -154, 1000, 861);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_FIXED_WIDTH +
                HPDF_FONT_SERIF;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, DotumChe_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
BatangChe_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = BatangChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
BatangChe_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = BatangChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
BatangChe_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = BatangChe_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
Batang_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_Batang_Init\n"));

    fontdef->ascent = 858;
    fontdef->descent = -141;
    fontdef->cap_height = 679;
    fontdef->font_bbox = HPDF_ToBox(0, -154, 1000, 861);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_SERIF;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, Dotum_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
Batang_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Batang_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
Batang_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Batang_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
Batang_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = Batang_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


HPDF_EXPORT(HPDF_STATUS)
HPDF_UseKRFonts   (HPDF_Doc   pdf)
{
    HPDF_FontDef fontdef;
    HPDF_STATUS ret;

    if (!HPDF_HasDoc (pdf))
        return HPDF_INVALID_DOCUMENT;

    /* DotumChe */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "DotumChe",
                DotumChe_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "DotumChe,Bold",
                DotumChe_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "DotumChe,Italic",
                DotumChe_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "DotumChe,BoldItalic",
                DotumChe_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* Dotum */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Dotum",
                Dotum_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Dotum,Bold",
                Dotum_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Dotum,Italic",
                Dotum_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Dotum,BoldItalic",
                Dotum_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* BatangChe */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "BatangChe",
                BatangChe_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "BatangChe,Bold",
                BatangChe_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "BatangChe,Italic",
                BatangChe_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "BatangChe,BoldItalic",
                BatangChe_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* Batang */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Batang",
                Batang_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Batang,Bold",
                Batang_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Batang,Italic",
                Batang_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "Batang,BoldItalic",
                Batang_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    return HPDF_OK;
}

