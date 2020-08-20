/*
 * << Haru Free PDF Library >> -- hpdf_fontdef_jp.c
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

static const HPDF_CID_Width MS_Gothic_W_ARRAY[] = {
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
    {231, 500},
    {232, 500},
    {233, 500},
    {234, 500},
    {235, 500},
    {236, 500},
    {237, 500},
    {238, 500},
    {239, 500},
    {240, 500},
    {241, 500},
    {242, 500},
    {243, 500},
    {244, 500},
    {245, 500},
    {246, 500},
    {247, 500},
    {248, 500},
    {249, 500},
    {250, 500},
    {251, 500},
    {252, 500},
    {253, 500},
    {254, 500},
    {255, 500},
    {256, 500},
    {257, 500},
    {258, 500},
    {259, 500},
    {260, 500},
    {261, 500},
    {262, 500},
    {263, 500},
    {264, 500},
    {265, 500},
    {266, 500},
    {267, 500},
    {268, 500},
    {269, 500},
    {270, 500},
    {271, 500},
    {272, 500},
    {273, 500},
    {274, 500},
    {275, 500},
    {276, 500},
    {277, 500},
    {278, 500},
    {279, 500},
    {280, 500},
    {281, 500},
    {282, 500},
    {283, 500},
    {284, 500},
    {285, 500},
    {286, 500},
    {287, 500},
    {288, 500},
    {289, 500},
    {290, 500},
    {291, 500},
    {292, 500},
    {293, 500},
    {294, 500},
    {295, 500},
    {296, 500},
    {297, 500},
    {298, 500},
    {299, 500},
    {300, 500},
    {301, 500},
    {302, 500},
    {303, 500},
    {304, 500},
    {305, 500},
    {306, 500},
    {307, 500},
    {308, 500},
    {309, 500},
    {310, 500},
    {311, 500},
    {312, 500},
    {313, 500},
    {314, 500},
    {315, 500},
    {316, 500},
    {317, 500},
    {318, 500},
    {319, 500},
    {320, 500},
    {321, 500},
    {322, 500},
    {323, 500},
    {324, 500},
    {327, 500},
    {328, 500},
    {329, 500},
    {330, 500},
    {331, 500},
    {332, 500},
    {333, 500},
    {334, 500},
    {335, 500},
    {336, 500},
    {337, 500},
    {338, 500},
    {339, 500},
    {340, 500},
    {341, 500},
    {342, 500},
    {343, 500},
    {344, 500},
    {345, 500},
    {346, 500},
    {347, 500},
    {348, 500},
    {349, 500},
    {350, 500},
    {351, 500},
    {352, 500},
    {353, 500},
    {354, 500},
    {355, 500},
    {356, 500},
    {357, 500},
    {358, 500},
    {359, 500},
    {360, 500},
    {361, 500},
    {362, 500},
    {363, 500},
    {364, 500},
    {365, 500},
    {366, 500},
    {367, 500},
    {368, 500},
    {369, 500},
    {370, 500},
    {371, 500},
    {372, 500},
    {373, 500},
    {374, 500},
    {375, 500},
    {376, 500},
    {377, 500},
    {378, 500},
    {379, 500},
    {380, 500},
    {381, 500},
    {382, 500},
    {383, 500},
    {384, 500},
    {385, 500},
    {386, 500},
    {387, 500},
    {388, 500},
    {389, 500},
    {631, 500},
    {0xFFFF, 0}
};


static const HPDF_CID_Width MS_PGothic_W_ARRAY[] = {
    {1, 304},
    {2, 218},
    {3, 500},
    {4, 500},
    {5, 500},
    {6, 500},
    {7, 593},
    {8, 203},
    {9, 304},
    {10, 304},
    {11, 500},
    {12, 500},
    {13, 203},
    {14, 500},
    {15, 203},
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
    {27, 203},
    {28, 203},
    {29, 500},
    {30, 500},
    {31, 500},
    {32, 453},
    {33, 667},
    {34, 632},
    {35, 636},
    {36, 664},
    {37, 648},
    {38, 566},
    {39, 550},
    {40, 679},
    {41, 640},
    {42, 246},
    {43, 542},
    {44, 597},
    {45, 539},
    {46, 742},
    {47, 640},
    {48, 707},
    {49, 617},
    {50, 707},
    {51, 625},
    {52, 601},
    {53, 589},
    {54, 640},
    {55, 632},
    {56, 742},
    {57, 601},
    {58, 589},
    {59, 566},
    {60, 335},
    {61, 503},
    {62, 335},
    {63, 414},
    {64, 304},
    {65, 414},
    {66, 476},
    {67, 496},
    {68, 500},
    {69, 496},
    {70, 500},
    {71, 304},
    {72, 460},
    {73, 500},
    {74, 210},
    {75, 218},
    {76, 460},
    {77, 210},
    {78, 734},
    {79, 500},
    {80, 507},
    {81, 496},
    {82, 496},
    {83, 347},
    {84, 460},
    {85, 351},
    {86, 500},
    {87, 476},
    {88, 648},
    {89, 460},
    {90, 476},
    {91, 457},
    {92, 234},
    {93, 234},
    {94, 234},
    {95, 414},
    {231, 304},
    {232, 218},
    {233, 500},
    {234, 500},
    {235, 500},
    {236, 500},
    {237, 593},
    {238, 203},
    {239, 304},
    {240, 304},
    {241, 500},
    {242, 500},
    {243, 203},
    {244, 500},
    {245, 203},
    {246, 500},
    {247, 500},
    {248, 500},
    {249, 500},
    {250, 500},
    {251, 500},
    {252, 500},
    {253, 500},
    {254, 500},
    {255, 500},
    {256, 500},
    {257, 203},
    {258, 203},
    {259, 500},
    {260, 500},
    {261, 500},
    {262, 453},
    {263, 667},
    {264, 632},
    {265, 636},
    {266, 664},
    {267, 648},
    {268, 566},
    {269, 550},
    {270, 679},
    {271, 640},
    {272, 246},
    {273, 542},
    {274, 597},
    {275, 539},
    {276, 742},
    {277, 640},
    {278, 707},
    {279, 617},
    {280, 707},
    {281, 625},
    {282, 601},
    {283, 589},
    {284, 640},
    {285, 632},
    {286, 742},
    {287, 601},
    {288, 589},
    {289, 566},
    {290, 335},
    {291, 503},
    {292, 335},
    {293, 414},
    {294, 304},
    {295, 414},
    {296, 476},
    {297, 496},
    {298, 500},
    {299, 496},
    {300, 500},
    {301, 304},
    {302, 460},
    {303, 500},
    {304, 210},
    {305, 218},
    {306, 460},
    {307, 210},
    {308, 734},
    {309, 500},
    {310, 507},
    {311, 496},
    {312, 496},
    {313, 347},
    {314, 460},
    {315, 351},
    {316, 500},
    {317, 476},
    {318, 648},
    {319, 460},
    {320, 476},
    {321, 457},
    {322, 234},
    {323, 234},
    {324, 234},
    {327, 441},
    {328, 441},
    {329, 441},
    {330, 441},
    {331, 441},
    {332, 546},
    {333, 523},
    {334, 445},
    {335, 480},
    {336, 468},
    {337, 515},
    {338, 523},
    {339, 503},
    {340, 437},
    {341, 500},
    {342, 640},
    {343, 617},
    {344, 566},
    {345, 625},
    {346, 597},
    {347, 636},
    {348, 562},
    {349, 652},
    {350, 539},
    {351, 621},
    {352, 523},
    {353, 664},
    {354, 589},
    {355, 636},
    {356, 644},
    {357, 554},
    {358, 527},
    {359, 601},
    {360, 601},
    {361, 601},
    {362, 460},
    {363, 644},
    {364, 597},
    {365, 578},
    {366, 648},
    {367, 492},
    {368, 636},
    {369, 515},
    {370, 546},
    {371, 613},
    {372, 640},
    {373, 605},
    {374, 453},
    {375, 660},
    {376, 507},
    {377, 609},
    {378, 664},
    {379, 640},
    {380, 519},
    {381, 558},
    {382, 511},
    {383, 656},
    {384, 566},
    {385, 558},
    {386, 589},
    {387, 562},
    {388, 250},
    {389, 230},
    {631, 414},
    {633, 664},
    {634, 664},
    {635, 664},
    {636, 664},
    {637, 664},
    {638, 500},
    {639, 500},
    {640, 500},
    {643, 500},
    {644, 500},
    {645, 500},
    {646, 500},
    {647, 500},
    {648, 500},
    {651, 746},
    {652, 746},
    {653, 734},
    {654, 699},
    {660, 960},
    {662, 500},
    {670, 500},
    {671, 500},
    {672, 500},
    {673, 500},
    {674, 500},
    {675, 500},
    {676, 500},
    {677, 500},
    {678, 500},
    {679, 500},
    {680, 500},
    {681, 500},
    {682, 500},
    {683, 500},
    {684, 500},
    {685, 500},
    {686, 500},
    {687, 500},
    {688, 500},
    {689, 500},
    {690, 500},
    {691, 500},
    {776, 500},
    {777, 500},
    {778, 500},
    {780, 683},
    {781, 683},
    {782, 683},
    {783, 683},
    {784, 683},
    {785, 683},
    {786, 683},
    {787, 683},
    {788, 683},
    {789, 683},
    {790, 714},
    {791, 777},
    {792, 742},
    {793, 757},
    {794, 710},
    {795, 632},
    {796, 773},
    {797, 769},
    {798, 273},
    {799, 605},
    {800, 753},
    {801, 628},
    {802, 933},
    {803, 769},
    {804, 804},
    {805, 710},
    {806, 804},
    {807, 757},
    {808, 742},
    {809, 617},
    {810, 769},
    {811, 714},
    {812, 980},
    {813, 652},
    {814, 648},
    {815, 648},
    {816, 574},
    {817, 601},
    {818, 562},
    {819, 601},
    {820, 562},
    {821, 296},
    {822, 578},
    {823, 621},
    {824, 250},
    {825, 250},
    {826, 593},
    {827, 250},
    {828, 937},
    {829, 621},
    {830, 605},
    {831, 605},
    {832, 601},
    {833, 378},
    {834, 570},
    {835, 335},
    {836, 621},
    {837, 511},
    {838, 777},
    {839, 519},
    {840, 496},
    {841, 507},
    {842, 746},
    {843, 941},
    {844, 804},
    {845, 945},
    {846, 601},
    {847, 707},
    {848, 750},
    {849, 902},
    {850, 804},
    {851, 945},
    {854, 843},
    {855, 902},
    {856, 589},
    {857, 816},
    {858, 945},
    {859, 980},
    {860, 796},
    {861, 894},
    {862, 765},
    {863, 882},
    {864, 765},
    {865, 765},
    {866, 960},
    {867, 980},
    {870, 921},
    {871, 960},
    {872, 921},
    {873, 921},
    {874, 863},
    {875, 902},
    {876, 804},
    {877, 953},
    {878, 957},
    {879, 902},
    {880, 902},
    {881, 765},
    {882, 882},
    {883, 902},
    {884, 941},
    {891, 960},
    {892, 960},
    {893, 960},
    {903, 890},
    {905, 980},
    {906, 980},
    {907, 804},
    {908, 843},
    {910, 843},
    {911, 980},
    {912, 726},
    {913, 863},
    {914, 804},
    {915, 746},
    {916, 863},
    {918, 843},
    {919, 863},
    {923, 855},
    {924, 960},
    {925, 757},
    {926, 898},
    {927, 652},
    {928, 824},
    {929, 753},
    {930, 941},
    {931, 742},
    {932, 894},
    {933, 808},
    {934, 933},
    {935, 824},
    {936, 921},
    {937, 960},
    {938, 964},
    {939, 804},
    {940, 941},
    {941, 929},
    {942, 960},
    {943, 796},
    {944, 890},
    {947, 898},
    {948, 898},
    {949, 902},
    {950, 964},
    {951, 914},
    {952, 980},
    {953, 804},
    {954, 882},
    {955, 765},
    {956, 921},
    {957, 910},
    {958, 960},
    {959, 734},
    {960, 863},
    {961, 921},
    {962, 886},
    {963, 960},
    {964, 648},
    {965, 707},
    {966, 941},
    {967, 910},
    {968, 824},
    {969, 929},
    {970, 707},
    {974, 765},
    {975, 863},
    {976, 863},
    {977, 804},
    {978, 882},
    {979, 882},
    {980, 945},
    {981, 945},
    {982, 945},
    {983, 921},
    {984, 953},
    {985, 953},
    {986, 902},
    {987, 667},
    {988, 976},
    {989, 718},
    {990, 898},
    {991, 804},
    {992, 980},
    {993, 812},
    {994, 960},
    {995, 628},
    {996, 726},
    {997, 808},
    {998, 746},
    {1000, 851},
    {1001, 863},
    {1002, 765},
    {1003, 941},
    {1006, 804},
    {1007, 863},
    {1008, 960},
    {1009, 726},
    {1010, 777},
    {0xFFFF, 0}
};

/*---------------------------------------------------------------------------*/
/*----- Mincho Font ---------------------------------------------------------*/


static HPDF_STATUS
MS_Gothic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_MS_Gothic_Init\n"));

    fontdef->ascent = 859;
    fontdef->descent = -140;
    fontdef->cap_height = 769;
    fontdef->font_bbox = HPDF_ToBox(-0, -136, 1000, 859);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_FIXED_WIDTH;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, MS_Gothic_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
MS_Gothic_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Gothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
MS_Gothic_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Gothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
MS_Gothic_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Gothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
MS_PGothic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_MS_PGothic_Init\n"));

    fontdef->ascent = 859;
    fontdef->descent = -140;
    fontdef->cap_height = 679;
    fontdef->font_bbox = HPDF_ToBox(-121, -136, 996, 859);
    fontdef->flags = HPDF_FONT_SYMBOLIC;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, MS_PGothic_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
MS_PGothic_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PGothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
MS_PGothic_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PGothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
MS_PGothic_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PGothic_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
MS_Mincho_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_MS_Mincho_Init\n"));

    fontdef->ascent = 859;
    fontdef->descent = -140;
    fontdef->cap_height = 769;
    fontdef->font_bbox = HPDF_ToBox(-0, -136, 1000, 859);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_FIXED_WIDTH +
                HPDF_FONT_SERIF;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, MS_Gothic_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
MS_Mincho_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Mincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
MS_Mincho_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Mincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
MS_Mincho_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_Mincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


static HPDF_STATUS
MS_PMincho_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret;

    HPDF_PTRACE ((" HPDF_FontDef_MS_PMincho_Init\n"));

    fontdef->ascent = 859;
    fontdef->descent = -140;
    fontdef->cap_height = 679;
    fontdef->font_bbox = HPDF_ToBox(-82, -136, 996, 859);
    fontdef->flags = HPDF_FONT_SYMBOLIC + HPDF_FONT_SERIF;
    fontdef->italic_angle = 0;
    fontdef->stemv = 78;
    if ((ret = HPDF_CIDFontDef_AddWidth (fontdef, MS_PGothic_W_ARRAY)) !=
                HPDF_OK) {
        return ret;
    }

    fontdef->type = HPDF_FONTDEF_TYPE_CID;
    fontdef->valid = HPDF_TRUE;

    return HPDF_OK;
}


static HPDF_STATUS
MS_PMincho_Bold_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PMincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_FALSE);
}


static HPDF_STATUS
MS_PMincho_Italic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PMincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_FALSE, HPDF_TRUE);
}

static HPDF_STATUS
MS_PMincho_BoldItalic_Init  (HPDF_FontDef   fontdef)
{
    HPDF_STATUS ret = MS_PMincho_Init (fontdef);

    if (ret != HPDF_OK)
        return ret;

    return HPDF_CIDFontDef_ChangeStyle (fontdef, HPDF_TRUE, HPDF_TRUE);
}


HPDF_EXPORT(HPDF_STATUS)
HPDF_UseJPFonts   (HPDF_Doc   pdf)
{
    HPDF_FontDef fontdef;
    HPDF_STATUS ret;

    if (!HPDF_Doc_Validate (pdf))
        return HPDF_INVALID_DOCUMENT;

    /* MS-Gothic */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Gothic",
                MS_Gothic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Gothic,Bold",
                MS_Gothic_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Gothic,Italic",
                MS_Gothic_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Gothic,BoldItalic",
                MS_Gothic_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* MS-PGothic */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PGothic",
                MS_PGothic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PGothic,Bold",
                MS_PGothic_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PGothic,Italic",
                MS_PGothic_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PGothic,BoldItalic",
                MS_PGothic_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* MS-Mincho */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Mincho",
                MS_Mincho_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Mincho,Bold",
                MS_Mincho_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Mincho,Italic",
                MS_Mincho_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-Mincho,BoldItalic",
                MS_Mincho_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    /* MS-PMincho */
    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PMincho",
                MS_PMincho_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PMincho,Bold",
                MS_PMincho_Bold_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PMincho,Italic",
                MS_PMincho_Italic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    fontdef = HPDF_CIDFontDef_New (pdf->mmgr,  "MS-PMincho,BoldItalic",
                MS_PMincho_BoldItalic_Init);

    if ((ret = HPDF_Doc_RegisterFontDef (pdf, fontdef)) != HPDF_OK)
        return ret;

    return HPDF_OK;
}

