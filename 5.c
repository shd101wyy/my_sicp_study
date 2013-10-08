/* Generated from 5.2 by the CHICKEN compiler
   http://www.call-cc.org
   2013-10-07 20:57
   Version 4.8.0.4 (stability/4.8.0) (rev 578619b)
   macosx-unix-clang-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2013-07-15 on aeryn.xorinia.dim (Darwin)
   command line: 5.2
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[107];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,23),40,102,95,52,50,57,32,114,101,103,105,115,116,101,114,45,110,97,109,101,49,55,41,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,22),40,102,111,114,45,101,97,99,104,45,108,111,111,112,55,32,103,49,52,50,49,41,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,52),40,109,97,107,101,45,109,97,99,104,105,110,101,32,114,101,103,105,115,116,101,114,45,110,97,109,101,115,49,32,111,112,115,50,32,99,111,110,116,114,111,108,108,101,114,45,116,101,120,116,51,41,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,15),40,102,95,52,57,51,32,118,97,108,117,101,52,48,41,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,20),40,100,105,115,112,97,116,99,104,32,109,101,115,115,97,103,101,51,54,41,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,22),40,109,97,107,101,45,114,101,103,105,115,116,101,114,32,110,97,109,101,51,51,41,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,25),40,103,101,116,45,99,111,110,116,101,110,116,115,32,114,101,103,105,115,116,101,114,52,51,41,0,0,0,0,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,34),40,115,101,116,45,99,111,110,116,101,110,116,115,33,32,114,101,103,105,115,116,101,114,52,53,32,118,97,108,117,101,52,54,41,0,0,0,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,10),40,112,117,115,104,32,120,53,53,41,0,0,0,0,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,5),40,112,111,112,41,0,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,12),40,105,110,105,116,105,97,108,105,122,101,41,0,0,0,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,20),40,100,105,115,112,97,116,99,104,32,109,101,115,115,97,103,101,53,57,41,0,0,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,12),40,109,97,107,101,45,115,116,97,99,107,41,0,0,0,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,13),40,112,111,112,32,115,116,97,99,107,54,56,41,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,22),40,112,117,115,104,32,115,116,97,99,107,55,48,32,118,97,108,117,101,55,49,41,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,7),40,102,95,55,52,51,41,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,26),40,97,108,108,111,99,97,116,101,45,114,101,103,105,115,116,101,114,32,110,97,109,101,56,53,41,0,0,0,0,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,24),40,108,111,111,107,117,112,45,114,101,103,105,115,116,101,114,32,110,97,109,101,56,55,41};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,9),40,101,120,101,99,117,116,101,41,0,0,0,0,0,0,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,13),40,102,95,54,56,57,32,115,101,113,57,56,41,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,12),40,102,95,55,49,49,32,111,112,57,57,41,0,0,0,0};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,20),40,100,105,115,112,97,116,99,104,32,109,101,115,115,97,103,101,57,51,41,0,0,0,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,18),40,109,97,107,101,45,110,101,119,45,109,97,99,104,105,110,101,41,0,0,0,0,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,18),40,115,116,97,114,116,32,109,97,99,104,105,110,101,49,48,53,41,0,0,0,0,0,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,51),40,103,101,116,45,114,101,103,105,115,116,101,114,45,99,111,110,116,101,110,116,115,32,109,97,99,104,105,110,101,49,48,55,32,114,101,103,105,115,116,101,114,45,110,97,109,101,49,48,56,41,0,0,0,0,0};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,61),40,115,101,116,45,114,101,103,105,115,116,101,114,45,99,111,110,116,101,110,116,115,33,32,109,97,99,104,105,110,101,49,49,48,32,114,101,103,105,115,116,101,114,45,110,97,109,101,49,49,49,32,118,97,108,117,101,49,49,50,41,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,37),40,103,101,116,45,114,101,103,105,115,116,101,114,32,109,97,99,104,105,110,101,49,49,53,32,114,101,103,45,110,97,109,101,49,49,54,41,0,0,0};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,26),40,102,95,55,56,55,32,105,110,115,116,115,49,50,50,32,108,97,98,101,108,115,49,50,51,41,0,0,0,0,0,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,40),40,97,115,115,101,109,98,108,101,32,99,111,110,116,114,111,108,108,101,114,45,116,101,120,116,49,50,48,32,109,97,99,104,105,110,101,49,50,49,41};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,26),40,102,95,56,49,48,32,105,110,115,116,115,49,50,56,32,108,97,98,101,108,115,49,50,57,41,0,0,0,0,0,0};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,35),40,101,120,116,114,97,99,116,45,108,97,98,101,108,115,32,116,101,120,116,49,50,54,32,114,101,99,101,105,118,101,49,50,55,41,0,0,0,0,0};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,15),40,102,95,56,53,48,32,105,110,115,116,49,53,49,41,0};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,26),40,102,111,114,45,101,97,99,104,45,108,111,111,112,49,52,49,32,103,49,52,56,49,53,51,41,0,0,0,0,0,0};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,45),40,117,112,100,97,116,101,45,105,110,115,116,115,33,32,105,110,115,116,115,49,51,50,32,108,97,98,101,108,115,49,51,51,32,109,97,99,104,105,110,101,49,51,52,41,0,0,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,26),40,109,97,107,101,45,105,110,115,116,114,117,99,116,105,111,110,32,116,101,120,116,49,53,56,41,0,0,0,0,0,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,26),40,105,110,115,116,114,117,99,116,105,111,110,45,116,101,120,116,32,105,110,115,116,49,54,48,41,0,0,0,0,0,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,36),40,105,110,115,116,114,117,99,116,105,111,110,45,101,120,101,99,117,116,105,111,110,45,112,114,111,99,32,105,110,115,116,49,54,50,41,0,0,0,0};
static C_char C_TLS li37[] C_aligned={C_lihdr(0,0,49),40,115,101,116,45,105,110,115,116,114,117,99,116,105,111,110,45,101,120,101,99,117,116,105,111,110,45,112,114,111,99,33,32,105,110,115,116,49,54,52,32,112,114,111,99,49,54,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li38[] C_aligned={C_lihdr(0,0,41),40,109,97,107,101,45,108,97,98,101,108,45,101,110,116,114,121,32,108,97,98,101,108,45,110,97,109,101,49,54,55,32,105,110,115,116,115,49,54,56,41,0,0,0,0,0,0,0};
static C_char C_TLS li39[] C_aligned={C_lihdr(0,0,38),40,108,111,111,107,117,112,45,108,97,98,101,108,32,108,97,98,101,108,115,49,55,48,32,108,97,98,101,108,45,110,97,109,101,49,55,49,41,0,0};
static C_char C_TLS li40[] C_aligned={C_lihdr(0,0,85),40,109,97,107,101,45,101,120,101,99,117,116,105,111,110,45,112,114,111,99,101,100,117,114,101,32,105,110,115,116,49,55,52,32,108,97,98,101,108,115,49,55,53,32,109,97,99,104,105,110,101,49,55,54,32,112,99,49,55,55,32,102,108,97,103,49,55,56,32,115,116,97,99,107,49,55,57,32,111,112,115,49,56,48,41,0,0,0};
static C_char C_TLS li41[] C_aligned={C_lihdr(0,0,8),40,102,95,49,48,51,48,41};
static C_char C_TLS li42[] C_aligned={C_lihdr(0,0,62),40,109,97,107,101,45,97,115,115,105,103,110,32,105,110,115,116,49,56,53,32,109,97,99,104,105,110,101,49,56,54,32,108,97,98,101,108,115,49,56,55,32,111,112,101,114,97,116,105,111,110,115,49,56,56,32,112,99,49,56,57,41,0,0};
static C_char C_TLS li43[] C_aligned={C_lihdr(0,0,39),40,97,115,115,105,103,110,45,114,101,103,45,110,97,109,101,32,97,115,115,105,103,110,45,105,110,115,116,114,117,99,116,105,111,110,49,57,52,41,0};
static C_char C_TLS li44[] C_aligned={C_lihdr(0,0,40),40,97,115,115,105,103,110,45,118,97,108,117,101,45,101,120,112,32,97,115,115,105,103,110,45,105,110,115,116,114,117,99,116,105,111,110,49,57,54,41};
static C_char C_TLS li45[] C_aligned={C_lihdr(0,0,18),40,97,100,118,97,110,99,101,45,112,99,32,112,99,49,57,56,41,0,0,0,0,0,0};
static C_char C_TLS li46[] C_aligned={C_lihdr(0,0,8),40,102,95,49,48,56,56,41};
static C_char C_TLS li47[] C_aligned={C_lihdr(0,0,68),40,109,97,107,101,45,116,101,115,116,32,105,110,115,116,50,48,48,32,109,97,99,104,105,110,101,50,48,49,32,108,97,98,101,108,115,50,48,50,32,111,112,101,114,97,116,105,111,110,115,50,48,51,32,102,108,97,103,50,48,52,32,112,99,50,48,53,41,0,0,0,0};
static C_char C_TLS li48[] C_aligned={C_lihdr(0,0,36),40,116,101,115,116,45,99,111,110,100,105,116,105,111,110,32,116,101,115,116,45,105,110,115,116,114,117,99,116,105,111,110,50,49,48,41,0,0,0,0};
static C_char C_TLS li49[] C_aligned={C_lihdr(0,0,8),40,102,95,49,49,50,50,41};
static C_char C_TLS li50[] C_aligned={C_lihdr(0,0,56),40,109,97,107,101,45,98,114,97,110,99,104,32,105,110,115,116,50,49,50,32,109,97,99,104,105,110,101,50,49,51,32,108,97,98,101,108,115,50,49,52,32,102,108,97,103,50,49,53,32,112,99,50,49,54,41};
static C_char C_TLS li51[] C_aligned={C_lihdr(0,0,35),40,98,114,97,110,99,104,45,100,101,115,116,32,98,114,97,110,99,104,45,105,110,115,116,114,117,99,116,105,111,110,50,50,48,41,0,0,0,0,0};
static C_char C_TLS li52[] C_aligned={C_lihdr(0,0,8),40,102,95,49,49,54,50,41};
static C_char C_TLS li53[] C_aligned={C_lihdr(0,0,8),40,102,95,49,49,55,56,41};
static C_char C_TLS li54[] C_aligned={C_lihdr(0,0,46),40,109,97,107,101,45,103,111,116,111,32,105,110,115,116,50,50,50,32,109,97,99,104,105,110,101,50,50,51,32,108,97,98,101,108,115,50,50,52,32,112,99,50,50,53,41,0,0};
static C_char C_TLS li55[] C_aligned={C_lihdr(0,0,31),40,103,111,116,111,45,100,101,115,116,32,103,111,116,111,45,105,110,115,116,114,117,99,116,105,111,110,50,51,51,41,0};
static C_char C_TLS li56[] C_aligned={C_lihdr(0,0,8),40,102,95,49,50,48,54,41};
static C_char C_TLS li57[] C_aligned={C_lihdr(0,0,45),40,109,97,107,101,45,115,97,118,101,32,105,110,115,116,50,51,53,32,109,97,99,104,105,110,101,50,51,54,32,115,116,97,99,107,50,51,55,32,112,99,50,51,56,41,0,0,0};
static C_char C_TLS li58[] C_aligned={C_lihdr(0,0,8),40,102,95,49,50,50,51,41};
static C_char C_TLS li59[] C_aligned={C_lihdr(0,0,48),40,109,97,107,101,45,114,101,115,116,111,114,101,32,105,110,115,116,50,52,50,32,109,97,99,104,105,110,101,50,52,51,32,115,116,97,99,107,50,52,52,32,112,99,50,52,53,41};
static C_char C_TLS li60[] C_aligned={C_lihdr(0,0,42),40,115,116,97,99,107,45,105,110,115,116,45,114,101,103,45,110,97,109,101,32,115,116,97,99,107,45,105,110,115,116,114,117,99,116,105,111,110,50,52,57,41,0,0,0,0,0,0};
static C_char C_TLS li61[] C_aligned={C_lihdr(0,0,8),40,102,95,49,50,53,55,41};
static C_char C_TLS li62[] C_aligned={C_lihdr(0,0,63),40,109,97,107,101,45,112,101,114,102,111,114,109,32,105,110,115,116,50,53,49,32,109,97,99,104,105,110,101,50,53,50,32,108,97,98,101,108,115,50,53,51,32,111,112,101,114,97,116,105,111,110,115,50,53,52,32,112,99,50,53,53,41,0};
static C_char C_TLS li63[] C_aligned={C_lihdr(0,0,24),40,112,101,114,102,111,114,109,45,97,99,116,105,111,110,32,105,110,115,116,50,54,48,41};
static C_char C_TLS li64[] C_aligned={C_lihdr(0,0,8),40,102,95,49,50,56,53,41};
static C_char C_TLS li65[] C_aligned={C_lihdr(0,0,8),40,102,95,49,50,57,54,41};
static C_char C_TLS li66[] C_aligned={C_lihdr(0,0,8),40,102,95,49,51,49,48,41};
static C_char C_TLS li67[] C_aligned={C_lihdr(0,0,48),40,109,97,107,101,45,112,114,105,109,105,116,105,118,101,45,101,120,112,32,101,120,112,50,54,50,32,109,97,99,104,105,110,101,50,54,51,32,108,97,98,101,108,115,50,54,52,41};
static C_char C_TLS li68[] C_aligned={C_lihdr(0,0,22),40,114,101,103,105,115,116,101,114,45,101,120,112,63,32,101,120,112,50,55,50,41,0,0};
static C_char C_TLS li69[] C_aligned={C_lihdr(0,0,25),40,114,101,103,105,115,116,101,114,45,101,120,112,45,114,101,103,32,101,120,112,50,55,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li70[] C_aligned={C_lihdr(0,0,22),40,99,111,110,115,116,97,110,116,45,101,120,112,63,32,101,120,112,50,55,54,41,0,0};
static C_char C_TLS li71[] C_aligned={C_lihdr(0,0,27),40,99,111,110,115,116,97,110,116,45,101,120,112,45,118,97,108,117,101,32,101,120,112,50,55,56,41,0,0,0,0,0};
static C_char C_TLS li72[] C_aligned={C_lihdr(0,0,19),40,108,97,98,101,108,45,101,120,112,63,32,101,120,112,50,56,48,41,0,0,0,0,0};
static C_char C_TLS li73[] C_aligned={C_lihdr(0,0,24),40,108,97,98,101,108,45,101,120,112,45,108,97,98,101,108,32,101,120,112,50,56,50,41};
static C_char C_TLS li74[] C_aligned={C_lihdr(0,0,13),40,102,95,49,51,54,51,32,101,51,48,57,41,0,0,0};
static C_char C_TLS li75[] C_aligned={C_lihdr(0,0,13),40,102,95,49,51,56,48,32,112,51,51,54,41,0,0,0};
static C_char C_TLS li76[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,51,49,57,32,103,51,51,49,51,51,56,41,0,0,0};
static C_char C_TLS li77[] C_aligned={C_lihdr(0,0,8),40,102,95,49,51,55,52,41};
static C_char C_TLS li78[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,50,57,50,32,103,51,48,52,51,49,49,41,0,0,0};
static C_char C_TLS li79[] C_aligned={C_lihdr(0,0,62),40,109,97,107,101,45,111,112,101,114,97,116,105,111,110,45,101,120,112,32,101,120,112,50,56,52,32,109,97,99,104,105,110,101,50,56,53,32,108,97,98,101,108,115,50,56,54,32,111,112,101,114,97,116,105,111,110,115,50,56,55,41,0,0};
static C_char C_TLS li80[] C_aligned={C_lihdr(0,0,23),40,111,112,101,114,97,116,105,111,110,45,101,120,112,63,32,101,120,112,51,52,53,41,0};
static C_char C_TLS li81[] C_aligned={C_lihdr(0,0,35),40,111,112,101,114,97,116,105,111,110,45,101,120,112,45,111,112,32,111,112,101,114,97,116,105,111,110,45,101,120,112,51,52,56,41,0,0,0,0,0};
static C_char C_TLS li82[] C_aligned={C_lihdr(0,0,41),40,111,112,101,114,97,116,105,111,110,45,101,120,112,45,111,112,101,114,97,110,100,115,32,111,112,101,114,97,116,105,111,110,45,101,120,112,51,53,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li83[] C_aligned={C_lihdr(0,0,37),40,108,111,111,107,117,112,45,112,114,105,109,32,115,121,109,98,111,108,51,53,50,32,111,112,101,114,97,116,105,111,110,115,51,53,51,41,0,0,0};
static C_char C_TLS li84[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_1269)
static void C_ccall f_1269(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1261)
static void C_ccall f_1261(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_659)
static void C_ccall f_659(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_651)
static void C_ccall f_651(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1129)
static void C_ccall f_1129(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1121)
static void C_ccall f_1121(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1122)
static void C_ccall f_1122(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1057)
static void C_ccall f_1057(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1051)
static void C_ccall f_1051(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1295)
static void C_ccall f_1295(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1296)
static void C_ccall f_1296(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1293)
static void C_ccall f_1293(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_647)
static void C_fcall f_647(C_word t0,C_word t1) C_noret;
C_noret_decl(f_790)
static void C_ccall f_790(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_792)
static void C_ccall f_792(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_596)
static void C_ccall f_596(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1049)
static void C_ccall f_1049(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_591)
static void C_ccall f_591(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_859)
static void C_ccall f_859(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_856)
static void C_ccall f_856(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_610)
static void C_ccall f_610(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1037)
static void C_ccall f_1037(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_507)
static void C_ccall f_507(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_501)
static void C_ccall f_501(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1030)
static void C_ccall f_1030(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_850)
static void C_ccall f_850(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_770)
static void C_ccall f_770(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1119)
static void C_ccall f_1119(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_849)
static void C_ccall f_849(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_847)
static void C_ccall f_847(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_600)
static void C_ccall f_600(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_845)
static void C_ccall f_845(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_772)
static void C_ccall f_772(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1113)
static void C_ccall f_1113(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_662)
static void C_ccall f_662(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1029)
static void C_ccall f_1029(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_776)
static void C_ccall f_776(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1027)
static void C_ccall f_1027(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1025)
static void C_ccall f_1025(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1021)
static void C_ccall f_1021(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6) C_noret;
C_noret_decl(f_843)
static void C_ccall f_843(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_614)
static void C_ccall f_614(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_679)
static void C_ccall f_679(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1169)
static void C_ccall f_1169(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1109)
static void C_ccall f_1109(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6) C_noret;
C_noret_decl(f_630)
static void C_ccall f_630(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1103)
static void C_ccall f_1103(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1161)
static void C_ccall f_1161(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1162)
static void C_ccall f_1162(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1092)
static void C_ccall f_1092(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1098)
static void C_ccall f_1098(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_875)
static void C_ccall f_875(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_604)
static void C_ccall f_604(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_602)
static void C_ccall f_602(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1159)
static void C_ccall f_1159(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1451)
static void C_ccall f_1451(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_754)
static void C_ccall f_754(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1453)
static void C_ccall f_1453(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_866)
static void C_fcall f_866(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1153)
static void C_ccall f_1153(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1087)
static void C_ccall f_1087(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1088)
static void C_ccall f_1088(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1085)
static void C_ccall f_1085(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_781)
static void C_ccall f_781(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1149)
static void C_ccall f_1149(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_1445)
static void C_ccall f_1445(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1143)
static void C_ccall f_1143(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1079)
static void C_ccall f_1079(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_787)
static void C_ccall f_787(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_546)
static void C_fcall f_546(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1073)
static void C_ccall f_1073(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1075)
static void C_ccall f_1075(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6,C_word t7) C_noret;
C_noret_decl(f_689)
static void C_ccall f_689(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1431)
static void C_fcall f_1431(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1138)
static void C_ccall f_1138(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1063)
static void C_ccall f_1063(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1420)
static void C_fcall f_1420(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_761)
static void C_ccall f_761(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_839)
static void C_ccall f_839(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_763)
static void C_ccall f_763(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_837)
static void C_ccall f_837(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_528)
static void C_fcall f_528(C_word t0,C_word t1) C_noret;
C_noret_decl(f_767)
static void C_ccall f_767(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_520)
static void C_ccall f_520(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_516)
static void C_ccall f_516(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_511)
static void C_ccall f_511(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_743)
static void C_ccall f_743(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1483)
static void C_ccall f_1483(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_670)
static void C_ccall f_670(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_748)
static void C_ccall f_748(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1199)
static void C_ccall f_1199(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_1193)
static void C_ccall f_1193(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1188)
static void C_ccall f_1188(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1185)
static void C_ccall f_1185(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1477)
static void C_ccall f_1477(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1175)
static void C_ccall f_1175(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1178)
static void C_ccall f_1178(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1177)
static void C_ccall f_1177(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1515)
static void C_ccall f_1515(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1512)
static void C_ccall f_1512(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1518)
static void C_ccall f_1518(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_551)
static void C_ccall f_551(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1510)
static void C_ccall f_1510(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1468)
static void C_ccall f_1468(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1502)
static void C_ccall f_1502(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1504)
static void C_ccall f_1504(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1506)
static void C_ccall f_1506(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1508)
static void C_ccall f_1508(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1500)
static void C_ccall f_1500(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1346)
static void C_ccall f_1346(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1340)
static void C_ccall f_1340(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1310)
static void C_ccall f_1310(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1317)
static void C_ccall f_1317(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1328)
static void C_ccall f_1328(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1322)
static void C_ccall f_1322(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1415)
static void C_ccall f_1415(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1358)
static void C_ccall f_1358(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_1352)
static void C_ccall f_1352(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1401)
static void C_fcall f_1401(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1388)
static void C_ccall f_1388(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1380)
static void C_ccall f_1380(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1334)
static void C_ccall f_1334(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1368)
static void C_ccall f_1368(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1363)
static void C_ccall f_1363(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1362)
static void C_ccall f_1362(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1390)
static void C_fcall f_1390(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_587)
static void C_ccall f_587(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1374)
static void C_ccall f_1374(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1373)
static void C_ccall f_1373(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_581)
static void C_ccall f_581(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1236)
static void C_ccall f_1236(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1238)
static void C_ccall f_1238(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1233)
static void C_ccall f_1233(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_432)
static void C_ccall f_432(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1206)
static void C_ccall f_1206(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1205)
static void C_ccall f_1205(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1203)
static void C_ccall f_1203(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_429)
static void C_ccall f_429(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_428)
static void C_ccall f_428(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_424)
static void C_ccall f_424(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_422)
static void C_ccall f_422(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_420)
static void C_ccall f_420(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_810)
static void C_ccall f_810(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_455)
static void C_fcall f_455(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_453)
static void C_ccall f_453(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_450)
static void C_ccall f_450(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_910)
static void C_ccall f_910(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_916)
static void C_ccall f_916(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1301)
static void C_ccall f_1301(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1307)
static void C_ccall f_1307(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1248)
static void C_ccall f_1248(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1309)
static void C_ccall f_1309(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1244)
static void C_ccall f_1244(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6) C_noret;
C_noret_decl(f_445)
static void C_ccall f_445(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_447)
static void C_ccall f_447(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_442)
static void C_ccall f_442(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_440)
static void C_ccall f_440(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1218)
static void C_ccall f_1218(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_1216)
static void C_ccall f_1216(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1213)
static void C_ccall f_1213(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_904)
static void C_ccall f_904(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_828)
static void C_ccall f_828(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1275)
static void C_ccall f_1275(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_711)
static void C_ccall f_711(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_717)
static void C_ccall f_717(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1227)
static void C_ccall f_1227(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1223)
static void C_ccall f_1223(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1222)
static void C_ccall f_1222(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1256)
static void C_ccall f_1256(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1257)
static void C_ccall f_1257(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1254)
static void C_ccall f_1254(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_475)
static void C_ccall f_475(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_479)
static void C_ccall f_479(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_464)
static void C_ccall f_464(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_931)
static void C_ccall f_931(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6,C_word t7,C_word t8) C_noret;
C_noret_decl(f_1285)
static void C_ccall f_1285(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_493)
static void C_ccall f_493(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1284)
static void C_ccall f_1284(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1282)
static void C_ccall f_1282(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_632)
static void C_ccall f_632(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_898)
static void C_ccall f_898(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_892)
static void C_ccall f_892(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_886)
static void C_ccall f_886(C_word c,C_word t0,C_word t1,C_word t2) C_noret;

C_noret_decl(trf_647)
static void C_fcall trf_647(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_647(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_647(t0,t1);}

C_noret_decl(trf_866)
static void C_fcall trf_866(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_866(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_866(t0,t1,t2);}

C_noret_decl(trf_546)
static void C_fcall trf_546(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_546(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_546(t0,t1);}

C_noret_decl(trf_1431)
static void C_fcall trf_1431(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1431(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_1431(t0,t1);}

C_noret_decl(trf_1420)
static void C_fcall trf_1420(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1420(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1420(t0,t1,t2);}

C_noret_decl(trf_528)
static void C_fcall trf_528(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_528(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_528(t0,t1);}

C_noret_decl(trf_1401)
static void C_fcall trf_1401(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1401(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_1401(t0,t1);}

C_noret_decl(trf_1390)
static void C_fcall trf_1390(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1390(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1390(t0,t1,t2);}

C_noret_decl(trf_455)
static void C_fcall trf_455(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_455(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_455(t0,t1,t2);}

C_noret_decl(tr9)
static void C_fcall tr9(C_proc9 k) C_regparm C_noret;
C_regparm static void C_fcall tr9(C_proc9 k){
C_word t8=C_pick(0);
C_word t7=C_pick(1);
C_word t6=C_pick(2);
C_word t5=C_pick(3);
C_word t4=C_pick(4);
C_word t3=C_pick(5);
C_word t2=C_pick(6);
C_word t1=C_pick(7);
C_word t0=C_pick(8);
C_adjust_stack(-9);
(k)(9,t0,t1,t2,t3,t4,t5,t6,t7,t8);}

C_noret_decl(tr5)
static void C_fcall tr5(C_proc5 k) C_regparm C_noret;
C_regparm static void C_fcall tr5(C_proc5 k){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
(k)(5,t0,t1,t2,t3,t4);}

C_noret_decl(tr8)
static void C_fcall tr8(C_proc8 k) C_regparm C_noret;
C_regparm static void C_fcall tr8(C_proc8 k){
C_word t7=C_pick(0);
C_word t6=C_pick(1);
C_word t5=C_pick(2);
C_word t4=C_pick(3);
C_word t3=C_pick(4);
C_word t2=C_pick(5);
C_word t1=C_pick(6);
C_word t0=C_pick(7);
C_adjust_stack(-8);
(k)(8,t0,t1,t2,t3,t4,t5,t6,t7);}

C_noret_decl(tr6)
static void C_fcall tr6(C_proc6 k) C_regparm C_noret;
C_regparm static void C_fcall tr6(C_proc6 k){
C_word t5=C_pick(0);
C_word t4=C_pick(1);
C_word t3=C_pick(2);
C_word t2=C_pick(3);
C_word t1=C_pick(4);
C_word t0=C_pick(5);
C_adjust_stack(-6);
(k)(6,t0,t1,t2,t3,t4,t5);}

C_noret_decl(tr7)
static void C_fcall tr7(C_proc7 k) C_regparm C_noret;
C_regparm static void C_fcall tr7(C_proc7 k){
C_word t6=C_pick(0);
C_word t5=C_pick(1);
C_word t4=C_pick(2);
C_word t3=C_pick(3);
C_word t2=C_pick(4);
C_word t1=C_pick(5);
C_word t0=C_pick(6);
C_adjust_stack(-7);
(k)(7,t0,t1,t2,t3,t4,t5,t6);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

/* perform-action in k421 in k419 */
static void C_ccall f_1269(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1269,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* k1260 */
static void C_ccall f_1261(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:290: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k658 in k650 in execute in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_659(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_659,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_662,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:96: g90");
t3=t1;
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}

/* k650 in execute in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_651(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_651,2,t0,t1);}
if(C_truep(C_i_nullp(t1))){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[28]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_659,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=C_i_car(t1);
C_trace("5.2:97: instruction-execution-proc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[29]+1)))(3,*((C_word*)lf[29]+1),t2,t3);}}

/* k1127 */
static void C_ccall f_1129(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(t1)){
C_trace("5.2:244: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
C_trace("5.2:245: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}}

/* k1120 in k1117 in k1112 in make-branch in k421 in k419 */
static void C_ccall f_1121(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1121,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1122,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=((C_word)li49),tmp=(C_word)a,a+=6,tmp));}

/* f_1122 in k1120 in k1117 in k1112 in make-branch in k421 in k419 */
static void C_ccall f_1122(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1122,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1129,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:243: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t2,((C_word*)t0)[4]);}

/* assign-value-exp in k421 in k419 */
static void C_ccall f_1057(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1057,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cddr(t2));}

/* assign-reg-name in k421 in k419 */
static void C_ccall f_1051(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1051,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1294 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1295(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1295,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1296,a[2]=t1,a[3]=((C_word)li65),tmp=(C_word)a,a+=4,tmp));}

/* f_1296 in k1294 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1296(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1296,2,t0,t1);}
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1293(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1293,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1295,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1301,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:300: label-exp-label");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[72]+1)))(3,*((C_word*)lf[72]+1),t3,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1307,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:302: register-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[78]+1)))(3,*((C_word*)lf[78]+1),t2,((C_word*)t0)[4]);}}

/* execute in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_fcall f_647(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_647,NULL,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_651,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:93: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t2,((C_word*)t0)[3]);}

/* k789 */
static void C_ccall f_790(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[3]);}

/* extract-labels in k421 in k419 */
static void C_ccall f_792(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_792,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
C_trace("5.2:136: receive");
t4=t3;
((C_proc4)C_fast_retrieve_proc(t4))(4,t4,t1,C_SCHEME_END_OF_LIST,C_SCHEME_END_OF_LIST);}
else{
t4=C_i_cdr(t2);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_810,a[2]=t2,a[3]=t3,a[4]=((C_word)li29),tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:137: extract-labels");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[40]+1)))(4,*((C_word*)lf[40]+1),t1,t4,t5);}}

/* make-new-machine in k421 in k419 */
static void C_ccall f_596(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_596,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_600,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:71: make-register");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[7]+1)))(3,*((C_word*)lf[7]+1),t2,lf[23]);}

/* k1048 in make-assign in k421 in k419 */
static void C_ccall f_1049(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:205: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k590 in push in k421 in k419 */
static void C_ccall f_591(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:64: g72");
t2=t1;
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k858 */
static void C_ccall f_859(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:157: make-execution-procedure");
((C_proc9)C_fast_retrieve_proc(*((C_word*)lf[44]+1)))(9,*((C_word*)lf[44]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3],((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[6],((C_word*)t0)[7],((C_word*)t0)[8]);}

/* k855 */
static void C_ccall f_856(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:155: set-instruction-execution-proc!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[43]+1)))(4,*((C_word*)lf[43]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* allocate-register in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_610(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_610,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_614,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
if(C_truep(C_i_assoc(t2,((C_word*)((C_word*)t0)[2])[1]))){
C_trace("5.2:82: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t3,lf[26],t2);}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_630,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:84: make-register");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[7]+1)))(3,*((C_word*)lf[7]+1),t4,t2);}}

/* k1035 in k1026 in k1024 in make-assign in k421 in k419 */
static void C_ccall f_1037(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(C_truep(t1)){
C_trace("5.2:209: make-operation-exp");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[65]+1)))(6,*((C_word*)lf[65]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[6]);}
else{
t2=C_i_car(((C_word*)t0)[3]);
C_trace("5.2:211: make-primitive-exp");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(5,*((C_word*)lf[66]+1),((C_word*)t0)[2],t2,((C_word*)t0)[4],((C_word*)t0)[5]);}}

/* set-contents! in k421 in k419 */
static void C_ccall f_507(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_507,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_511,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:38: register");
t5=t2;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,lf[10]);}

/* get-contents in k421 in k419 */
static void C_ccall f_501(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_501,3,t0,t1,t2);}
C_trace("5.2:36: register");
t3=t2;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t1,lf[9]);}

/* f_1030 in k1028 in k1026 in k1024 in make-assign in k421 in k419 */
static void C_ccall f_1030(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1030,2,t0,t1);}
C_trace("5.2:214: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),t1,((C_word*)t0)[2]);}

/* f_850 in k848 in k846 in k844 in k842 in update-insts! in k421 in k419 */
static void C_ccall f_850(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[13],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_850,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_856,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_859,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],tmp=(C_word)a,a+=9,tmp);
C_trace("5.2:158: instruction-text");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[45]+1)))(3,*((C_word*)lf[45]+1),t4,t2);}

/* k769 in set-register-contents! in k421 in k419 */
static void C_ccall f_770(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:120: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1117 in k1112 in make-branch in k421 in k419 */
static void C_ccall f_1119(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1119,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1121,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1138,a[2]=t2,a[3]=((C_word*)t0)[5],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:241: label-exp-label");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[72]+1)))(3,*((C_word*)lf[72]+1),t3,((C_word*)t0)[6]);}
else{
C_trace("5.2:246: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[73],((C_word*)t0)[7]);}}

/* k848 in k846 in k844 in k842 in update-insts! in k421 in k419 */
static void C_ccall f_849(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_849,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_850,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=t1,a[8]=((C_word)li31),tmp=(C_word)a,a+=9,tmp);
t3=((C_word*)t0)[7];
t4=C_i_check_list_2(t3,lf[2]);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_866,a[2]=t6,a[3]=t2,a[4]=((C_word)li32),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_866(t8,((C_word*)t0)[8],t3);}

/* k846 in k844 in k842 in update-insts! in k421 in k419 */
static void C_ccall f_847(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_847,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_849,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=t1,a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],tmp=(C_word)a,a+=9,tmp);
C_trace("5.2:152: machine");
t3=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t2,lf[35]);}

/* k599 in make-new-machine in k421 in k419 */
static void C_ccall f_600(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_600,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_602,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:72: make-register");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[7]+1)))(3,*((C_word*)lf[7]+1),t2,lf[24]);}

/* k844 in k842 in update-insts! in k421 in k419 */
static void C_ccall f_845(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_845,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_847,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],tmp=(C_word)a,a+=8,tmp);
C_trace("5.2:151: machine");
t3=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t2,lf[34]);}

/* get-register in k421 in k419 */
static void C_ccall f_772(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_772,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_776,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:123: machine");
t5=t2;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,lf[31]);}

/* k1112 in make-branch in k421 in k419 */
static void C_ccall f_1113(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1113,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1119,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=t1,a[7]=((C_word*)t0)[6],tmp=(C_word)a,a+=8,tmp);
C_trace("5.2:239: label-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[74]+1)))(3,*((C_word*)lf[74]+1),t2,t1);}

/* k660 in k658 in k650 in execute in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_662(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:98: execute");
t2=((C_word*)((C_word*)t0)[2])[1];
f_647(t2,((C_word*)t0)[3]);}

/* k1028 in k1026 in k1024 in make-assign in k421 in k419 */
static void C_ccall f_1029(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1029,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1030,a[2]=((C_word*)t0)[3],a[3]=((C_word)li41),tmp=(C_word)a,a+=4,tmp));}

/* k775 in get-register in k421 in k419 */
static void C_ccall f_776(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:122: g117");
t2=t1;
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k1026 in k1024 in make-assign in k421 in k419 */
static void C_ccall f_1027(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1027,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1029,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1037,a[2]=t2,a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:208: operation-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[67]+1)))(3,*((C_word*)lf[67]+1),t3,t1);}

/* k1024 in make-assign in k421 in k419 */
static void C_ccall f_1025(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1025,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1027,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:206: assign-value-exp");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[68]+1)))(3,*((C_word*)lf[68]+1),t2,((C_word*)t0)[7]);}

/* make-assign in k421 in k419 */
static void C_ccall f_1021(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6){
C_word tmp;
C_word t7;
C_word t8;
C_word t9;
C_word ab[12],*a=ab;
if(c!=7) C_bad_argc_2(c,7,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr7,(void*)f_1021,7,t0,t1,t2,t3,t4,t5,t6);}
t7=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1025,a[2]=t1,a[3]=t6,a[4]=t3,a[5]=t4,a[6]=t5,a[7]=t2,tmp=(C_word)a,a+=8,tmp);
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1049,a[2]=t7,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:205: assign-reg-name");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[69]+1)))(3,*((C_word*)lf[69]+1),t8,t2);}

/* k842 in update-insts! in k421 in k419 */
static void C_ccall f_843(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_843,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_845,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:150: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),t2,((C_word*)t0)[3],lf[24]);}

/* k613 in allocate-register in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_614(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[25]);}

/* k678 in dispatch in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_679(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:102: execute");
t2=((C_word*)((C_word*)t0)[2])[1];
f_647(t2,((C_word*)t0)[3]);}

/* k1168 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1169(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:254: lookup-label");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[47]+1)))(4,*((C_word*)lf[47]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* make-branch in k421 in k419 */
static void C_ccall f_1109(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6){
C_word tmp;
C_word t7;
C_word t8;
C_word ab[7],*a=ab;
if(c!=7) C_bad_argc_2(c,7,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr7,(void*)f_1109,7,t0,t1,t2,t3,t4,t5,t6);}
t7=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1113,a[2]=t1,a[3]=t6,a[4]=t5,a[5]=t4,a[6]=t2,tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:238: branch-dest");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[75]+1)))(3,*((C_word*)lf[75]+1),t7,t2);}

/* k629 in allocate-register in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_630(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_630,2,t0,t1);}
t2=C_a_i_list2(&a,2,((C_word*)t0)[2],t1);
t3=C_a_i_cons(&a,2,t2,((C_word*)((C_word*)t0)[3])[1]);
t4=C_mutate(((C_word *)((C_word*)t0)[3])+1,t3);
t5=((C_word*)t0)[4];
f_614(2,t5,t4);}

/* test-condition in k421 in k419 */
static void C_ccall f_1103(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1103,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* k1160 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1161(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1161,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1162,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word)li52),tmp=(C_word)a,a+=5,tmp));}

/* f_1162 in k1160 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1162(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1162,2,t0,t1);}
C_trace("5.2:256: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),t1,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k1091 */
static void C_ccall f_1092(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:232: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k1097 */
static void C_ccall f_1098(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:231: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k874 in for-each-loop141 in k848 in k846 in k844 in k842 in update-insts! in k421 in k419 */
static void C_ccall f_875(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[2],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_866(t3,((C_word*)t0)[4],t2);}

/* k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_604(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word ab[63],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_604,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_743,a[2]=t1,a[3]=((C_word)li15),tmp=(C_word)a,a+=4,tmp);
t3=C_a_i_list2(&a,2,lf[22],t2);
t4=C_a_i_list1(&a,1,t3);
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_a_i_list2(&a,2,lf[23],((C_word*)t0)[2]);
t7=C_a_i_list2(&a,2,lf[24],((C_word*)t0)[3]);
t8=C_a_i_list2(&a,2,t6,t7);
t9=(*a=C_VECTOR_TYPE|1,a[1]=t8,tmp=(C_word)a,a+=2,tmp);
t10=C_SCHEME_UNDEFINED;
t11=(*a=C_VECTOR_TYPE|1,a[1]=t10,tmp=(C_word)a,a+=2,tmp);
t12=C_SCHEME_UNDEFINED;
t13=(*a=C_VECTOR_TYPE|1,a[1]=t12,tmp=(C_word)a,a+=2,tmp);
t14=C_SCHEME_UNDEFINED;
t15=(*a=C_VECTOR_TYPE|1,a[1]=t14,tmp=(C_word)a,a+=2,tmp);
t16=C_SCHEME_UNDEFINED;
t17=C_set_block_item(t11,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_610,a[2]=t9,a[3]=((C_word)li16),tmp=(C_word)a,a+=4,tmp));
t18=C_set_block_item(t13,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_632,a[2]=t9,a[3]=((C_word)li17),tmp=(C_word)a,a+=4,tmp));
t19=C_set_block_item(t15,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_647,a[2]=t15,a[3]=((C_word*)t0)[2],a[4]=((C_word)li18),tmp=(C_word)a,a+=5,tmp));
t20=t16=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_670,a[2]=t15,a[3]=((C_word*)t0)[2],a[4]=t11,a[5]=t13,a[6]=t5,a[7]=t1,a[8]=((C_word)li21),tmp=(C_word)a,a+=9,tmp);
t21=((C_word*)t0)[4];
((C_proc2)(void*)(*((C_word*)t21+1)))(2,t21,t16);}

/* k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_602(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_602,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_604,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:73: make-stack");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(2,*((C_word*)lf[15]+1),t2);}

/* k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1159(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1159,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1161,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1169,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:255: label-exp-label");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[72]+1)))(3,*((C_word*)lf[72]+1),t3,((C_word*)t0)[5]);}
else{
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1175,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[6],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[7],tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:257: register-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[78]+1)))(3,*((C_word*)lf[78]+1),t2,((C_word*)t0)[5]);}}

/* k1450 in make-operation-exp in k421 in k419 */
static void C_ccall f_1451(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:315: lookup-prim");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[92]+1)))(4,*((C_word*)lf[92]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* get-register-contents in k421 in k419 */
static void C_ccall f_754(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[3],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_754,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_761,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:118: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),t4,t2,t3);}

/* operation-exp? in k421 in k419 */
static void C_ccall f_1453(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1453,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_i_car(t2);
C_trace("5.2:323: tagged-list?");
((C_proc4)C_fast_retrieve_symbol_proc(lf[86]))(4,*((C_word*)lf[86]+1),t1,t3,lf[94]);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_FALSE);}}

/* for-each-loop141 in k848 in k846 in k844 in k842 in update-insts! in k421 in k419 */
static void C_fcall f_866(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_866,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_875,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("5.2:148: g142");
t5=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k1152 in make-goto in k421 in k419 */
static void C_ccall f_1153(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1153,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1159,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],tmp=(C_word)a,a+=8,tmp);
C_trace("5.2:252: label-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[74]+1)))(3,*((C_word*)lf[74]+1),t2,t1);}

/* k1086 in k1083 in k1078 in make-test in k421 in k419 */
static void C_ccall f_1087(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1087,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1088,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=t1,a[5]=((C_word)li46),tmp=(C_word)a,a+=6,tmp));}

/* f_1088 in k1086 in k1083 in k1078 in make-test in k421 in k419 */
static void C_ccall f_1088(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1088,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1092,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1098,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:231: condition-proc");
t4=((C_word*)t0)[4];
((C_proc2)C_fast_retrieve_proc(t4))(2,t4,t3);}

/* k1083 in k1078 in make-test in k421 in k419 */
static void C_ccall f_1085(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1085,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1087,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:228: make-operation-exp");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[65]+1)))(6,*((C_word*)lf[65]+1),t2,((C_word*)t0)[5],((C_word*)t0)[6],((C_word*)t0)[7],((C_word*)t0)[8]);}
else{
C_trace("5.2:233: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[70],((C_word*)t0)[9]);}}

/* assemble in k421 in k419 */
static void C_ccall f_781(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_781,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_787,a[2]=t3,a[3]=((C_word)li27),tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:130: extract-labels");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[40]+1)))(4,*((C_word*)lf[40]+1),t1,t2,t4);}

/* make-goto in k421 in k419 */
static void C_ccall f_1149(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word ab[7],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_1149,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1153,a[2]=t1,a[3]=t5,a[4]=t4,a[5]=t3,a[6]=t2,tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:251: goto-dest");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[79]+1)))(3,*((C_word*)lf[79]+1),t6,t2);}

/* k1444 in map-loop292 in k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1445(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1445,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1431,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t4=t3;
f_1431(t4,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[6])+1,t2);
t5=t3;
f_1431(t5,t4);}}

/* branch-dest in k421 in k419 */
static void C_ccall f_1143(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1143,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1078 in make-test in k421 in k419 */
static void C_ccall f_1079(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1079,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|9,a[1]=(C_word)f_1085,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],a[9]=((C_word*)t0)[8],tmp=(C_word)a,a+=10,tmp);
C_trace("5.2:226: operation-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[67]+1)))(3,*((C_word*)lf[67]+1),t2,t1);}

/* f_787 in assemble in k421 in k419 */
static void C_ccall f_787(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_787,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_790,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:132: update-insts!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[39]+1)))(5,*((C_word*)lf[39]+1),t4,t2,t3,((C_word*)t0)[2]);}

/* initialize in make-stack in k421 in k419 */
static void C_fcall f_546(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_546,NULL,2,t0,t1);}
t2=C_set_block_item(((C_word*)t0)[2],0,C_SCHEME_END_OF_LIST);
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,lf[17]);}

/* k1072 in advance-pc in k421 in k419 */
static void C_ccall f_1073(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_i_cdr(t1);
C_trace("5.2:221: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],((C_word*)t0)[3],t2);}

/* make-test in k421 in k419 */
static void C_ccall f_1075(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6,C_word t7){
C_word tmp;
C_word t8;
C_word t9;
C_word ab[9],*a=ab;
if(c!=8) C_bad_argc_2(c,8,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr8,(void*)f_1075,8,t0,t1,t2,t3,t4,t5,t6,t7);}
t8=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_1079,a[2]=t1,a[3]=t7,a[4]=t6,a[5]=t3,a[6]=t4,a[7]=t5,a[8]=t2,tmp=(C_word)a,a+=9,tmp);
C_trace("5.2:225: test-condition");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[71]+1)))(3,*((C_word*)lf[71]+1),t8,t2);}

/* f_689 in dispatch in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_689(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_689,3,t0,t1,t2);}
t3=C_mutate((C_word*)lf[4]+1 /* (set! install-instruction-sequence ...) */,t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k1430 in k1444 in map-loop292 in k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_fcall f_1431(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_1420(t4,((C_word*)t0)[6],t3);}

/* k1137 in k1117 in k1112 in make-branch in k421 in k419 */
static void C_ccall f_1138(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:241: lookup-label");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[47]+1)))(4,*((C_word*)lf[47]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* advance-pc in k421 in k419 */
static void C_ccall f_1063(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1063,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1073,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:221: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t3,t2);}

/* map-loop292 in k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_fcall f_1420(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1420,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1445,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=t1,a[6]=((C_word*)t0)[4],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("5.2:317: g298");
t5=((C_word*)t0)[5];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* k760 in get-register-contents in k421 in k419 */
static void C_ccall f_761(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:118: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),((C_word*)t0)[2],t1);}

/* update-insts! in k421 in k419 */
static void C_ccall f_839(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_839,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_843,a[2]=t3,a[3]=t4,a[4]=t2,a[5]=t1,tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:149: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),t5,t4,lf[23]);}

/* set-register-contents! in k421 in k419 */
static void C_ccall f_763(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word ab[7],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_763,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_767,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_770,a[2]=t5,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:120: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),t6,t2,t3);}

/* k836 */
static void C_ccall f_837(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_837,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)t0)[2]);
C_trace("5.2:145: receive");
t3=((C_word*)t0)[3];
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,((C_word*)t0)[4],t2,((C_word*)t0)[5]);}

/* pop in make-stack in k421 in k419 */
static void C_fcall f_528(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_528,NULL,2,t0,t1);}
if(C_truep(C_i_nullp(((C_word*)((C_word*)t0)[2])[1]))){
C_trace("5.2:48: error");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t1,lf[16]);}
else{
t2=C_i_car(((C_word*)((C_word*)t0)[2])[1]);
t3=C_i_cdr(((C_word*)((C_word*)t0)[2])[1]);
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t2);}}

/* k766 in set-register-contents! in k421 in k419 */
static void C_ccall f_767(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[17]);}

/* push in make-stack in k421 in k419 */
static void C_ccall f_520(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_520,3,t0,t1,t2);}
t3=C_a_i_cons(&a,2,t2,((C_word*)((C_word*)t0)[2])[1]);
t4=C_mutate(((C_word *)((C_word*)t0)[2])+1,t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t4);}

/* make-stack in k421 in k419 */
static void C_ccall f_516(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word ab[26],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_516,2,t0,t1);}
t2=C_SCHEME_END_OF_LIST;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_SCHEME_UNDEFINED;
t7=(*a=C_VECTOR_TYPE|1,a[1]=t6,tmp=(C_word)a,a+=2,tmp);
t8=C_SCHEME_UNDEFINED;
t9=(*a=C_VECTOR_TYPE|1,a[1]=t8,tmp=(C_word)a,a+=2,tmp);
t10=C_SCHEME_UNDEFINED;
t11=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_520,a[2]=t3,a[3]=((C_word)li8),tmp=(C_word)a,a+=4,tmp));
t12=C_set_block_item(t7,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_528,a[2]=t3,a[3]=((C_word)li9),tmp=(C_word)a,a+=4,tmp));
t13=C_set_block_item(t9,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_546,a[2]=t3,a[3]=((C_word)li10),tmp=(C_word)a,a+=4,tmp));
t14=t10=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_551,a[2]=t5,a[3]=t7,a[4]=t9,a[5]=((C_word)li11),tmp=(C_word)a,a+=6,tmp);
t15=t1;
((C_proc2)(void*)(*((C_word*)t15+1)))(2,t15,t10);}

/* k510 in set-contents! in k421 in k419 */
static void C_ccall f_511(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:37: g47");
t2=t1;
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* f_743 in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_743(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_743,2,t0,t1);}
C_trace("5.2:77: stack");
t2=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,t1,lf[20]);}

/* lookup-prim in k421 in k419 */
static void C_ccall f_1483(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1483,4,t0,t1,t2,t3);}
t4=C_i_assoc(t2,t3);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cadr(t4));}
else{
C_trace("5.2:332: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[95],t2);}}

/* dispatch in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_670(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_670,3,t0,t1,t2);}
t3=C_eqp(t2,lf[30]);
if(C_truep(t3)){
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_679,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:101: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),t4,((C_word*)t0)[3],C_SCHEME_END_OF_LIST);}
else{
t4=C_eqp(t2,lf[4]);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_689,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));}
else{
t5=C_eqp(t2,lf[1]);
if(C_truep(t5)){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,((C_word*)((C_word*)t0)[4])[1]);}
else{
t6=C_eqp(t2,lf[31]);
if(C_truep(t6)){
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,((C_word*)((C_word*)t0)[5])[1]);}
else{
t7=C_eqp(t2,lf[5]);
if(C_truep(t7)){
t8=t1;
((C_proc2)(void*)(*((C_word*)t8+1)))(2,t8,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_711,a[2]=((C_word*)t0)[6],a[3]=((C_word)li20),tmp=(C_word)a,a+=4,tmp));}
else{
t8=C_eqp(t2,lf[34]);
if(C_truep(t8)){
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,((C_word*)t0)[7]);}
else{
t9=C_eqp(t2,lf[35]);
if(C_truep(t9)){
t10=t1;
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,((C_word*)((C_word*)t0)[6])[1]);}
else{
C_trace("5.2:111: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[36],t2);}}}}}}}}

/* start in k421 in k419 */
static void C_ccall f_748(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_748,3,t0,t1,t2);}
C_trace("5.2:116: machine");
t3=t2;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t1,lf[30]);}

/* make-save in k421 in k419 */
static void C_ccall f_1199(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word ab[9],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_1199,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1203,a[2]=t1,a[3]=t5,a[4]=t4,tmp=(C_word)a,a+=5,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1216,a[2]=t6,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:270: stack-inst-reg-name");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[80]+1)))(3,*((C_word*)lf[80]+1),t7,t2);}

/* goto-dest in k421 in k419 */
static void C_ccall f_1193(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1193,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1187 in k1173 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1188(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:259: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k1184 */
static void C_ccall f_1185(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:262: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* operation-exp-operands in k421 in k419 */
static void C_ccall f_1477(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1477,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* k1173 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1175(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1175,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1177,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1188,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:260: register-exp-reg");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[76]+1)))(3,*((C_word*)lf[76]+1),t3,((C_word*)t0)[5]);}
else{
C_trace("5.2:263: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[77],((C_word*)t0)[6]);}}

/* f_1178 in k1176 in k1173 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1178(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1178,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1185,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:262: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t2,((C_word*)t0)[3]);}

/* k1176 in k1173 in k1157 in k1152 in make-goto in k421 in k419 */
static void C_ccall f_1177(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1177,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1178,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word)li53),tmp=(C_word)a,a+=5,tmp));}

/* k1513 in k1509 in k1507 in k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1515(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k1511 in k1509 in k1507 in k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1512(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* k1517 in k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1518(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:365: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[98]+1)))(3,*((C_word*)lf[98]+1),((C_word*)t0)[2],t1);}

/* dispatch in make-stack in k421 in k419 */
static void C_ccall f_551(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_551,3,t0,t1,t2);}
t3=C_eqp(t2,lf[18]);
if(C_truep(t3)){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,((C_word*)((C_word*)t0)[2])[1]);}
else{
t4=C_eqp(t2,lf[19]);
if(C_truep(t4)){
C_trace("5.2:57: pop");
t5=((C_word*)((C_word*)t0)[3])[1];
f_528(t5,t1);}
else{
t5=C_eqp(t2,lf[20]);
if(C_truep(t5)){
C_trace("5.2:58: initialize");
t6=((C_word*)((C_word*)t0)[4])[1];
f_546(t6,t1);}
else{
C_trace("5.2:59: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[21],t2);}}}}

/* k1509 in k1507 in k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1510(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1510,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1512,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1515,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[97]))(2,*((C_word*)lf[97]+1),t3);}

/* operation-exp-op in k421 in k419 */
static void C_ccall f_1468(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1468,3,t0,t1,t2);}
t3=C_i_car(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_cadr(t3));}

/* k1501 in k1498 in k421 in k419 */
static void C_ccall f_1502(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1502,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1504,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:361: set-register-contents!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[38]+1)))(5,*((C_word*)lf[38]+1),t2,C_fast_retrieve(lf[96]),lf[101],C_fix(40));}

/* k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1504(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1504,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1506,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:363: start");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[30]+1)))(3,*((C_word*)lf[30]+1),t2,C_fast_retrieve(lf[96]));}

/* k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1506(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1506,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1508,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1518,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:365: get-register-contents");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[37]+1)))(4,*((C_word*)lf[37]+1),t3,C_fast_retrieve(lf[96]),lf[100]);}

/* k1507 in k1505 in k1503 in k1501 in k1498 in k421 in k419 */
static void C_ccall f_1508(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1508,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1510,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:368: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[98]+1)))(3,*((C_word*)lf[98]+1),t2,lf[99]);}

/* k1498 in k421 in k419 */
static void C_ccall f_1500(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1500,2,t0,t1);}
t2=C_mutate((C_word*)lf[96]+1 /* (set! gcd-machine ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1502,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:359: set-register-contents!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[38]+1)))(5,*((C_word*)lf[38]+1),t3,C_fast_retrieve(lf[96]),lf[100],C_fix(206));}

/* label-exp? in k421 in k419 */
static void C_ccall f_1346(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1346,3,t0,t1,t2);}
C_trace("5.2:312: tagged-list?");
((C_proc4)C_fast_retrieve_symbol_proc(lf[86]))(4,*((C_word*)lf[86]+1),t1,t2,lf[89]);}

/* constant-exp-value in k421 in k419 */
static void C_ccall f_1340(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1340,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* f_1310 in k1308 in k1305 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1310(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1310,2,t0,t1);}
C_trace("5.2:305: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t1,((C_word*)t0)[2]);}

/* k1316 in k1305 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1317(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:303: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* register-exp-reg in k421 in k419 */
static void C_ccall f_1328(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1328,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* register-exp? in k421 in k419 */
static void C_ccall f_1322(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1322,3,t0,t1,t2);}
C_trace("5.2:308: tagged-list?");
((C_proc4)C_fast_retrieve_symbol_proc(lf[86]))(4,*((C_word*)lf[86]+1),t1,t2,lf[87]);}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(1380)){
C_save(t1);
C_rereclaim2(1380*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,107);
lf[0]=C_h_intern(&lf[0],12,"make-machine");
lf[1]=C_h_intern(&lf[1],17,"allocate-register");
lf[2]=C_h_intern(&lf[2],8,"for-each");
lf[3]=C_h_intern(&lf[3],8,"assemble");
lf[4]=C_h_intern(&lf[4],28,"install-instruction-sequence");
lf[5]=C_h_intern(&lf[5],18,"install-operations");
lf[6]=C_h_intern(&lf[6],16,"make-new-machine");
lf[7]=C_h_intern(&lf[7],13,"make-register");
lf[8]=C_h_intern(&lf[8],12,"\052unassigned\052");
lf[9]=C_h_intern(&lf[9],3,"get");
lf[10]=C_h_intern(&lf[10],3,"set");
lf[11]=C_h_intern(&lf[11],5,"error");
lf[12]=C_decode_literal(C_heaptop,"\376B\000\000\033Unknown request -- REGISTER");
lf[13]=C_h_intern(&lf[13],12,"get-contents");
lf[14]=C_h_intern(&lf[14],13,"set-contents!");
lf[15]=C_h_intern(&lf[15],10,"make-stack");
lf[16]=C_decode_literal(C_heaptop,"\376B\000\000\022Empty Stack -- POP");
lf[17]=C_h_intern(&lf[17],4,"done");
lf[18]=C_h_intern(&lf[18],4,"push");
lf[19]=C_h_intern(&lf[19],3,"pop");
lf[20]=C_h_intern(&lf[20],10,"initialize");
lf[21]=C_decode_literal(C_heaptop,"\376B\000\000\030Unknown request -- STACK");
lf[22]=C_h_intern(&lf[22],16,"initialize-stack");
lf[23]=C_h_intern(&lf[23],2,"pc");
lf[24]=C_h_intern(&lf[24],4,"flag");
lf[25]=C_h_intern(&lf[25],18,"register-allocated");
lf[26]=C_decode_literal(C_heaptop,"\376B\000\000\033Multiply defined register: ");
lf[27]=C_decode_literal(C_heaptop,"\376B\000\000\022Unknown register: ");
lf[28]=C_h_intern(&lf[28],4,"Done");
lf[29]=C_h_intern(&lf[29],26,"instruction-execution-proc");
lf[30]=C_h_intern(&lf[30],5,"start");
lf[31]=C_h_intern(&lf[31],12,"get-register");
lf[32]=C_h_intern(&lf[32],6,"append");
lf[33]=C_h_intern(&lf[33],3,"ops");
lf[34]=C_h_intern(&lf[34],5,"stack");
lf[35]=C_h_intern(&lf[35],10,"operations");
lf[36]=C_decode_literal(C_heaptop,"\376B\000\000\032Unknown request -- MACHINE");
lf[37]=C_h_intern(&lf[37],21,"get-register-contents");
lf[38]=C_h_intern(&lf[38],22,"set-register-contents!");
lf[39]=C_h_intern(&lf[39],13,"update-insts!");
lf[40]=C_h_intern(&lf[40],14,"extract-labels");
lf[41]=C_h_intern(&lf[41],16,"make-label-entry");
lf[42]=C_h_intern(&lf[42],16,"make-instruction");
lf[43]=C_h_intern(&lf[43],31,"set-instruction-execution-proc!");
lf[44]=C_h_intern(&lf[44],24,"make-execution-procedure");
lf[45]=C_h_intern(&lf[45],16,"instruction-text");
lf[46]=C_h_intern(&lf[46],3,"\357\277\274");
lf[47]=C_h_intern(&lf[47],12,"lookup-label");
lf[48]=C_decode_literal(C_heaptop,"\376B\000\000\033Undefined label -- ASSEMBLE");
lf[49]=C_h_intern(&lf[49],6,"assign");
lf[50]=C_h_intern(&lf[50],11,"make-assign");
lf[51]=C_h_intern(&lf[51],4,"test");
lf[52]=C_h_intern(&lf[52],9,"make-test");
lf[53]=C_h_intern(&lf[53],6,"branch");
lf[54]=C_h_intern(&lf[54],11,"make-branch");
lf[55]=C_h_intern(&lf[55],4,"goto");
lf[56]=C_h_intern(&lf[56],9,"make-goto");
lf[57]=C_h_intern(&lf[57],4,"save");
lf[58]=C_h_intern(&lf[58],9,"make-save");
lf[59]=C_h_intern(&lf[59],7,"restore");
lf[60]=C_h_intern(&lf[60],12,"make-restore");
lf[61]=C_h_intern(&lf[61],7,"perform");
lf[62]=C_h_intern(&lf[62],12,"make-perform");
lf[63]=C_decode_literal(C_heaptop,"\376B\000\000$Unknown instruction type -- ASSEMBLE");
lf[64]=C_h_intern(&lf[64],10,"advance-pc");
lf[65]=C_h_intern(&lf[65],18,"make-operation-exp");
lf[66]=C_h_intern(&lf[66],18,"make-primitive-exp");
lf[67]=C_h_intern(&lf[67],14,"operation-exp\077");
lf[68]=C_h_intern(&lf[68],16,"assign-value-exp");
lf[69]=C_h_intern(&lf[69],15,"assign-reg-name");
lf[70]=C_decode_literal(C_heaptop,"\376B\000\000 Bad TEST instruction -- ASSEMBLE");
lf[71]=C_h_intern(&lf[71],14,"test-condition");
lf[72]=C_h_intern(&lf[72],15,"label-exp-label");
lf[73]=C_decode_literal(C_heaptop,"\376B\000\000\042Bad BRANCH instruction -- ASSEMBLE");
lf[74]=C_h_intern(&lf[74],10,"label-exp\077");
lf[75]=C_h_intern(&lf[75],11,"branch-dest");
lf[76]=C_h_intern(&lf[76],16,"register-exp-reg");
lf[77]=C_decode_literal(C_heaptop,"\376B\000\000 Bad GOTO instruction -- ASSEMBLE");
lf[78]=C_h_intern(&lf[78],13,"register-exp\077");
lf[79]=C_h_intern(&lf[79],9,"goto-dest");
lf[80]=C_h_intern(&lf[80],19,"stack-inst-reg-name");
lf[81]=C_decode_literal(C_heaptop,"\376B\000\000#Bad PERFORM instruction -- ASSEMBLE");
lf[82]=C_h_intern(&lf[82],14,"perform-action");
lf[83]=C_h_intern(&lf[83],18,"constant-exp-value");
lf[84]=C_decode_literal(C_heaptop,"\376B\000\000#Unknown expression type -- ASSEMBLE");
lf[85]=C_h_intern(&lf[85],13,"constant-exp\077");
lf[86]=C_h_intern(&lf[86],12,"tagged-list\077");
lf[87]=C_h_intern(&lf[87],3,"reg");
lf[88]=C_h_intern(&lf[88],5,"const");
lf[89]=C_h_intern(&lf[89],5,"label");
lf[90]=C_h_intern(&lf[90],3,"map");
lf[91]=C_h_intern(&lf[91],22,"operation-exp-operands");
lf[92]=C_h_intern(&lf[92],11,"lookup-prim");
lf[93]=C_h_intern(&lf[93],16,"operation-exp-op");
lf[94]=C_h_intern(&lf[94],2,"op");
lf[95]=C_decode_literal(C_heaptop,"\376B\000\000\035Unknown operation -- ASSEMBLE");
lf[96]=C_h_intern(&lf[96],11,"gcd-machine");
lf[97]=C_h_intern(&lf[97],25,"\003sysimplicit-exit-handler");
lf[98]=C_h_intern(&lf[98],7,"display");
lf[99]=C_decode_literal(C_heaptop,"\376B\000\000\004Done");
lf[100]=C_h_intern(&lf[100],1,"a");
lf[101]=C_h_intern(&lf[101],1,"b");
lf[102]=C_h_intern(&lf[102],3,"rem");
lf[103]=C_h_intern(&lf[103],9,"remainder");
lf[104]=C_h_intern(&lf[104],1,"=");
lf[105]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\001a\376\003\000\000\002\376\001\000\000\001b\376\003\000\000\002\376\001\000\000\001t\376\377\016");
lf[106]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\006test-b\376\003\000\000\002\376\003\000\000\002\376\001\000\000\004test\376\003\000\000\002\376\003\000\000\002\376\001\000\000\002op\376\003\000\000\002\376\001\000\000\001=\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000"
"\003reg\376\003\000\000\002\376\001\000\000\001b\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\005const\376\003\000\000\002\376\377\001\000\000\000\000\376\377\016\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\006branch\376\003\000"
"\000\002\376\003\000\000\002\376\001\000\000\005label\376\003\000\000\002\376\001\000\000\010gcd-done\376\377\016\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\006assign\376\003\000\000\002\376\001\000\000\001t\376\003\000\000\002\376\003"
"\000\000\002\376\001\000\000\002op\376\003\000\000\002\376\001\000\000\003rem\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\003reg\376\003\000\000\002\376\001\000\000\001a\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\003reg\376\003\000\000"
"\002\376\001\000\000\001b\376\377\016\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000\000\006assign\376\003\000\000\002\376\001\000\000\001a\376\003\000\000\002\376\003\000\000\002\376\001\000\000\003reg\376\003\000\000\002\376\001\000\000\001b\376\377\016\376\377\016"
"\376\003\000\000\002\376\003\000\000\002\376\001\000\000\006assign\376\003\000\000\002\376\001\000\000\001b\376\003\000\000\002\376\003\000\000\002\376\001\000\000\003reg\376\003\000\000\002\376\001\000\000\001t\376\377\016\376\377\016\376\003\000\000\002\376\003\000\000\002\376\001\000"
"\000\004goto\376\003\000\000\002\376\003\000\000\002\376\001\000\000\005label\376\003\000\000\002\376\001\000\000\006test-b\376\377\016\376\377\016\376\003\000\000\002\376\001\000\000\010gcd-done\376\377\016");
C_register_lf2(lf,107,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_420,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k1414 in map-loop319 */
static void C_ccall f_1415(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1415,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1401,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t4=t3;
f_1401(t4,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[6])+1,t2);
t5=t3;
f_1401(t5,t4);}}

/* make-operation-exp in k421 in k419 */
static void C_ccall f_1358(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word ab[10],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_1358,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1362,a[2]=t3,a[3]=t4,a[4]=t1,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1451,a[2]=t6,a[3]=t5,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:315: operation-exp-op");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[93]+1)))(3,*((C_word*)lf[93]+1),t7,t2);}

/* label-exp-label in k421 in k419 */
static void C_ccall f_1352(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1352,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1400 in k1414 in map-loop319 */
static void C_fcall f_1401(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_1390(t4,((C_word*)t0)[6],t3);}

/* k1386 */
static void C_ccall f_1388(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_apply(4,0,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* f_1380 */
static void C_ccall f_1380(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1380,3,t0,t1,t2);}
C_trace("5.2:321: p");
t3=t2;
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t1);}

/* constant-exp? in k421 in k419 */
static void C_ccall f_1334(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1334,3,t0,t1,t2);}
C_trace("5.2:310: tagged-list?");
((C_proc4)C_fast_retrieve_symbol_proc(lf[86]))(4,*((C_word*)lf[86]+1),t1,t2,lf[88]);}

/* k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1368(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[13],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1368,2,t0,t1);}
t2=C_i_check_list_2(t1,lf[90]);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1373,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1420,a[2]=((C_word*)t0)[4],a[3]=t5,a[4]=((C_word*)t0)[5],a[5]=((C_word*)t0)[6],a[6]=((C_word)li78),tmp=(C_word)a,a+=7,tmp));
t7=((C_word*)t5)[1];
f_1420(t7,t3,t1);}

/* f_1363 in k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1363(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1363,3,t0,t1,t2);}
C_trace("5.2:318: make-primitive-exp");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(5,*((C_word*)lf[66]+1),t1,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1362(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[16],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1362,2,t0,t1);}
t2=C_SCHEME_END_OF_LIST;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_SCHEME_FALSE;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1363,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word)li74),tmp=(C_word)a,a+=5,tmp);
t7=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1368,a[2]=((C_word*)t0)[4],a[3]=t1,a[4]=t5,a[5]=t3,a[6]=t6,tmp=(C_word)a,a+=7,tmp);
C_trace("5.2:319: operation-exp-operands");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(3,*((C_word*)lf[91]+1),t7,((C_word*)t0)[5]);}

/* map-loop319 */
static void C_fcall f_1390(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1390,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1415,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=t1,a[6]=((C_word*)t0)[4],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("5.2:321: g325");
t5=((C_word*)t0)[5];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* push in k421 in k419 */
static void C_ccall f_587(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_587,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_591,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:65: stack");
t5=t2;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,lf[18]);}

/* f_1374 in k1371 in k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1374(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word ab[20],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1374,2,t0,t1);}
t2=C_SCHEME_END_OF_LIST;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_SCHEME_FALSE;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1380,a[2]=((C_word)li75),tmp=(C_word)a,a+=3,tmp);
t7=C_i_check_list_2(((C_word*)t0)[2],lf[90]);
t8=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1388,a[2]=t1,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t9=C_SCHEME_UNDEFINED;
t10=(*a=C_VECTOR_TYPE|1,a[1]=t9,tmp=(C_word)a,a+=2,tmp);
t11=C_set_block_item(t10,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1390,a[2]=t5,a[3]=t10,a[4]=t3,a[5]=t6,a[6]=((C_word)li76),tmp=(C_word)a,a+=7,tmp));
t12=((C_word*)t10)[1];
f_1390(t12,t8,((C_word*)t0)[2]);}

/* k1371 in k1367 in k1361 in make-operation-exp in k421 in k419 */
static void C_ccall f_1373(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1373,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1374,a[2]=t1,a[3]=((C_word*)t0)[3],a[4]=((C_word)li77),tmp=(C_word)a,a+=5,tmp));}

/* pop in k421 in k419 */
static void C_ccall f_581(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_581,3,t0,t1,t2);}
C_trace("5.2:63: stack");
t3=t2;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t1,lf[19]);}

/* k1235 in make-restore in k421 in k419 */
static void C_ccall f_1236(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:275: get-register");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(4,*((C_word*)lf[31]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* stack-inst-reg-name in k421 in k419 */
static void C_ccall f_1238(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1238,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1232 */
static void C_ccall f_1233(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:278: set-contents!");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k431 */
static void C_ccall f_432(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:17: g18");
t2=t1;
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* f_1206 in k1204 in k1202 in make-save in k421 in k419 */
static void C_ccall f_1206(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1206,2,t0,t1);}
C_trace("5.2:273: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),t1,((C_word*)t0)[2]);}

/* k1204 in k1202 in make-save in k421 in k419 */
static void C_ccall f_1205(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1205,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1206,a[2]=((C_word*)t0)[3],a[3]=((C_word)li56),tmp=(C_word)a,a+=4,tmp));}

/* k1202 in make-save in k421 in k419 */
static void C_ccall f_1203(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1203,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1205,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1213,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:271: get-contents");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),t3,t1);}

/* f_429 in k427 in make-machine in k421 in k419 */
static void C_ccall f_429(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_429,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_432,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:17: machine");
t4=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,lf[1]);}

/* k427 in make-machine in k421 in k419 */
static void C_ccall f_428(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[17],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_428,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_429,a[2]=t1,a[3]=((C_word)li0),tmp=(C_word)a,a+=4,tmp);
t3=((C_word*)t0)[2];
t4=C_i_check_list_2(t3,lf[2]);
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_440,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
t6=C_SCHEME_UNDEFINED;
t7=(*a=C_VECTOR_TYPE|1,a[1]=t6,tmp=(C_word)a,a+=2,tmp);
t8=C_set_block_item(t7,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_455,a[2]=t7,a[3]=t2,a[4]=((C_word)li1),tmp=(C_word)a,a+=5,tmp));
t9=((C_word*)t7)[1];
f_455(t9,t5,t3);}

/* make-machine in k421 in k419 */
static void C_ccall f_424(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_424,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_428,a[2]=t2,a[3]=t1,a[4]=t4,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:16: make-new-machine");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(2,*((C_word*)lf[6]+1),t5);}

/* k421 in k419 */
static void C_ccall f_422(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word t40;
C_word t41;
C_word t42;
C_word t43;
C_word t44;
C_word t45;
C_word t46;
C_word t47;
C_word t48;
C_word t49;
C_word t50;
C_word t51;
C_word t52;
C_word t53;
C_word t54;
C_word t55;
C_word t56;
C_word ab[168],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_422,2,t0,t1);}
t2=C_mutate((C_word*)lf[0]+1 /* (set! make-machine ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_424,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[7]+1 /* (set! make-register ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_475,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[13]+1 /* (set! get-contents ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_501,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[14]+1 /* (set! set-contents! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_507,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[15]+1 /* (set! make-stack ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_516,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[19]+1 /* (set! pop ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_581,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[18]+1 /* (set! push ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_587,a[2]=((C_word)li14),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[6]+1 /* (set! make-new-machine ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_596,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[30]+1 /* (set! start ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_748,a[2]=((C_word)li23),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[37]+1 /* (set! get-register-contents ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_754,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[38]+1 /* (set! set-register-contents! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_763,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[31]+1 /* (set! get-register ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_772,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[3]+1 /* (set! assemble ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_781,a[2]=((C_word)li28),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[40]+1 /* (set! extract-labels ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_792,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate((C_word*)lf[39]+1 /* (set! update-insts! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_839,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate((C_word*)lf[42]+1 /* (set! make-instruction ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_886,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t18=C_fast_retrieve(lf[46]);
t19=C_mutate((C_word*)lf[45]+1 /* (set! instruction-text ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_892,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[29]+1 /* (set! instruction-execution-proc ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_898,a[2]=((C_word)li36),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[43]+1 /* (set! set-instruction-execution-proc! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_904,a[2]=((C_word)li37),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[41]+1 /* (set! make-label-entry ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_910,a[2]=((C_word)li38),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate((C_word*)lf[47]+1 /* (set! lookup-label ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_916,a[2]=((C_word)li39),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate((C_word*)lf[44]+1 /* (set! make-execution-procedure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_931,a[2]=((C_word)li40),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate((C_word*)lf[50]+1 /* (set! make-assign ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1021,a[2]=((C_word)li42),tmp=(C_word)a,a+=3,tmp));
t26=C_mutate((C_word*)lf[69]+1 /* (set! assign-reg-name ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1051,a[2]=((C_word)li43),tmp=(C_word)a,a+=3,tmp));
t27=C_mutate((C_word*)lf[68]+1 /* (set! assign-value-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1057,a[2]=((C_word)li44),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[64]+1 /* (set! advance-pc ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1063,a[2]=((C_word)li45),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate((C_word*)lf[52]+1 /* (set! make-test ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1075,a[2]=((C_word)li47),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[71]+1 /* (set! test-condition ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1103,a[2]=((C_word)li48),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[54]+1 /* (set! make-branch ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1109,a[2]=((C_word)li50),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate((C_word*)lf[75]+1 /* (set! branch-dest ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1143,a[2]=((C_word)li51),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate((C_word*)lf[56]+1 /* (set! make-goto ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1149,a[2]=((C_word)li54),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate((C_word*)lf[79]+1 /* (set! goto-dest ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1193,a[2]=((C_word)li55),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate((C_word*)lf[58]+1 /* (set! make-save ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1199,a[2]=((C_word)li57),tmp=(C_word)a,a+=3,tmp));
t36=C_mutate((C_word*)lf[60]+1 /* (set! make-restore ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1218,a[2]=((C_word)li59),tmp=(C_word)a,a+=3,tmp));
t37=C_mutate((C_word*)lf[80]+1 /* (set! stack-inst-reg-name ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1238,a[2]=((C_word)li60),tmp=(C_word)a,a+=3,tmp));
t38=C_mutate((C_word*)lf[62]+1 /* (set! make-perform ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1244,a[2]=((C_word)li62),tmp=(C_word)a,a+=3,tmp));
t39=C_mutate((C_word*)lf[82]+1 /* (set! perform-action ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1269,a[2]=((C_word)li63),tmp=(C_word)a,a+=3,tmp));
t40=C_mutate((C_word*)lf[66]+1 /* (set! make-primitive-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1275,a[2]=((C_word)li67),tmp=(C_word)a,a+=3,tmp));
t41=C_mutate((C_word*)lf[78]+1 /* (set! register-exp? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1322,a[2]=((C_word)li68),tmp=(C_word)a,a+=3,tmp));
t42=C_mutate((C_word*)lf[76]+1 /* (set! register-exp-reg ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1328,a[2]=((C_word)li69),tmp=(C_word)a,a+=3,tmp));
t43=C_mutate((C_word*)lf[85]+1 /* (set! constant-exp? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1334,a[2]=((C_word)li70),tmp=(C_word)a,a+=3,tmp));
t44=C_mutate((C_word*)lf[83]+1 /* (set! constant-exp-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1340,a[2]=((C_word)li71),tmp=(C_word)a,a+=3,tmp));
t45=C_mutate((C_word*)lf[74]+1 /* (set! label-exp? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1346,a[2]=((C_word)li72),tmp=(C_word)a,a+=3,tmp));
t46=C_mutate((C_word*)lf[72]+1 /* (set! label-exp-label ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1352,a[2]=((C_word)li73),tmp=(C_word)a,a+=3,tmp));
t47=C_mutate((C_word*)lf[65]+1 /* (set! make-operation-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1358,a[2]=((C_word)li79),tmp=(C_word)a,a+=3,tmp));
t48=C_mutate((C_word*)lf[67]+1 /* (set! operation-exp? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1453,a[2]=((C_word)li80),tmp=(C_word)a,a+=3,tmp));
t49=C_mutate((C_word*)lf[93]+1 /* (set! operation-exp-op ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1468,a[2]=((C_word)li81),tmp=(C_word)a,a+=3,tmp));
t50=C_mutate((C_word*)lf[91]+1 /* (set! operation-exp-operands ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1477,a[2]=((C_word)li82),tmp=(C_word)a,a+=3,tmp));
t51=C_mutate((C_word*)lf[92]+1 /* (set! lookup-prim ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1483,a[2]=((C_word)li83),tmp=(C_word)a,a+=3,tmp));
t52=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1500,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t53=C_a_i_list2(&a,2,lf[102],*((C_word*)lf[103]+1));
t54=C_a_i_list2(&a,2,lf[104],*((C_word*)lf[104]+1));
t55=C_a_i_list2(&a,2,t53,t54);
C_trace("5.2:346: make-machine");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[0]+1)))(5,*((C_word*)lf[0]+1),t52,lf[105],t55,lf[106]);}

/* k419 */
static void C_ccall f_420(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_420,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_422,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* f_810 in extract-labels in k421 in k419 */
static void C_ccall f_810(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[6],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_810,4,t0,t1,t2,t3);}
t4=C_i_car(((C_word*)t0)[2]);
if(C_truep(C_i_symbolp(t4))){
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_828,a[2]=t3,a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:142: make-label-entry");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[41]+1)))(4,*((C_word*)lf[41]+1),t5,t4,t2);}
else{
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_837,a[2]=t2,a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:145: make-instruction");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[42]+1)))(3,*((C_word*)lf[42]+1),t5,t4);}}

/* for-each-loop7 in k427 in make-machine in k421 in k419 */
static void C_fcall f_455(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_455,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_464,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("5.2:15: g8");
t5=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* k452 in k446 in k443 in k441 in k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_453(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:15: g27");
t2=((C_word*)t0)[2];
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[3],t1);}

/* k448 in k446 in k443 in k441 in k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_450(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[3]);}

/* make-label-entry in k421 in k419 */
static void C_ccall f_910(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word ab[3],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_910,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,t2,t3));}

/* lookup-label in k421 in k419 */
static void C_ccall f_916(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_916,4,t0,t1,t2,t3);}
t4=C_i_assoc(t3,t2);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cdr(t4));}
else{
C_trace("5.2:178: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[48],t3);}}

/* k1300 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1301(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:299: lookup-label");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[47]+1)))(4,*((C_word*)lf[47]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k1305 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1307(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1307,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1309,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1317,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:304: register-exp-reg");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[76]+1)))(3,*((C_word*)lf[76]+1),t3,((C_word*)t0)[4]);}
else{
C_trace("5.2:307: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[84],((C_word*)t0)[4]);}}

/* k1247 in make-perform in k421 in k419 */
static void C_ccall f_1248(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1248,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_1254,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],a[8]=((C_word*)t0)[7],tmp=(C_word)a,a+=9,tmp);
C_trace("5.2:284: operation-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[67]+1)))(3,*((C_word*)lf[67]+1),t2,t1);}

/* k1308 in k1305 in k1291 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1309(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1309,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1310,a[2]=t1,a[3]=((C_word)li66),tmp=(C_word)a,a+=4,tmp));}

/* make-perform in k421 in k419 */
static void C_ccall f_1244(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6){
C_word tmp;
C_word t7;
C_word t8;
C_word ab[8],*a=ab;
if(c!=7) C_bad_argc_2(c,7,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr7,(void*)f_1244,7,t0,t1,t2,t3,t4,t5,t6);}
t7=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1248,a[2]=t1,a[3]=t6,a[4]=t3,a[5]=t4,a[6]=t5,a[7]=t2,tmp=(C_word)a,a+=8,tmp);
C_trace("5.2:283: perform-action");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[82]+1)))(3,*((C_word*)lf[82]+1),t7,t2);}

/* k443 in k441 in k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_445(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_445,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_447,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:20: machine");
t3=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t2,lf[4]);}

/* k446 in k443 in k441 in k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_447(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_447,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_450,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_453,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:21: assemble");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[3]+1)))(4,*((C_word*)lf[3]+1),t3,((C_word*)t0)[4],((C_word*)t0)[3]);}

/* k441 in k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_442(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_442,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_445,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("5.2:15: g25");
t3=t1;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t2,((C_word*)t0)[5]);}

/* k438 in k427 in make-machine in k421 in k419 */
static void C_ccall f_440(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_440,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_442,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:19: machine");
t3=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t2,lf[5]);}

/* make-restore in k421 in k419 */
static void C_ccall f_1218(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word ab[9],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_1218,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1222,a[2]=t1,a[3]=t5,a[4]=t4,tmp=(C_word)a,a+=5,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1236,a[2]=t6,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:276: stack-inst-reg-name");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[80]+1)))(3,*((C_word*)lf[80]+1),t7,t2);}

/* k1215 in make-save in k421 in k419 */
static void C_ccall f_1216(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:269: get-register");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(5,*((C_word*)lf[31]+1),((C_word*)t0)[2],((C_word*)t0)[3],C_fast_retrieve(lf[46]),t1);}

/* k1212 in k1202 in make-save in k421 in k419 */
static void C_ccall f_1213(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:271: push");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[18]+1)))(4,*((C_word*)lf[18]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* set-instruction-execution-proc! in k421 in k419 */
static void C_ccall f_904(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_904,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_set_cdr(t2,t3));}

/* k827 */
static void C_ccall f_828(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_828,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)t0)[2]);
C_trace("5.2:141: receive");
t3=((C_word*)t0)[3];
((C_proc4)C_fast_retrieve_proc(t3))(4,t3,((C_word*)t0)[4],((C_word*)t0)[5],t2);}

/* make-primitive-exp in k421 in k419 */
static void C_ccall f_1275(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1275,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1282,a[2]=t1,a[3]=t2,a[4]=t4,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:294: constant-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[85]+1)))(3,*((C_word*)lf[85]+1),t5,t2);}

/* f_711 in dispatch in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_711(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_711,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_717,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:108: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[32]+1)))(4,*((C_word*)lf[32]+1),t3,((C_word*)((C_word*)t0)[2])[1],C_fast_retrieve(lf[33]));}

/* k715 */
static void C_ccall f_717(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,t1);
t3=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}

/* k1226 */
static void C_ccall f_1227(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("5.2:279: advance-pc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}

/* f_1223 in k1221 in make-restore in k421 in k419 */
static void C_ccall f_1223(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1223,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1227,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1233,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:278: pop");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(3,*((C_word*)lf[19]+1),t3,((C_word*)t0)[4]);}

/* k1221 in make-restore in k421 in k419 */
static void C_ccall f_1222(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1222,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1223,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word*)t0)[4],a[5]=((C_word)li58),tmp=(C_word)a,a+=6,tmp));}

/* k1255 in k1252 in k1247 in make-perform in k421 in k419 */
static void C_ccall f_1256(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1256,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1257,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word)li61),tmp=(C_word)a,a+=5,tmp));}

/* f_1257 in k1255 in k1252 in k1247 in make-perform in k421 in k419 */
static void C_ccall f_1257(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1257,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1261,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:289: action-proc");
t3=((C_word*)t0)[3];
((C_proc2)C_fast_retrieve_proc(t3))(2,t3,t2);}

/* k1252 in k1247 in make-perform in k421 in k419 */
static void C_ccall f_1254(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1254,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1256,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("5.2:286: make-operation-exp");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[65]+1)))(6,*((C_word*)lf[65]+1),t2,((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[6],((C_word*)t0)[7]);}
else{
C_trace("5.2:291: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[81],((C_word*)t0)[8]);}}

/* make-register in k421 in k419 */
static void C_ccall f_475(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_475,3,t0,t1,t2);}
t3=lf[8];
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_479,a[2]=t4,a[3]=((C_word)li4),tmp=(C_word)a,a+=4,tmp);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* dispatch in make-register in k421 in k419 */
static void C_ccall f_479(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_479,3,t0,t1,t2);}
t3=C_eqp(t2,lf[9]);
if(C_truep(t3)){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,((C_word*)((C_word*)t0)[2])[1]);}
else{
t4=C_eqp(t2,lf[10]);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_493,a[2]=((C_word*)t0)[2],a[3]=((C_word)li3),tmp=(C_word)a,a+=4,tmp));}
else{
C_trace("5.2:32: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[12],t2);}}}

/* k463 in for-each-loop7 in k427 in make-machine in k421 in k419 */
static void C_ccall f_464(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[2],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_455(t3,((C_word*)t0)[4],t2);}

/* make-execution-procedure in k421 in k419 */
static void C_ccall f_931(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5,C_word t6,C_word t7,C_word t8){
C_word tmp;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word *a;
if(c!=9) C_bad_argc_2(c,9,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr9,(void*)f_931,9,t0,t1,t2,t3,t4,t5,t6,t7,t8);}
t9=C_i_car(t2);
t10=C_eqp(t9,lf[49]);
if(C_truep(t10)){
C_trace("5.2:186: make-assign");
((C_proc7)C_fast_retrieve_proc(*((C_word*)lf[50]+1)))(7,*((C_word*)lf[50]+1),t1,t2,t4,t3,t8,t5);}
else{
t11=C_i_car(t2);
t12=C_eqp(t11,lf[51]);
if(C_truep(t12)){
C_trace("5.2:188: make-test");
((C_proc8)C_fast_retrieve_proc(*((C_word*)lf[52]+1)))(8,*((C_word*)lf[52]+1),t1,t2,t4,t3,t8,t6,t5);}
else{
t13=C_i_car(t2);
t14=C_eqp(t13,lf[53]);
if(C_truep(t14)){
C_trace("5.2:190: make-branch");
((C_proc7)C_fast_retrieve_proc(*((C_word*)lf[54]+1)))(7,*((C_word*)lf[54]+1),t1,t2,t4,t3,t6,t5);}
else{
t15=C_i_car(t2);
t16=C_eqp(t15,lf[55]);
if(C_truep(t16)){
C_trace("5.2:192: make-goto");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[56]+1)))(6,*((C_word*)lf[56]+1),t1,t2,t4,t3,t5);}
else{
t17=C_i_car(t2);
t18=C_eqp(t17,lf[57]);
if(C_truep(t18)){
C_trace("5.2:194: make-save");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[58]+1)))(6,*((C_word*)lf[58]+1),t1,t2,t4,t7,t5);}
else{
t19=C_i_car(t2);
t20=C_eqp(t19,lf[59]);
if(C_truep(t20)){
C_trace("5.2:196: make-restore");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[60]+1)))(6,*((C_word*)lf[60]+1),t1,t2,t4,t7,t5);}
else{
t21=C_i_car(t2);
t22=C_eqp(t21,lf[61]);
if(C_truep(t22)){
C_trace("5.2:198: make-perform");
((C_proc7)C_fast_retrieve_proc(*((C_word*)lf[62]+1)))(7,*((C_word*)lf[62]+1),t1,t2,t4,t3,t8,t5);}
else{
C_trace("5.2:199: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[63],t2);}}}}}}}}

/* f_1285 in k1283 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1285(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1285,2,t0,t1);}
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}

/* f_493 in dispatch in make-register in k421 in k419 */
static void C_ccall f_493(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_493,3,t0,t1,t2);}
t3=C_mutate(((C_word *)((C_word*)t0)[2])+1,t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}

/* k1283 in k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1284(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1284,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1285,a[2]=t1,a[3]=((C_word)li64),tmp=(C_word)a,a+=4,tmp));}

/* k1280 in make-primitive-exp in k421 in k419 */
static void C_ccall f_1282(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1282,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1284,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("5.2:295: constant-exp-value");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[83]+1)))(3,*((C_word*)lf[83]+1),t2,((C_word*)t0)[3]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1293,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("5.2:297: label-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[74]+1)))(3,*((C_word*)lf[74]+1),t2,((C_word*)t0)[3]);}}

/* lookup-register in k603 in k601 in k599 in make-new-machine in k421 in k419 */
static void C_ccall f_632(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_632,3,t0,t1,t2);}
t3=C_i_assoc(t2,((C_word*)((C_word*)t0)[2])[1]);
if(C_truep(t3)){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_cadr(t3));}
else{
C_trace("5.2:91: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(4,*((C_word*)lf[11]+1),t1,lf[27],t2);}}

/* instruction-execution-proc in k421 in k419 */
static void C_ccall f_898(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_898,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* instruction-text in k421 in k419 */
static void C_ccall f_892(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_892,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* make-instruction in k421 in k419 */
static void C_ccall f_886(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_886,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,t2,C_SCHEME_END_OF_LIST));}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[185] = {
{"f_1269:_35_2e2",(void*)f_1269},
{"f_1261:_35_2e2",(void*)f_1261},
{"f_659:_35_2e2",(void*)f_659},
{"f_651:_35_2e2",(void*)f_651},
{"f_1129:_35_2e2",(void*)f_1129},
{"f_1121:_35_2e2",(void*)f_1121},
{"f_1122:_35_2e2",(void*)f_1122},
{"f_1057:_35_2e2",(void*)f_1057},
{"f_1051:_35_2e2",(void*)f_1051},
{"f_1295:_35_2e2",(void*)f_1295},
{"f_1296:_35_2e2",(void*)f_1296},
{"f_1293:_35_2e2",(void*)f_1293},
{"f_647:_35_2e2",(void*)f_647},
{"f_790:_35_2e2",(void*)f_790},
{"f_792:_35_2e2",(void*)f_792},
{"f_596:_35_2e2",(void*)f_596},
{"f_1049:_35_2e2",(void*)f_1049},
{"f_591:_35_2e2",(void*)f_591},
{"f_859:_35_2e2",(void*)f_859},
{"f_856:_35_2e2",(void*)f_856},
{"f_610:_35_2e2",(void*)f_610},
{"f_1037:_35_2e2",(void*)f_1037},
{"f_507:_35_2e2",(void*)f_507},
{"f_501:_35_2e2",(void*)f_501},
{"f_1030:_35_2e2",(void*)f_1030},
{"f_850:_35_2e2",(void*)f_850},
{"f_770:_35_2e2",(void*)f_770},
{"f_1119:_35_2e2",(void*)f_1119},
{"f_849:_35_2e2",(void*)f_849},
{"f_847:_35_2e2",(void*)f_847},
{"f_600:_35_2e2",(void*)f_600},
{"f_845:_35_2e2",(void*)f_845},
{"f_772:_35_2e2",(void*)f_772},
{"f_1113:_35_2e2",(void*)f_1113},
{"f_662:_35_2e2",(void*)f_662},
{"f_1029:_35_2e2",(void*)f_1029},
{"f_776:_35_2e2",(void*)f_776},
{"f_1027:_35_2e2",(void*)f_1027},
{"f_1025:_35_2e2",(void*)f_1025},
{"f_1021:_35_2e2",(void*)f_1021},
{"f_843:_35_2e2",(void*)f_843},
{"f_614:_35_2e2",(void*)f_614},
{"f_679:_35_2e2",(void*)f_679},
{"f_1169:_35_2e2",(void*)f_1169},
{"f_1109:_35_2e2",(void*)f_1109},
{"f_630:_35_2e2",(void*)f_630},
{"f_1103:_35_2e2",(void*)f_1103},
{"f_1161:_35_2e2",(void*)f_1161},
{"f_1162:_35_2e2",(void*)f_1162},
{"f_1092:_35_2e2",(void*)f_1092},
{"f_1098:_35_2e2",(void*)f_1098},
{"f_875:_35_2e2",(void*)f_875},
{"f_604:_35_2e2",(void*)f_604},
{"f_602:_35_2e2",(void*)f_602},
{"f_1159:_35_2e2",(void*)f_1159},
{"f_1451:_35_2e2",(void*)f_1451},
{"f_754:_35_2e2",(void*)f_754},
{"f_1453:_35_2e2",(void*)f_1453},
{"f_866:_35_2e2",(void*)f_866},
{"f_1153:_35_2e2",(void*)f_1153},
{"f_1087:_35_2e2",(void*)f_1087},
{"f_1088:_35_2e2",(void*)f_1088},
{"f_1085:_35_2e2",(void*)f_1085},
{"f_781:_35_2e2",(void*)f_781},
{"f_1149:_35_2e2",(void*)f_1149},
{"f_1445:_35_2e2",(void*)f_1445},
{"f_1143:_35_2e2",(void*)f_1143},
{"f_1079:_35_2e2",(void*)f_1079},
{"f_787:_35_2e2",(void*)f_787},
{"f_546:_35_2e2",(void*)f_546},
{"f_1073:_35_2e2",(void*)f_1073},
{"f_1075:_35_2e2",(void*)f_1075},
{"f_689:_35_2e2",(void*)f_689},
{"f_1431:_35_2e2",(void*)f_1431},
{"f_1138:_35_2e2",(void*)f_1138},
{"f_1063:_35_2e2",(void*)f_1063},
{"f_1420:_35_2e2",(void*)f_1420},
{"f_761:_35_2e2",(void*)f_761},
{"f_839:_35_2e2",(void*)f_839},
{"f_763:_35_2e2",(void*)f_763},
{"f_837:_35_2e2",(void*)f_837},
{"f_528:_35_2e2",(void*)f_528},
{"f_767:_35_2e2",(void*)f_767},
{"f_520:_35_2e2",(void*)f_520},
{"f_516:_35_2e2",(void*)f_516},
{"f_511:_35_2e2",(void*)f_511},
{"f_743:_35_2e2",(void*)f_743},
{"f_1483:_35_2e2",(void*)f_1483},
{"f_670:_35_2e2",(void*)f_670},
{"f_748:_35_2e2",(void*)f_748},
{"f_1199:_35_2e2",(void*)f_1199},
{"f_1193:_35_2e2",(void*)f_1193},
{"f_1188:_35_2e2",(void*)f_1188},
{"f_1185:_35_2e2",(void*)f_1185},
{"f_1477:_35_2e2",(void*)f_1477},
{"f_1175:_35_2e2",(void*)f_1175},
{"f_1178:_35_2e2",(void*)f_1178},
{"f_1177:_35_2e2",(void*)f_1177},
{"f_1515:_35_2e2",(void*)f_1515},
{"f_1512:_35_2e2",(void*)f_1512},
{"f_1518:_35_2e2",(void*)f_1518},
{"f_551:_35_2e2",(void*)f_551},
{"f_1510:_35_2e2",(void*)f_1510},
{"f_1468:_35_2e2",(void*)f_1468},
{"f_1502:_35_2e2",(void*)f_1502},
{"f_1504:_35_2e2",(void*)f_1504},
{"f_1506:_35_2e2",(void*)f_1506},
{"f_1508:_35_2e2",(void*)f_1508},
{"f_1500:_35_2e2",(void*)f_1500},
{"f_1346:_35_2e2",(void*)f_1346},
{"f_1340:_35_2e2",(void*)f_1340},
{"f_1310:_35_2e2",(void*)f_1310},
{"f_1317:_35_2e2",(void*)f_1317},
{"f_1328:_35_2e2",(void*)f_1328},
{"f_1322:_35_2e2",(void*)f_1322},
{"toplevel:_35_2e2",(void*)C_toplevel},
{"f_1415:_35_2e2",(void*)f_1415},
{"f_1358:_35_2e2",(void*)f_1358},
{"f_1352:_35_2e2",(void*)f_1352},
{"f_1401:_35_2e2",(void*)f_1401},
{"f_1388:_35_2e2",(void*)f_1388},
{"f_1380:_35_2e2",(void*)f_1380},
{"f_1334:_35_2e2",(void*)f_1334},
{"f_1368:_35_2e2",(void*)f_1368},
{"f_1363:_35_2e2",(void*)f_1363},
{"f_1362:_35_2e2",(void*)f_1362},
{"f_1390:_35_2e2",(void*)f_1390},
{"f_587:_35_2e2",(void*)f_587},
{"f_1374:_35_2e2",(void*)f_1374},
{"f_1373:_35_2e2",(void*)f_1373},
{"f_581:_35_2e2",(void*)f_581},
{"f_1236:_35_2e2",(void*)f_1236},
{"f_1238:_35_2e2",(void*)f_1238},
{"f_1233:_35_2e2",(void*)f_1233},
{"f_432:_35_2e2",(void*)f_432},
{"f_1206:_35_2e2",(void*)f_1206},
{"f_1205:_35_2e2",(void*)f_1205},
{"f_1203:_35_2e2",(void*)f_1203},
{"f_429:_35_2e2",(void*)f_429},
{"f_428:_35_2e2",(void*)f_428},
{"f_424:_35_2e2",(void*)f_424},
{"f_422:_35_2e2",(void*)f_422},
{"f_420:_35_2e2",(void*)f_420},
{"f_810:_35_2e2",(void*)f_810},
{"f_455:_35_2e2",(void*)f_455},
{"f_453:_35_2e2",(void*)f_453},
{"f_450:_35_2e2",(void*)f_450},
{"f_910:_35_2e2",(void*)f_910},
{"f_916:_35_2e2",(void*)f_916},
{"f_1301:_35_2e2",(void*)f_1301},
{"f_1307:_35_2e2",(void*)f_1307},
{"f_1248:_35_2e2",(void*)f_1248},
{"f_1309:_35_2e2",(void*)f_1309},
{"f_1244:_35_2e2",(void*)f_1244},
{"f_445:_35_2e2",(void*)f_445},
{"f_447:_35_2e2",(void*)f_447},
{"f_442:_35_2e2",(void*)f_442},
{"f_440:_35_2e2",(void*)f_440},
{"f_1218:_35_2e2",(void*)f_1218},
{"f_1216:_35_2e2",(void*)f_1216},
{"f_1213:_35_2e2",(void*)f_1213},
{"f_904:_35_2e2",(void*)f_904},
{"f_828:_35_2e2",(void*)f_828},
{"f_1275:_35_2e2",(void*)f_1275},
{"f_711:_35_2e2",(void*)f_711},
{"f_717:_35_2e2",(void*)f_717},
{"f_1227:_35_2e2",(void*)f_1227},
{"f_1223:_35_2e2",(void*)f_1223},
{"f_1222:_35_2e2",(void*)f_1222},
{"f_1256:_35_2e2",(void*)f_1256},
{"f_1257:_35_2e2",(void*)f_1257},
{"f_1254:_35_2e2",(void*)f_1254},
{"f_475:_35_2e2",(void*)f_475},
{"f_479:_35_2e2",(void*)f_479},
{"f_464:_35_2e2",(void*)f_464},
{"f_931:_35_2e2",(void*)f_931},
{"f_1285:_35_2e2",(void*)f_1285},
{"f_493:_35_2e2",(void*)f_493},
{"f_1284:_35_2e2",(void*)f_1284},
{"f_1282:_35_2e2",(void*)f_1282},
{"f_632:_35_2e2",(void*)f_632},
{"f_898:_35_2e2",(void*)f_898},
{"f_892:_35_2e2",(void*)f_892},
{"f_886:_35_2e2",(void*)f_886},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
S|applied compiler syntax:
S|  map		2
S|  for-each		2
o|eliminated procedure checks: 10 
o|safe globals: (lookup-prim operation-exp-operands operation-exp-op operation-exp? make-operation-exp label-exp-label label-exp? constant-exp-value constant-exp? register-exp-reg register-exp? make-primitive-exp perform-action make-perform stack-inst-reg-name make-restore make-save goto-dest make-goto branch-dest make-branch test-condition make-test advance-pc assign-value-exp assign-reg-name make-assign make-execution-procedure lookup-label make-label-entry set-instruction-execution-proc! instruction-execution-proc instruction-text make-instruction update-insts! extract-labels assemble get-register set-register-contents! get-register-contents start make-new-machine push pop make-stack set-contents! get-contents make-register make-machine) 
o|substituted constant variable: the-instruction-sequence78 
o|replaced variables: 171 
o|removed binding forms: 71 
o|converted assignments to bindings: (dispatch35) 
o|simplifications: ((let . 1)) 
o|removed binding forms: 131 
o|simplifications: ((##core#call . 98)) 
o|  call simplifications:
o|    ##sys#setslot	2
o|    apply
o|    cddr
o|    set-cdr!
o|    symbol?
o|    cadr	10
o|    assoc	4
o|    list	9
o|    null?	3
o|    car	14
o|    cdr	8
o|    cons	8
o|    eq?	19
o|    ##sys#check-list	4
o|    pair?	5
o|    ##sys#slot	8
o|contracted procedure: k436 
o|contracted procedure: k460 
o|contracted procedure: k469 
o|contracted procedure: k472 
o|contracted procedure: k484 
o|contracted procedure: k490 
o|contracted procedure: k524 
o|contracted procedure: k533 
o|contracted procedure: k539 
o|contracted procedure: k542 
o|contracted procedure: k556 
o|contracted procedure: k562 
o|contracted procedure: k571 
o|contracted procedure: k740 
o|contracted procedure: k605 
o|contracted procedure: k734 
o|contracted procedure: k737 
o|contracted procedure: k607 
o|contracted procedure: k615 
o|contracted procedure: k626 
o|contracted procedure: k622 
o|contracted procedure: k635 
o|contracted procedure: k655 
o|contracted procedure: k667 
o|contracted procedure: k675 
o|contracted procedure: k686 
o|contracted procedure: k696 
o|contracted procedure: k702 
o|contracted procedure: k708 
o|contracted procedure: k721 
o|contracted procedure: k727 
o|contracted procedure: k797 
o|contracted procedure: k807 
o|contracted procedure: k812 
o|contracted procedure: k817 
o|contracted procedure: k824 
o|contracted procedure: k833 
o|contracted procedure: k860 
o|contracted procedure: k871 
o|contracted procedure: k880 
o|contracted procedure: k883 
o|contracted procedure: k919 
o|contracted procedure: k1018 
o|contracted procedure: k936 
o|contracted procedure: k1015 
o|contracted procedure: k945 
o|contracted procedure: k1012 
o|contracted procedure: k954 
o|contracted procedure: k1009 
o|contracted procedure: k963 
o|contracted procedure: k1006 
o|contracted procedure: k972 
o|contracted procedure: k1003 
o|contracted procedure: k981 
o|contracted procedure: k1000 
o|contracted procedure: k990 
o|contracted procedure: k1045 
o|contracted procedure: k1069 
o|contracted procedure: k1369 
o|contracted procedure: k1384 
o|contracted procedure: k1395 
o|contracted procedure: k1398 
o|contracted procedure: k1407 
o|contracted procedure: k1417 
o|contracted procedure: k1425 
o|contracted procedure: k1428 
o|contracted procedure: k1437 
o|contracted procedure: k1447 
o|contracted procedure: k1458 
o|contracted procedure: k1465 
o|contracted procedure: k1474 
o|contracted procedure: k1486 
o|contracted procedure: k1523 
o|contracted procedure: k1526 
o|contracted procedure: k1520 
o|simplifications: ((let . 10)) 
o|removed binding forms: 75 
o|customizable procedures: (k1430 map-loop292310 k1400 map-loop319337 for-each-loop141152 execute83 initialize53 pop52 for-each-loop720) 
o|calls to known targets: 17 
o|fast box initializations: 10 
*/
/* end of file */
