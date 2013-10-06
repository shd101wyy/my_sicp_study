/* Generated from 4.1 by the CHICKEN compiler
   http://www.call-cc.org
   2013-10-06 16:59
   Version 4.8.0.4 (stability/4.8.0) (rev 578619b)
   macosx-unix-clang-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2013-07-15 on aeryn.xorinia.dim (Darwin)
   command line: 4.1
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[125];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,26),40,101,118,97,108,45,115,101,113,117,101,110,99,101,32,101,120,112,115,51,32,101,110,118,52,41,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,29),40,101,118,97,108,45,97,115,115,105,103,110,109,101,110,116,32,101,120,112,49,48,32,101,110,118,49,49,41,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,29),40,101,118,97,108,45,100,101,102,105,110,105,116,105,111,110,32,101,120,112,49,52,32,101,110,118,49,53,41,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,24),40,115,101,108,102,45,101,118,97,108,117,97,116,105,110,103,63,32,101,120,112,49,56,41};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,17),40,118,97,114,105,97,98,108,101,63,32,101,120,112,50,51,41,0,0,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,15),40,113,117,111,116,101,100,63,32,101,120,112,50,53,41,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,12),40,99,97,100,114,32,101,120,112,50,55,41,0,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,13),40,99,97,100,100,114,32,101,120,112,50,57,41,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,13),40,99,97,97,100,114,32,101,120,112,51,49,41,0,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,13),40,99,100,97,100,114,32,101,120,112,51,51,41,0,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,12),40,99,100,100,114,32,101,120,112,51,53,41,0,0,0,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,13),40,99,100,100,100,114,32,101,120,112,51,55,41,0,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,14),40,99,97,100,100,100,114,32,101,120,112,51,57,41,0,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,25),40,116,101,120,116,45,111,102,45,113,117,111,116,97,116,105,111,110,32,101,120,112,52,49,41,0,0,0,0,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,26),40,116,97,103,103,101,100,45,108,105,115,116,63,32,101,120,112,52,51,32,116,97,103,52,52,41,0,0,0,0,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,19),40,97,115,115,105,103,110,109,101,110,116,63,32,101,120,112,52,54,41,0,0,0,0,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,27),40,97,115,115,105,103,110,109,101,110,116,45,118,97,114,105,97,98,108,101,32,101,120,112,52,56,41,0,0,0,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,24),40,97,115,115,105,103,110,109,101,110,116,45,118,97,108,117,101,32,101,120,112,53,48,41};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,19),40,100,101,102,105,110,105,116,105,111,110,63,32,101,120,112,53,50,41,0,0,0,0,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,27),40,100,101,102,105,110,105,116,105,111,110,45,118,97,114,105,97,98,108,101,32,101,120,112,53,52,41,0,0,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,24),40,100,101,102,105,110,105,116,105,111,110,45,118,97,108,117,101,32,101,120,112,53,54,41};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,15),40,108,97,109,98,100,97,63,32,101,120,112,53,56,41,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,25),40,108,97,109,98,100,97,45,112,97,114,97,109,101,116,101,114,115,32,101,120,112,54,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,19),40,108,97,109,98,100,97,45,98,111,100,121,32,101,120,112,54,50,41,0,0,0,0,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,33),40,109,97,107,101,45,108,97,109,98,100,97,32,112,97,114,97,109,101,116,101,114,115,54,52,32,98,111,100,121,54,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,11),40,105,102,63,32,101,120,112,54,55,41,0,0,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,20),40,105,102,45,112,114,101,100,105,99,97,116,101,32,101,120,112,54,57,41,0,0,0,0};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,21),40,105,102,45,99,111,110,115,101,113,117,101,110,116,32,101,120,112,55,49,41,0,0,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,22),40,105,102,45,97,108,116,101,114,110,97,116,105,118,101,32,101,120,112,55,51,41,0,0};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,48),40,109,97,107,101,45,105,102,32,112,114,101,100,105,99,97,116,101,55,53,32,99,111,110,115,101,113,117,101,110,116,55,54,32,97,108,116,101,114,110,97,116,105,118,101,55,55,41};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,14),40,98,101,103,105,110,63,32,101,120,112,55,57,41,0,0};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,21),40,98,101,103,105,110,45,97,99,116,105,111,110,115,32,101,120,112,56,49,41,0,0,0};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,17),40,108,97,115,116,45,101,120,112,63,32,115,101,113,56,51,41,0,0,0,0,0,0,0};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,17),40,102,105,114,115,116,45,101,120,112,32,115,101,113,56,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,17),40,114,101,115,116,45,101,120,112,115,32,115,101,113,56,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,21),40,115,101,113,117,101,110,99,101,45,62,101,120,112,32,115,101,113,56,57,41,0,0,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,18),40,109,97,107,101,45,98,101,103,105,110,32,115,101,113,57,52,41,0,0,0,0,0,0};
static C_char C_TLS li37[] C_aligned={C_lihdr(0,0,20),40,97,112,112,108,105,99,97,116,105,111,110,63,32,101,120,112,57,54,41,0,0,0,0};
static C_char C_TLS li38[] C_aligned={C_lihdr(0,0,16),40,111,112,101,114,97,116,111,114,32,101,120,112,57,56,41};
static C_char C_TLS li39[] C_aligned={C_lihdr(0,0,17),40,111,112,101,114,97,110,100,115,32,101,120,112,49,48,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li40[] C_aligned={C_lihdr(0,0,21),40,110,111,45,111,112,101,114,97,110,100,115,63,32,111,112,115,49,48,50,41,0,0,0};
static C_char C_TLS li41[] C_aligned={C_lihdr(0,0,22),40,102,105,114,115,116,45,111,112,101,114,97,110,100,32,111,112,115,49,48,52,41,0,0};
static C_char C_TLS li42[] C_aligned={C_lihdr(0,0,22),40,114,101,115,116,45,111,112,101,114,97,110,100,115,32,111,112,115,49,48,54,41,0,0};
static C_char C_TLS li43[] C_aligned={C_lihdr(0,0,14),40,99,111,110,100,63,32,101,120,112,49,48,56,41,0,0};
static C_char C_TLS li44[] C_aligned={C_lihdr(0,0,21),40,99,111,110,100,45,99,108,97,117,115,101,115,32,101,120,112,49,49,48,41,0,0,0};
static C_char C_TLS li45[] C_aligned={C_lihdr(0,0,29),40,99,111,110,100,45,101,108,115,101,45,99,108,97,117,115,101,115,32,99,108,97,117,115,101,49,49,50,41,0,0,0};
static C_char C_TLS li46[] C_aligned={C_lihdr(0,0,26),40,99,111,110,100,45,112,114,101,100,105,99,97,116,101,32,99,108,97,117,115,101,49,49,52,41,0,0,0,0,0,0};
static C_char C_TLS li47[] C_aligned={C_lihdr(0,0,24),40,99,111,110,100,45,97,99,116,105,111,110,115,32,99,108,97,117,115,101,49,49,54,41};
static C_char C_TLS li48[] C_aligned={C_lihdr(0,0,17),40,99,111,110,100,45,62,105,102,32,101,120,112,49,49,56,41,0,0,0,0,0,0,0};
static C_char C_TLS li49[] C_aligned={C_lihdr(0,0,27),40,101,120,112,97,110,100,45,99,108,97,117,115,101,115,32,99,108,97,117,115,101,115,49,50,48,41,0,0,0,0,0};
static C_char C_TLS li50[] C_aligned={C_lihdr(0,0,12),40,116,114,117,101,63,32,120,49,50,52,41,0,0,0,0};
static C_char C_TLS li51[] C_aligned={C_lihdr(0,0,13),40,102,97,108,115,101,63,32,120,49,50,54,41,0,0,0};
static C_char C_TLS li52[] C_aligned={C_lihdr(0,0,45),40,109,97,107,101,45,112,114,111,99,101,100,117,114,101,32,112,97,114,97,109,101,116,101,114,115,49,50,56,32,98,111,100,121,49,50,57,32,101,110,118,49,51,48,41,0,0,0};
static C_char C_TLS li53[] C_aligned={C_lihdr(0,0,26),40,99,111,109,112,111,117,110,100,45,112,114,111,99,101,100,117,114,101,63,32,112,49,51,50,41,0,0,0,0,0,0};
static C_char C_TLS li54[] C_aligned={C_lihdr(0,0,27),40,112,114,111,99,101,100,117,114,101,45,112,97,114,97,109,101,116,101,114,115,32,112,49,51,52,41,0,0,0,0,0};
static C_char C_TLS li55[] C_aligned={C_lihdr(0,0,21),40,112,114,111,99,101,100,117,114,101,45,98,111,100,121,32,112,49,51,54,41,0,0,0};
static C_char C_TLS li56[] C_aligned={C_lihdr(0,0,28),40,112,114,111,99,101,100,117,114,101,45,101,110,118,105,114,111,110,109,101,110,116,32,112,49,51,56,41,0,0,0,0};
static C_char C_TLS li57[] C_aligned={C_lihdr(0,0,30),40,101,110,99,108,111,115,105,110,103,45,101,110,118,105,114,111,110,109,101,110,116,32,101,110,118,49,52,48,41,0,0};
static C_char C_TLS li58[] C_aligned={C_lihdr(0,0,20),40,102,105,114,115,116,45,102,114,97,109,101,32,101,110,118,49,52,50,41,0,0,0,0};
static C_char C_TLS li59[] C_aligned={C_lihdr(0,0,22),40,116,104,101,45,101,109,112,116,121,45,101,110,118,32,101,110,118,49,52,52,41,0,0};
static C_char C_TLS li60[] C_aligned={C_lihdr(0,0,35),40,109,97,107,101,45,102,114,97,109,101,32,118,97,114,105,97,98,108,101,115,49,52,54,32,118,97,108,117,101,115,49,52,55,41,0,0,0,0,0};
static C_char C_TLS li61[] C_aligned={C_lihdr(0,0,26),40,102,114,97,109,101,45,118,97,114,105,97,98,108,101,115,32,102,114,97,109,101,49,52,57,41,0,0,0,0,0,0};
static C_char C_TLS li62[] C_aligned={C_lihdr(0,0,23),40,102,114,97,109,101,45,118,97,108,117,101,115,32,102,114,97,109,101,49,53,49,41,0};
static C_char C_TLS li63[] C_aligned={C_lihdr(0,0,46),40,97,100,100,45,98,105,110,100,105,110,103,45,116,111,45,102,114,97,109,101,33,32,118,97,114,49,53,51,32,118,97,108,49,53,52,32,102,114,97,109,101,49,53,53,41,0,0};
static C_char C_TLS li64[] C_aligned={C_lihdr(0,0,48),40,101,120,116,101,110,100,45,101,110,118,105,114,111,110,109,101,110,116,32,118,97,114,115,49,53,56,32,118,97,108,115,49,53,57,32,98,97,115,101,45,101,110,118,49,54,48,41};
static C_char C_TLS li65[] C_aligned={C_lihdr(0,0,22),40,115,99,97,110,32,118,97,114,115,49,54,55,32,118,97,108,115,49,54,56,41,0,0};
static C_char C_TLS li66[] C_aligned={C_lihdr(0,0,17),40,101,110,118,45,108,111,111,112,32,101,110,118,49,54,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li67[] C_aligned={C_lihdr(0,0,38),40,108,111,111,107,45,117,112,45,118,97,114,105,97,98,108,101,45,118,97,108,117,101,32,118,97,114,49,54,50,32,101,110,118,49,54,51,41,0,0};
static C_char C_TLS li68[] C_aligned={C_lihdr(0,0,22),40,115,99,97,110,32,118,97,114,115,49,56,50,32,118,97,108,115,49,56,51,41,0,0};
static C_char C_TLS li69[] C_aligned={C_lihdr(0,0,17),40,101,110,118,45,108,111,111,112,32,101,110,118,49,56,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li70[] C_aligned={C_lihdr(0,0,42),40,115,101,116,45,118,97,114,105,97,98,108,101,45,118,97,108,117,101,33,32,118,97,114,49,55,54,32,118,97,108,49,55,55,32,101,110,118,49,55,56,41,0,0,0,0,0,0};
static C_char C_TLS li71[] C_aligned={C_lihdr(0,0,22),40,115,99,97,110,32,118,97,114,115,49,57,54,32,118,97,108,115,49,57,55,41,0,0};
static C_char C_TLS li72[] C_aligned={C_lihdr(0,0,39),40,100,101,102,105,110,101,45,118,97,114,105,97,98,108,101,33,32,118,97,114,49,57,49,32,118,97,108,49,57,50,32,101,110,118,49,57,51,41,0};
static C_char C_TLS li73[] C_aligned={C_lihdr(0,0,19),40,115,101,116,117,112,45,101,110,118,105,114,111,110,109,101,110,116,41,0,0,0,0,0};
static C_char C_TLS li74[] C_aligned={C_lihdr(0,0,30),40,112,114,105,109,105,116,105,118,101,45,112,114,111,99,101,100,117,114,101,63,32,112,114,111,99,50,48,56,41,0,0};
static C_char C_TLS li75[] C_aligned={C_lihdr(0,0,34),40,112,114,105,109,105,116,105,118,101,45,105,109,112,108,101,109,101,110,116,97,116,105,111,110,32,112,114,111,99,50,49,48,41,0,0,0,0,0,0};
static C_char C_TLS li76[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,50,49,53,32,103,50,50,55,50,51,51,41,0,0,0};
static C_char C_TLS li77[] C_aligned={C_lihdr(0,0,27),40,112,114,105,109,105,116,105,118,101,45,112,114,111,99,101,100,117,114,101,45,110,97,109,101,115,41,0,0,0,0,0};
static C_char C_TLS li78[] C_aligned={C_lihdr(0,0,16),40,102,95,49,51,50,48,32,112,114,111,99,50,53,57,41};
static C_char C_TLS li79[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,50,52,50,32,103,50,53,52,50,54,49,41,0,0,0};
static C_char C_TLS li80[] C_aligned={C_lihdr(0,0,29),40,112,114,105,109,105,116,105,118,101,45,112,114,111,99,101,100,117,114,101,45,111,98,106,101,99,116,115,41,0,0,0};
static C_char C_TLS li81[] C_aligned={C_lihdr(0,0,43),40,97,112,112,108,121,45,112,114,105,109,105,116,105,118,101,45,112,114,111,99,101,100,117,114,101,32,112,114,111,99,50,54,57,32,97,114,103,115,50,55,48,41,0,0,0,0,0};
static C_char C_TLS li82[] C_aligned={C_lihdr(0,0,13),40,100,114,105,118,101,114,45,108,111,111,112,41,0,0,0};
static C_char C_TLS li83[] C_aligned={C_lihdr(0,0,28),40,112,114,111,109,112,116,45,102,111,114,45,105,110,112,117,116,32,115,116,114,105,110,103,50,56,48,41,0,0,0,0};
static C_char C_TLS li84[] C_aligned={C_lihdr(0,0,27),40,97,110,110,111,117,110,99,101,45,111,117,116,112,117,116,32,115,116,114,105,110,103,50,56,53,41,0,0,0,0,0};
static C_char C_TLS li85[] C_aligned={C_lihdr(0,0,22),40,117,115,101,114,45,112,114,105,110,116,32,111,98,106,101,99,116,50,56,57,41,0,0};
static C_char C_TLS li86[] C_aligned={C_lihdr(0,0,31),40,108,105,115,116,45,111,102,45,118,97,108,117,101,115,32,101,120,112,115,50,57,56,32,101,110,118,50,57,57,41,0};
static C_char C_TLS li87[] C_aligned={C_lihdr(0,0,20),40,101,118,97,108,32,101,120,112,50,57,49,32,101,110,118,50,57,50,41,0,0,0,0};
static C_char C_TLS li88[] C_aligned={C_lihdr(0,0,33),40,97,112,112,108,121,32,112,114,111,99,101,100,117,114,101,51,48,53,32,97,114,103,117,109,101,110,116,115,51,48,54,41,0,0,0,0,0,0,0};
static C_char C_TLS li89[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_1524)
static void C_ccall f_1524(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_728)
static void C_ccall f_728(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1515)
static void C_ccall f_1515(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1083)
static void C_ccall f_1083(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1089)
static void C_ccall f_1089(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_734)
static void C_ccall f_734(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_710)
static void C_ccall f_710(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_465)
static void C_ccall f_465(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1542)
static void C_ccall f_1542(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_462)
static void C_ccall f_462(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_468)
static void C_ccall f_468(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_704)
static void C_ccall f_704(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_901)
static void C_ccall f_901(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_921)
static void C_ccall f_921(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1180)
static void C_ccall f_1180(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1186)
static void C_ccall f_1186(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1189)
static void C_ccall f_1189(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_455)
static void C_ccall f_455(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_453)
static void C_ccall f_453(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1533)
static void C_ccall f_1533(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_450)
static void C_ccall f_450(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_459)
static void C_ccall f_459(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1060)
static void C_ccall f_1060(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1064)
static void C_fcall f_1064(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1068)
static void C_fcall f_1068(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_954)
static void C_ccall f_954(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1101)
static void C_ccall f_1101(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1560)
static void C_ccall f_1560(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1566)
static void C_ccall f_1566(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1551)
static void C_ccall f_1551(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1557)
static void C_ccall f_1557(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_936)
static void C_ccall f_936(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_542)
static void C_ccall f_542(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_930)
static void C_ccall f_930(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1127)
static void C_ccall f_1127(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1584)
static void C_ccall f_1584(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1072)
static void C_ccall f_1072(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1287)
static void C_fcall f_1287(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_966)
static void C_ccall f_966(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_530)
static void C_ccall f_530(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_960)
static void C_ccall f_960(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1572)
static void C_ccall f_1572(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1578)
static void C_ccall f_1578(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1021)
static void C_ccall f_1021(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1278)
static void C_ccall f_1278(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_994)
static void C_ccall f_994(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1148)
static void C_ccall f_1148(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1262)
static void C_ccall f_1262(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1260)
static void C_ccall f_1260(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1268)
static void C_ccall f_1268(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_948)
static void C_ccall f_948(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_599)
static void C_ccall f_599(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_942)
static void C_ccall f_942(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1135)
static void C_fcall f_1135(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1131)
static void C_fcall f_1131(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1596)
static void C_ccall f_1596(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1599)
static void C_ccall f_1599(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1253)
static void C_ccall f_1253(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1590)
static void C_ccall f_1590(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1250)
static void C_ccall f_1250(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1256)
static void C_ccall f_1256(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_584)
static void C_ccall f_584(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_978)
static void C_ccall f_978(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_972)
static void C_ccall f_972(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1246)
static void C_ccall f_1246(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1240)
static void C_ccall f_1240(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1242)
static void C_ccall f_1242(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1248)
static void C_ccall f_1248(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_578)
static void C_ccall f_578(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1237)
static void C_ccall f_1237(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_683)
static void C_ccall f_683(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_988)
static void C_ccall f_988(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_982)
static void C_ccall f_982(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1399)
static void C_ccall f_1399(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1397)
static void C_ccall f_1397(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1393)
static void C_ccall f_1393(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_671)
static void C_ccall f_671(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_677)
static void C_ccall f_677(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_665)
static void C_ccall f_665(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1092)
static void C_ccall f_1092(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_518)
static void C_ccall f_518(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_657)
static void C_ccall f_657(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_910)
static void C_ccall f_910(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_919)
static void C_ccall f_919(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_916)
static void C_ccall f_916(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_913)
static void C_ccall f_913(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_506)
static void C_ccall f_506(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_812)
static void C_ccall f_812(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_818)
static void C_ccall f_818(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1358)
static void C_ccall f_1358(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_692)
static void C_ccall f_692(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_698)
static void C_ccall f_698(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1381)
static void C_ccall f_1381(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1383)
static void C_ccall f_1383(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1385)
static void C_ccall f_1385(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1388)
static void C_ccall f_1388(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_563)
static void C_ccall f_563(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1371)
static void C_ccall f_1371(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1375)
static void C_ccall f_1375(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1379)
static void C_ccall f_1379(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_551)
static void C_ccall f_551(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1452)
static void C_ccall f_1452(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1458)
static void C_ccall f_1458(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1441)
static void C_ccall f_1441(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1445)
static void C_fcall f_1445(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1317)
static void C_ccall f_1317(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1433)
static void C_ccall f_1433(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1436)
static void C_ccall f_1436(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1344)
static void C_fcall f_1344(C_word t0,C_word t1) C_noret;
C_noret_decl(f_641)
static void C_ccall f_641(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1298)
static void C_fcall f_1298(C_word t0,C_word t1) C_noret;
C_noret_decl(f_1424)
static void C_ccall f_1424(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1333)
static void C_fcall f_1333(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1198)
static void C_ccall f_1198(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1194)
static void C_ccall f_1194(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1410)
static void C_ccall f_1410(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1412)
static void C_ccall f_1412(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1417)
static void C_ccall f_1417(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1364)
static void C_ccall f_1364(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_849)
static void C_ccall f_849(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1401)
static void C_ccall f_1401(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_842)
static void C_ccall f_842(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1406)
static void C_ccall f_1406(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_429)
static void C_ccall f_429(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_427)
static void C_ccall f_427(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_421)
static void C_ccall f_421(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_836)
static void C_ccall f_836(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_830)
static void C_ccall f_830(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1000)
static void C_ccall f_1000(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_1602)
static void C_ccall f_1602(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1605)
static void C_ccall f_1605(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_414)
static void C_ccall f_414(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_410)
static void C_ccall f_410(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1034)
static void C_ccall f_1034(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_824)
static void C_ccall f_824(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1320)
static void C_ccall f_1320(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_408)
static void C_ccall f_408(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_444)
static void C_ccall f_444(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_440)
static void C_ccall f_440(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_447)
static void C_ccall f_447(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_605)
static void C_ccall f_605(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_889)
static void C_ccall f_889(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_435)
static void C_ccall f_435(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1652)
static void C_ccall f_1652(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1650)
static void C_ccall f_1650(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_438)
static void C_ccall f_438(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1655)
static void C_ccall f_1655(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_872)
static void C_ccall f_872(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_870)
static void C_ccall f_870(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_761)
static void C_ccall f_761(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_767)
static void C_ccall f_767(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_491)
static void C_ccall f_491(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_497)
static void C_ccall f_497(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_863)
static void C_ccall f_863(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1200)
static void C_fcall f_1200(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1632)
static void C_ccall f_1632(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1635)
static void C_ccall f_1635(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1638)
static void C_ccall f_1638(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_755)
static void C_ccall f_755(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_857)
static void C_ccall f_857(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_485)
static void C_ccall f_485(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_851)
static void C_ccall f_851(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1494)
static void C_ccall f_1494(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1626)
static void C_ccall f_1626(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1491)
static void C_ccall f_1491(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_746)
static void C_ccall f_746(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_740)
static void C_ccall f_740(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_470)
static void C_ccall f_470(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1482)
static void C_ccall f_1482(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1610)
static void C_ccall f_1610(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_1488)
static void C_ccall f_1488(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1617)
static void C_ccall f_1617(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1476)
static void C_ccall f_1476(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_623)
static void C_ccall f_623(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1461)
static void C_ccall f_1461(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1467)
static void C_ccall f_1467(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1464)
static void C_ccall f_1464(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_794)
static void C_ccall f_794(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_617)
static void C_ccall f_617(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_611)
static void C_ccall f_611(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_780)
static void C_ccall f_780(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_788)
static void C_ccall f_788(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_800)
static void C_ccall f_800(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_806)
static void C_ccall f_806(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_1500)
static void C_ccall f_1500(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1641)
static void C_ccall f_1641(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1506)
static void C_ccall f_1506(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_1648)
static void C_ccall f_1648(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_1064)
static void C_fcall trf_1064(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1064(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1064(t0,t1,t2);}

C_noret_decl(trf_1068)
static void C_fcall trf_1068(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1068(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_1068(t0,t1,t2,t3);}

C_noret_decl(trf_1287)
static void C_fcall trf_1287(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1287(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1287(t0,t1,t2);}

C_noret_decl(trf_1135)
static void C_fcall trf_1135(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1135(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_1135(t0,t1,t2,t3);}

C_noret_decl(trf_1131)
static void C_fcall trf_1131(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1131(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1131(t0,t1,t2);}

C_noret_decl(trf_1445)
static void C_fcall trf_1445(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1445(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_1445(t0,t1,t2,t3);}

C_noret_decl(trf_1344)
static void C_fcall trf_1344(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1344(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_1344(t0,t1);}

C_noret_decl(trf_1298)
static void C_fcall trf_1298(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1298(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_1298(t0,t1);}

C_noret_decl(trf_1333)
static void C_fcall trf_1333(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1333(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_1333(t0,t1,t2);}

C_noret_decl(trf_1200)
static void C_fcall trf_1200(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_1200(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_1200(t0,t1,t2,t3);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

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

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

/* k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1524(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1524,2,t0,t1);}
if(C_truep(t1)){
C_trace("4.1:363: eval-assignment");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[7]+1)))(4,*((C_word*)lf[7]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1533,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:364: definition?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(3,*((C_word*)lf[31]+1),t2,((C_word*)t0)[3]);}}

/* make-if in k409 in k407 */
static void C_ccall f_728(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word ab[12],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_728,5,t0,t1,t2,t3,t4);}
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_list4(&a,4,lf[40],t2,t3,t4));}

/* k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1515(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1515,2,t0,t1);}
if(C_truep(t1)){
C_trace("4.1:362: text-of-quotation");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[28]+1)))(3,*((C_word*)lf[28]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1524,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:363: assignment?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[29]+1)))(3,*((C_word*)lf[29]+1),t2,((C_word*)t0)[3]);}}

/* k1082 in k1071 in scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_ccall f_1083(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1083,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1089,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:255: frame-variables");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[80]+1)))(3,*((C_word*)lf[80]+1),t2,t1);}

/* k1088 in k1082 in k1071 in scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_ccall f_1089(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1089,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1092,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:256: frame-values");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[81]+1)))(3,*((C_word*)lf[81]+1),t2,((C_word*)t0)[4]);}

/* begin? in k409 in k407 */
static void C_ccall f_734(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_734,3,t0,t1,t2);}
C_trace("4.1:125: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[46]);}

/* if-alternative in k409 in k407 */
static void C_ccall f_710(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_710,3,t0,t1,t2);}
t3=C_i_cdddr(t2);
t4=C_i_nullp(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_truep(t4)?lf[1]:C_i_cadddr(t2)));}

/* k464 in k461 in eval-definition in k409 in k407 */
static void C_ccall f_465(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:33: define-variable!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(5,*((C_word*)lf[13]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1542(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1542,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];
t3=((C_word*)t0)[3];
t4=((C_word*)t0)[4];
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1476,a[2]=t2,a[3]=t4,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1491,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1494,a[2]=t6,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:355: if-predicate");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[41]+1)))(3,*((C_word*)lf[41]+1),t7,t3);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1551,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:366: lambda?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(3,*((C_word*)lf[34]+1),t2,((C_word*)t0)[3]);}}

/* k461 in eval-definition in k409 in k407 */
static void C_ccall f_462(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_462,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_465,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_468,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:34: definition-value");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(3,*((C_word*)lf[14]+1),t3,((C_word*)t0)[4]);}

/* k467 in k461 in eval-definition in k409 in k407 */
static void C_ccall f_468(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:34: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* if-consequent in k409 in k407 */
static void C_ccall f_704(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_704,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* k900 in k887 in expand-clauses in k409 in k407 */
static void C_ccall f_901(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:169: sequence->exp");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[48]+1)))(3,*((C_word*)lf[48]+1),((C_word*)t0)[2],t1);}

/* true? in k409 in k407 */
static void C_ccall f_921(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_921,3,t0,t1,t2);}
t3=C_eqp(t2,*((C_word*)lf[1]+1));
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_not(t3));}

/* k1179 in env-loop in set-variable-value! in k409 in k407 */
static void C_ccall f_1180(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1180,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1186,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:270: frame-variables");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[80]+1)))(3,*((C_word*)lf[80]+1),t2,t1);}

/* k1185 in k1179 in env-loop in set-variable-value! in k409 in k407 */
static void C_ccall f_1186(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1186,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1189,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:271: frame-values");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[81]+1)))(3,*((C_word*)lf[81]+1),t2,((C_word*)t0)[4]);}

/* k1188 in k1185 in k1179 in env-loop in set-variable-value! in k409 in k407 */
static void C_ccall f_1189(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:270: scan");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1135(t2,((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* eval-definition in k409 in k407 */
static void C_ccall f_455(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[8],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_455,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_459,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_462,a[2]=t4,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:33: definition-variable");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(3,*((C_word*)lf[15]+1),t5,t2);}

/* k452 in k446 in eval-assignment in k409 in k407 */
static void C_ccall f_453(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:27: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1533(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1533,2,t0,t1);}
if(C_truep(t1)){
C_trace("4.1:364: eval-definition");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[12]+1)))(4,*((C_word*)lf[12]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1542,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:365: if?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[39]+1)))(3,*((C_word*)lf[39]+1),t2,((C_word*)t0)[3]);}}

/* k449 in k446 in eval-assignment in k409 in k407 */
static void C_ccall f_450(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:26: set-variable-value!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[9]+1)))(5,*((C_word*)lf[9]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* k458 in eval-definition in k409 in k407 */
static void C_ccall f_459(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[8]);}

/* look-up-variable-value in k409 in k407 */
static void C_ccall f_1060(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[7],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1060,4,t0,t1,t2,t3);}
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1064,a[2]=t2,a[3]=t5,a[4]=((C_word)li66),tmp=(C_word)a,a+=5,tmp));
C_trace("4.1:257: env-loop");
t7=((C_word*)t5)[1];
f_1064(t7,t1,t3);}

/* env-loop in look-up-variable-value in k409 in k407 */
static void C_fcall f_1064(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1064,NULL,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1068,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t4,a[5]=((C_word*)t0)[3],a[6]=((C_word)li65),tmp=(C_word)a,a+=7,tmp));
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_fcall f_1068(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
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
C_word *a;
loop:
a=C_alloc(10);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_1068,NULL,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1072,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
if(C_truep(C_i_nullp(t2))){
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1101,a[2]=((C_word*)t0)[5],a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:248: enclosing-environment");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[76]+1)))(3,*((C_word*)lf[76]+1),t5,((C_word*)t0)[2]);}
else{
t5=C_i_car(((C_word*)t0)[3]);
t6=C_eqp(((C_word*)t0)[3],t5);
if(C_truep(t6)){
t7=t4;
f_1072(2,t7,C_i_car(t3));}
else{
t7=C_i_cdr(t2);
t8=C_i_cdr(t3);
C_trace("4.1:251: scan");
t11=t4;
t12=t7;
t13=t8;
t1=t11;
t2=t12;
t3=t13;
goto loop;}}}

/* procedure-body in k409 in k407 */
static void C_ccall f_954(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_954,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* k1100 in scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_ccall f_1101(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:248: env-loop");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1064(t2,((C_word*)t0)[3],t1);}

/* k1559 in k1556 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1560(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:367: make-procedure");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[70]+1)))(5,*((C_word*)lf[70]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1566(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1566,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1572,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:371: begin-actions");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[47]+1)))(3,*((C_word*)lf[47]+1),t2,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1578,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:372: cond?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[57]+1)))(3,*((C_word*)lf[57]+1),t2,((C_word*)t0)[4]);}}

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
if(!C_demand_2(1160)){
C_save(t1);
C_rereclaim2(1160*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,125);
lf[0]=C_h_intern(&lf[0],4,"true");
lf[1]=C_h_intern(&lf[1],5,"false");
lf[2]=C_h_intern(&lf[2],13,"eval-sequence");
lf[3]=C_h_intern(&lf[3],4,"eval");
lf[4]=C_h_intern(&lf[4],9,"first-exp");
lf[5]=C_h_intern(&lf[5],9,"rest-exps");
lf[6]=C_h_intern(&lf[6],9,"last-exp\077");
lf[7]=C_h_intern(&lf[7],15,"eval-assignment");
lf[8]=C_h_intern(&lf[8],2,"ok");
lf[9]=C_h_intern(&lf[9],19,"set-variable-value!");
lf[10]=C_h_intern(&lf[10],16,"assignment-value");
lf[11]=C_h_intern(&lf[11],19,"assignment-variable");
lf[12]=C_h_intern(&lf[12],15,"eval-definition");
lf[13]=C_h_intern(&lf[13],16,"define-variable!");
lf[14]=C_h_intern(&lf[14],16,"definition-value");
lf[15]=C_h_intern(&lf[15],19,"definition-variable");
lf[16]=C_h_intern(&lf[16],16,"self-evaluating\077");
lf[17]=C_h_intern(&lf[17],9,"variable\077");
lf[18]=C_h_intern(&lf[18],7,"quoted\077");
lf[19]=C_h_intern(&lf[19],12,"tagged-list\077");
lf[20]=C_h_intern(&lf[20],5,"quote");
lf[21]=C_h_intern(&lf[21],4,"cadr");
lf[22]=C_h_intern(&lf[22],5,"caddr");
lf[23]=C_h_intern(&lf[23],5,"caadr");
lf[24]=C_h_intern(&lf[24],5,"cdadr");
lf[25]=C_h_intern(&lf[25],4,"cddr");
lf[26]=C_h_intern(&lf[26],5,"cdddr");
lf[27]=C_h_intern(&lf[27],6,"cadddr");
lf[28]=C_h_intern(&lf[28],17,"text-of-quotation");
lf[29]=C_h_intern(&lf[29],11,"assignment\077");
lf[30]=C_h_intern(&lf[30],4,"set!");
lf[31]=C_h_intern(&lf[31],11,"definition\077");
lf[32]=C_h_intern(&lf[32],6,"define");
lf[33]=C_h_intern(&lf[33],11,"make-lambda");
lf[34]=C_h_intern(&lf[34],7,"lambda\077");
lf[35]=C_h_intern(&lf[35],11,"tagged-list");
lf[36]=C_h_intern(&lf[36],6,"lambda");
lf[37]=C_h_intern(&lf[37],17,"lambda-parameters");
lf[38]=C_h_intern(&lf[38],11,"lambda-body");
lf[39]=C_h_intern(&lf[39],3,"if\077");
lf[40]=C_h_intern(&lf[40],2,"if");
lf[41]=C_h_intern(&lf[41],12,"if-predicate");
lf[42]=C_h_intern(&lf[42],13,"if-consequent");
lf[43]=C_h_intern(&lf[43],14,"if-alternative");
lf[44]=C_h_intern(&lf[44],7,"make-if");
lf[45]=C_h_intern(&lf[45],6,"begin\077");
lf[46]=C_h_intern(&lf[46],5,"begin");
lf[47]=C_h_intern(&lf[47],13,"begin-actions");
lf[48]=C_h_intern(&lf[48],13,"sequence->exp");
lf[49]=C_h_intern(&lf[49],9,"first-seq");
lf[50]=C_h_intern(&lf[50],10,"make-begin");
lf[51]=C_h_intern(&lf[51],12,"application\077");
lf[52]=C_h_intern(&lf[52],8,"operator");
lf[53]=C_h_intern(&lf[53],8,"operands");
lf[54]=C_h_intern(&lf[54],12,"no-operands\077");
lf[55]=C_h_intern(&lf[55],13,"first-operand");
lf[56]=C_h_intern(&lf[56],13,"rest-operands");
lf[57]=C_h_intern(&lf[57],5,"cond\077");
lf[58]=C_h_intern(&lf[58],4,"cond");
lf[59]=C_h_intern(&lf[59],12,"cond-clauses");
lf[60]=C_h_intern(&lf[60],17,"cond-else-clauses");
lf[61]=C_h_intern(&lf[61],4,"else");
lf[62]=C_h_intern(&lf[62],14,"cond-predicate");
lf[63]=C_h_intern(&lf[63],12,"cond-actions");
lf[64]=C_h_intern(&lf[64],8,"cond->if");
lf[65]=C_h_intern(&lf[65],14,"expand-clauses");
lf[66]=C_h_intern(&lf[66],5,"error");
lf[67]=C_decode_literal(C_heaptop,"\376B\000\000\042ELSE clause isn\047t last -- COND->IF");
lf[68]=C_h_intern(&lf[68],5,"true\077");
lf[69]=C_h_intern(&lf[69],6,"false\077");
lf[70]=C_h_intern(&lf[70],14,"make-procedure");
lf[71]=C_h_intern(&lf[71],9,"procedure");
lf[72]=C_h_intern(&lf[72],19,"compound-procedure\077");
lf[73]=C_h_intern(&lf[73],20,"procedure-parameters");
lf[74]=C_h_intern(&lf[74],14,"procedure-body");
lf[75]=C_h_intern(&lf[75],21,"procedure-environment");
lf[76]=C_h_intern(&lf[76],21,"enclosing-environment");
lf[77]=C_h_intern(&lf[77],11,"first-frame");
lf[78]=C_h_intern(&lf[78],13,"the-empty-env");
lf[79]=C_h_intern(&lf[79],10,"make-frame");
lf[80]=C_h_intern(&lf[80],15,"frame-variables");
lf[81]=C_h_intern(&lf[81],12,"frame-values");
lf[82]=C_h_intern(&lf[82],21,"add-binding-to-frame!");
lf[83]=C_h_intern(&lf[83],18,"extend-environment");
lf[84]=C_h_intern(&lf[84],9,"make-fram");
lf[85]=C_decode_literal(C_heaptop,"\376B\000\000\033Too many arguments supplied");
lf[86]=C_decode_literal(C_heaptop,"\376B\000\000\032Too few arguments supplied");
lf[87]=C_h_intern(&lf[87],22,"look-up-variable-value");
lf[88]=C_h_intern(&lf[88],21,"the-empty-environment");
lf[89]=C_decode_literal(C_heaptop,"\376B\000\000\020Unbound variable");
lf[90]=C_decode_literal(C_heaptop,"\376B\000\000\030Unbound variable -- SET!");
lf[91]=C_h_intern(&lf[91],17,"setup-environment");
lf[92]=C_h_intern(&lf[92],27,"primitive-procedure-objects");
lf[93]=C_h_intern(&lf[93],25,"primitive-procedure-names");
lf[94]=C_h_intern(&lf[94],14,"the-global-env");
lf[95]=C_h_intern(&lf[95],20,"primitive-procedure\077");
lf[96]=C_h_intern(&lf[96],9,"primitive");
lf[97]=C_h_intern(&lf[97],24,"primitive-implementation");
lf[98]=C_h_intern(&lf[98],3,"car");
lf[99]=C_h_intern(&lf[99],3,"cdr");
lf[100]=C_h_intern(&lf[100],4,"cons");
lf[101]=C_h_intern(&lf[101],5,"null\077");
lf[102]=C_h_intern(&lf[102],20,"primitive-procedures");
lf[103]=C_h_intern(&lf[103],3,"map");
lf[104]=C_h_intern(&lf[104],26,"apply-in-underlying-scheme");
lf[105]=C_h_intern(&lf[105],5,"apply");
lf[106]=C_h_intern(&lf[106],25,"apply-primitive-procedure");
lf[107]=C_h_intern(&lf[107],12,"input-prompt");
lf[108]=C_decode_literal(C_heaptop,"\376B\000\000\021;;; M-Eval input:");
lf[109]=C_h_intern(&lf[109],13,"output-prompt");
lf[110]=C_decode_literal(C_heaptop,"\376B\000\000\021;;; M-Eval value:");
lf[111]=C_h_intern(&lf[111],11,"driver-loop");
lf[112]=C_h_intern(&lf[112],10,"user-print");
lf[113]=C_h_intern(&lf[113],15,"announce-output");
lf[114]=C_h_intern(&lf[114],22,"the-global-environment");
lf[115]=C_h_intern(&lf[115],4,"read");
lf[116]=C_h_intern(&lf[116],16,"prompt-for-input");
lf[117]=C_h_intern(&lf[117],7,"newline");
lf[118]=C_h_intern(&lf[118],7,"display");
lf[119]=C_h_intern(&lf[119],18,"compound-procedure");
lf[120]=C_h_intern(&lf[120],15,"<procedure-env>");
lf[121]=C_h_intern(&lf[121],21,"lookup-variable-value");
lf[122]=C_decode_literal(C_heaptop,"\376B\000\000\037Unknown expression type -- EVAL");
lf[123]=C_decode_literal(C_heaptop,"\376B\000\000\037Unknown procedure type -- APPLY");
lf[124]=C_h_intern(&lf[124],25,"\003sysimplicit-exit-handler");
C_register_lf2(lf,125,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_408,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1551(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1551,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1557,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:367: lambda-parameters");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[37]+1)))(3,*((C_word*)lf[37]+1),t2,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1566,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:370: begin?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[45]+1)))(3,*((C_word*)lf[45]+1),t2,((C_word*)t0)[4]);}}

/* k1556 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1557(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1557,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1560,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:368: lambda-body");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[38]+1)))(3,*((C_word*)lf[38]+1),t2,((C_word*)t0)[4]);}

/* make-procedure in k409 in k407 */
static void C_ccall f_936(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word ab[12],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_936,5,t0,t1,t2,t3,t4);}
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_list4(&a,4,lf[71],t2,t3,t4));}

/* cddr in k409 in k407 */
static void C_ccall f_542(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_542,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_cdr(t3));}

/* false? in k409 in k407 */
static void C_ccall f_930(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_930,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_eqp(t2,*((C_word*)lf[1]+1)));}

/* set-variable-value! in k409 in k407 */
static void C_ccall f_1127(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[8],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1127,5,t0,t1,t2,t3,t4);}
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1131,a[2]=t6,a[3]=t2,a[4]=t3,a[5]=((C_word)li69),tmp=(C_word)a,a+=6,tmp));
C_trace("4.1:272: env-loop");
t8=((C_word*)t6)[1];
f_1131(t8,t1,t4);}

/* k1583 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1584(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:372: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1071 in scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_ccall f_1072(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1072,2,t0,t1);}
t2=C_eqp(((C_word*)t0)[2],C_fast_retrieve(lf[88]));
if(C_truep(t2)){
C_trace("4.1:253: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(4,*((C_word*)lf[66]+1),((C_word*)t0)[3],lf[89],((C_word*)t0)[4]);}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1083,a[2]=((C_word*)t0)[5],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:254: first-frame");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[77]+1)))(3,*((C_word*)lf[77]+1),t3,((C_word*)t0)[2]);}}

/* map-loop215 in primitive-procedure-names in k1258 in k409 in k407 */
static void C_fcall f_1287(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1287,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_slot(t2,C_fix(0));
t4=C_i_car(t3);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1298,a[2]=((C_word*)t0)[2],a[3]=t5,a[4]=t2,a[5]=((C_word*)t0)[3],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t7=t6;
f_1298(t7,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t5));}
else{
t7=C_mutate(((C_word *)((C_word*)t0)[4])+1,t5);
t8=t6;
f_1298(t8,t7);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* enclosing-environment in k409 in k407 */
static void C_ccall f_966(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_966,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* cdadr in k409 in k407 */
static void C_ccall f_530(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_530,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_car(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cdr(t4));}

/* procedure-environment in k409 in k407 */
static void C_ccall f_960(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_960,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadddr(t2));}

/* k1571 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1572(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:371: eval-sequence");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(4,*((C_word*)lf[2]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1578(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1578,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1584,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:372: cond->if");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[64]+1)))(3,*((C_word*)lf[64]+1),t2,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1590,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[5],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:373: application?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[51]+1)))(3,*((C_word*)lf[51]+1),t2,((C_word*)t0)[4]);}}

/* extend-environment in k409 in k407 */
static void C_ccall f_1021(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word ab[4],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1021,5,t0,t1,t2,t3,t4);}
t5=C_i_length(t2);
t6=C_i_length(t3);
if(C_truep(C_i_nequalp(t5,t6))){
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1034,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:235: make-fram");
((C_proc4)C_fast_retrieve_symbol_proc(lf[84]))(4,*((C_word*)lf[84]+1),t7,t2,t3);}
else{
t7=C_i_length(t2);
t8=C_i_length(t3);
if(C_truep(C_i_lessp(t7,t8))){
C_trace("4.1:237: error");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(5,*((C_word*)lf[66]+1),t1,lf[85],t2,t3);}
else{
C_trace("4.1:238: error");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(5,*((C_word*)lf[66]+1),t1,lf[86],t2,t3);}}}

/* primitive-procedure-names in k1258 in k409 in k407 */
static void C_ccall f_1278(C_word c,C_word t0,C_word t1){
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
C_word ab[12],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1278,2,t0,t1);}
t2=C_SCHEME_END_OF_LIST;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_SCHEME_FALSE;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_fast_retrieve(lf[102]);
t7=C_i_check_list_2(C_fast_retrieve(lf[102]),lf[103]);
t8=C_SCHEME_UNDEFINED;
t9=(*a=C_VECTOR_TYPE|1,a[1]=t8,tmp=(C_word)a,a+=2,tmp);
t10=C_set_block_item(t9,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1287,a[2]=t5,a[3]=t9,a[4]=t3,a[5]=((C_word)li76),tmp=(C_word)a,a+=6,tmp));
t11=((C_word*)t9)[1];
f_1287(t11,t1,C_fast_retrieve(lf[102]));}

/* frame-values in k409 in k407 */
static void C_ccall f_994(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_994,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* k1147 in scan in env-loop in set-variable-value! in k409 in k407 */
static void C_ccall f_1148(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:263: env-loop");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1131(t2,((C_word*)t0)[3],t1);}

/* primitive-procedure? in k1258 in k409 in k407 */
static void C_ccall f_1262(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1262,3,t0,t1,t2);}
C_trace("4.1:299: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[96]);}

/* k1258 in k409 in k407 */
static void C_ccall f_1260(C_word c,C_word t0,C_word t1){
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
C_word ab[72],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1260,2,t0,t1);}
t2=C_mutate((C_word*)lf[94]+1 /* (set! the-global-env ...) */,t1);
t3=C_mutate((C_word*)lf[95]+1 /* (set! primitive-procedure? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1262,a[2]=((C_word)li74),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[97]+1 /* (set! primitive-implementation ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1268,a[2]=((C_word)li75),tmp=(C_word)a,a+=3,tmp));
t5=C_a_i_list2(&a,2,lf[98],*((C_word*)lf[98]+1));
t6=C_a_i_list2(&a,2,lf[99],*((C_word*)lf[99]+1));
t7=C_a_i_list2(&a,2,lf[100],*((C_word*)lf[100]+1));
t8=C_a_i_list2(&a,2,lf[101],*((C_word*)lf[101]+1));
t9=C_a_i_list4(&a,4,t5,t6,t7,t8);
t10=C_mutate((C_word*)lf[102]+1 /* (set! primitive-procedures ...) */,t9);
t11=C_mutate((C_word*)lf[93]+1 /* (set! primitive-procedure-names ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1278,a[2]=((C_word)li77),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[92]+1 /* (set! primitive-procedure-objects ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1317,a[2]=((C_word)li80),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[104]+1 /* (set! apply-in-underlying-scheme ...) */,*((C_word*)lf[105]+1));
t14=C_mutate((C_word*)lf[106]+1 /* (set! apply-primitive-procedure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1364,a[2]=((C_word)li81),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[107]+1 /* (set! input-prompt ...) */,lf[108]);
t16=C_mutate((C_word*)lf[109]+1 /* (set! output-prompt ...) */,lf[110]);
t17=C_mutate((C_word*)lf[111]+1 /* (set! driver-loop ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1375,a[2]=((C_word)li82),tmp=(C_word)a,a+=3,tmp));
t18=C_mutate((C_word*)lf[116]+1 /* (set! prompt-for-input ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1393,a[2]=((C_word)li83),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate((C_word*)lf[113]+1 /* (set! announce-output ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1406,a[2]=((C_word)li84),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[112]+1 /* (set! user-print ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1417,a[2]=((C_word)li85),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[3]+1 /* (set! eval ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1441,a[2]=((C_word)li87),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[105]+1 /* (set! apply ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1610,a[2]=((C_word)li88),tmp=(C_word)a,a+=3,tmp));
t23=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1648,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:397: setup-environment");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(2,*((C_word*)lf[91]+1),t23);}

/* primitive-implementation in k1258 in k409 in k407 */
static void C_ccall f_1268(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1268,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* procedure-parameters in k409 in k407 */
static void C_ccall f_948(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_948,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* assignment? in k409 in k407 */
static void C_ccall f_599(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_599,3,t0,t1,t2);}
C_trace("4.1:71: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[30]);}

/* compound-procedure? in k409 in k407 */
static void C_ccall f_942(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_942,3,t0,t1,t2);}
C_trace("4.1:193: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[71]);}

/* scan in env-loop in set-variable-value! in k409 in k407 */
static void C_fcall f_1135(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word *a;
loop:
a=C_alloc(4);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_1135,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1148,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:263: enclosing-environment");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[76]+1)))(3,*((C_word*)lf[76]+1),t4,((C_word*)t0)[3]);}
else{
t4=C_i_car(t2);
t5=C_eqp(((C_word*)t0)[4],t4);
if(C_truep(t5)){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_i_set_car(t3,((C_word*)t0)[5]));}
else{
t6=C_i_cdr(t2);
t7=C_i_cdr(t3);
C_trace("4.1:266: scan");
t10=t1;
t11=t6;
t12=t7;
t1=t10;
t2=t11;
t3=t12;
goto loop;}}}

/* env-loop in set-variable-value! in k409 in k407 */
static void C_fcall f_1131(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1131,NULL,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1135,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=t4,a[7]=((C_word)li68),tmp=(C_word)a,a+=8,tmp));
t6=C_eqp(t2,C_fast_retrieve(lf[88]));
if(C_truep(t6)){
C_trace("4.1:268: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(4,*((C_word*)lf[66]+1),t1,lf[90],((C_word*)t0)[3]);}
else{
t7=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1180,a[2]=t4,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:269: first-frame");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[77]+1)))(3,*((C_word*)lf[77]+1),t7,t2);}}

/* k1595 in k1588 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1596(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1596,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1599,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1602,a[2]=((C_word*)t0)[3],a[3]=t2,a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:375: operands");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[53]+1)))(3,*((C_word*)lf[53]+1),t3,((C_word*)t0)[5]);}

/* k1598 in k1595 in k1588 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1599(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_apply(4,0,((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k1252 in setup-environment in k409 in k407 */
static void C_ccall f_1253(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1253,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1256,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:292: primitive-procedure-objects");
((C_proc2)C_fast_retrieve_symbol_proc(lf[92]))(2,*((C_word*)lf[92]+1),t2);}

/* k1588 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1590(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1590,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1596,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1605,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:374: operator");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[52]+1)))(3,*((C_word*)lf[52]+1),t3,((C_word*)t0)[5]);}
else{
C_trace("4.1:377: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(4,*((C_word*)lf[66]+1),((C_word*)t0)[2],lf[122],((C_word*)t0)[5]);}}

/* k1249 in k1247 in k1245 in setup-environment in k409 in k407 */
static void C_ccall f_1250(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[3]);}

/* k1255 in k1252 in setup-environment in k409 in k407 */
static void C_ccall f_1256(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:291: extend-environment");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[83]+1)))(5,*((C_word*)lf[83]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,C_fast_retrieve(lf[88]));}

/* tagged-list? in k409 in k407 */
static void C_ccall f_584(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_584,4,t0,t1,t2,t3);}
if(C_truep(C_i_pairp(t2))){
t4=C_i_car(t2);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_eqp(t4,t3));}
else{
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,*((C_word*)lf[1]+1));}}

/* the-empty-env in k409 in k407 */
static void C_ccall f_978(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_978,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}

/* first-frame in k409 in k407 */
static void C_ccall f_972(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_972,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* k1245 in setup-environment in k409 in k407 */
static void C_ccall f_1246(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1246,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1248,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:294: define-variable!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(5,*((C_word*)lf[13]+1),t2,lf[0],*((C_word*)lf[0]+1),t1);}

/* k1239 in k1236 in k1197 in define-variable! in k409 in k407 */
static void C_ccall f_1240(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:283: scan");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1200(t2,((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* setup-environment in k409 in k407 */
static void C_ccall f_1242(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1242,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1246,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1253,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:291: primitive-procedure-names");
((C_proc2)C_fast_retrieve_symbol_proc(lf[93]))(2,*((C_word*)lf[93]+1),t3);}

/* k1247 in k1245 in setup-environment in k409 in k407 */
static void C_ccall f_1248(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1248,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1250,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:295: define-variable!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(5,*((C_word*)lf[13]+1),t2,lf[1],*((C_word*)lf[1]+1),((C_word*)t0)[3]);}

/* text-of-quotation in k409 in k407 */
static void C_ccall f_578(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_578,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1236 in k1197 in define-variable! in k409 in k407 */
static void C_ccall f_1237(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1237,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1240,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:284: frame-values");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[81]+1)))(3,*((C_word*)lf[81]+1),t2,((C_word*)t0)[4]);}

/* make-lambda in k409 in k407 */
static void C_ccall f_683(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_683,4,t0,t1,t2,t3);}
t4=C_a_i_cons(&a,2,t2,t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_cons(&a,2,lf[36],t4));}

/* frame-variables in k409 in k407 */
static void C_ccall f_988(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_988,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* make-frame in k409 in k407 */
static void C_ccall f_982(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word ab[3],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_982,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,t2,t3));}

/* k1398 in k1396 in prompt-for-input in k1258 in k409 in k407 */
static void C_ccall f_1399(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1399,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1401,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:332: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[118]+1)))(3,*((C_word*)lf[118]+1),t2,((C_word*)t0)[3]);}

/* k1396 in prompt-for-input in k1258 in k409 in k407 */
static void C_ccall f_1397(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1397,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1399,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:332: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[117]+1)))(2,*((C_word*)lf[117]+1),t2);}

/* prompt-for-input in k1258 in k409 in k407 */
static void C_ccall f_1393(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1393,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1397,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:332: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[117]+1)))(2,*((C_word*)lf[117]+1),t3);}

/* lambda-parameters in k409 in k407 */
static void C_ccall f_671(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_671,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* lambda-body in k409 in k407 */
static void C_ccall f_677(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_677,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cddr(t2));}

/* lambda? in k409 in k407 */
static void C_ccall f_665(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_665,3,t0,t1,t2);}
C_trace("4.1:98: tagged-list");
((C_proc4)C_fast_retrieve_symbol_proc(lf[35]))(4,*((C_word*)lf[35]+1),t1,t2,lf[36]);}

/* k1091 in k1088 in k1082 in k1071 in scan in env-loop in look-up-variable-value in k409 in k407 */
static void C_ccall f_1092(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:255: scan");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1068(t2,((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* caadr in k409 in k407 */
static void C_ccall f_518(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_518,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_car(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_car(t4));}

/* k656 in definition-value in k409 in k407 */
static void C_ccall f_657(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_i_cddr(((C_word*)t0)[2]);
C_trace("4.1:92: make-lambda");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[33]+1)))(4,*((C_word*)lf[33]+1),((C_word*)t0)[3],t1,t2);}

/* k909 in k887 in expand-clauses in k409 in k407 */
static void C_ccall f_910(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_910,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_913,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_919,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:172: cond-actions");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[63]+1)))(3,*((C_word*)lf[63]+1),t3,((C_word*)t0)[4]);}

/* k918 in k909 in k887 in expand-clauses in k409 in k407 */
static void C_ccall f_919(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:172: sequence->exp");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[48]+1)))(3,*((C_word*)lf[48]+1),((C_word*)t0)[2],t1);}

/* k915 in k912 in k909 in k887 in expand-clauses in k409 in k407 */
static void C_ccall f_916(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:171: make-if");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[44]+1)))(5,*((C_word*)lf[44]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* k912 in k909 in k887 in expand-clauses in k409 in k407 */
static void C_ccall f_913(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_913,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_916,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:173: expand-clauses");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[65]+1)))(3,*((C_word*)lf[65]+1),t2,((C_word*)t0)[4]);}

/* caddr in k409 in k407 */
static void C_ccall f_506(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_506,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_car(t4));}

/* no-operands? in k409 in k407 */
static void C_ccall f_812(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_812,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_nullp(t2));}

/* first-operand in k409 in k407 */
static void C_ccall f_818(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_818,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* k1357 in map-loop242 in primitive-procedure-objects in k1258 in k409 in k407 */
static void C_ccall f_1358(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1358,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,C_SCHEME_END_OF_LIST);
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1344,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t4=t3;
f_1344(t4,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t2));}
else{
t4=C_mutate(((C_word *)((C_word*)t0)[6])+1,t2);
t5=t3;
f_1344(t5,t4);}}

/* if? in k409 in k407 */
static void C_ccall f_692(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_692,3,t0,t1,t2);}
C_trace("4.1:108: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[40]);}

/* if-predicate in k409 in k407 */
static void C_ccall f_698(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_698,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k1380 in k1378 in driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1381(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1381,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1383,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:327: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),t2,t1,C_fast_retrieve(lf[114]));}

/* k1382 in k1380 in k1378 in driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1383(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1383,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1385,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:328: announce-output");
((C_proc3)C_fast_retrieve_symbol_proc(lf[113]))(3,*((C_word*)lf[113]+1),t2,C_fast_retrieve(lf[109]));}

/* k1384 in k1382 in k1380 in k1378 in driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1385(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1385,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1388,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:329: user-print");
((C_proc3)C_fast_retrieve_symbol_proc(lf[112]))(3,*((C_word*)lf[112]+1),t2,((C_word*)t0)[3]);}

/* k1386 in k1384 in k1382 in k1380 in k1378 in driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1388(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:330: driver-loop");
((C_proc2)C_fast_retrieve_symbol_proc(lf[111]))(2,*((C_word*)lf[111]+1),((C_word*)t0)[2]);}

/* cadddr in k409 in k407 */
static void C_ccall f_563(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_563,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=C_i_cdr(t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_i_car(t5));}

/* k1370 in apply-primitive-procedure in k1258 in k409 in k407 */
static void C_ccall f_1371(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:318: apply-in-underlying-scheme");
((C_proc4)C_fast_retrieve_symbol_proc(lf[104]))(4,*((C_word*)lf[104]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1375(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1375,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1379,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:325: prompt-for-input");
((C_proc3)C_fast_retrieve_symbol_proc(lf[116]))(3,*((C_word*)lf[116]+1),t2,C_fast_retrieve(lf[107]));}

/* k1378 in driver-loop in k1258 in k409 in k407 */
static void C_ccall f_1379(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1379,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1381,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:326: read");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[115]+1)))(2,*((C_word*)lf[115]+1),t2);}

/* cdddr in k409 in k407 */
static void C_ccall f_551(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_551,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cdr(t4));}

/* k1450 in list-of-values in eval in k1258 in k409 in k407 */
static void C_ccall f_1452(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1452,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_END_OF_LIST);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1458,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1467,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:350: first-operand");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[55]+1)))(3,*((C_word*)lf[55]+1),t3,((C_word*)t0)[5]);}}

/* k1457 in k1450 in list-of-values in eval in k1258 in k409 in k407 */
static void C_ccall f_1458(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1458,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1461,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1464,a[2]=((C_word*)t0)[3],a[3]=t2,a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:351: rest-operands");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[56]+1)))(3,*((C_word*)lf[56]+1),t3,((C_word*)t0)[5]);}

/* eval in k1258 in k409 in k407 */
static void C_ccall f_1441(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[12],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1441,4,t0,t1,t2,t3);}
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1445,a[2]=t5,a[3]=((C_word)li86),tmp=(C_word)a,a+=4,tmp));
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1500,a[2]=t1,a[3]=t2,a[4]=t3,a[5]=t5,tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:360: self-evaluating?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[16]+1)))(3,*((C_word*)lf[16]+1),t7,t2);}

/* list-of-values in eval in k1258 in k409 in k407 */
static void C_fcall f_1445(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1445,NULL,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1452,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t3,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:348: no-operands?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[54]+1)))(3,*((C_word*)lf[54]+1),t4,t2);}

/* primitive-procedure-objects in k1258 in k409 in k407 */
static void C_ccall f_1317(C_word c,C_word t0,C_word t1){
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
C_word ab[16],*a=ab;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1317,2,t0,t1);}
t2=C_SCHEME_END_OF_LIST;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_SCHEME_FALSE;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1320,a[2]=((C_word)li78),tmp=(C_word)a,a+=3,tmp);
t7=C_fast_retrieve(lf[102]);
t8=C_i_check_list_2(C_fast_retrieve(lf[102]),lf[103]);
t9=C_SCHEME_UNDEFINED;
t10=(*a=C_VECTOR_TYPE|1,a[1]=t9,tmp=(C_word)a,a+=2,tmp);
t11=C_set_block_item(t10,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1333,a[2]=t5,a[3]=t10,a[4]=t3,a[5]=t6,a[6]=((C_word)li79),tmp=(C_word)a,a+=7,tmp));
t12=((C_word*)t10)[1];
f_1333(t12,t1,C_fast_retrieve(lf[102]));}

/* k1432 in k1422 in user-print in k1258 in k409 in k407 */
static void C_ccall f_1433(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1433,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1436,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:339: procedure-body");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[74]+1)))(3,*((C_word*)lf[74]+1),t2,((C_word*)t0)[3]);}

/* k1435 in k1432 in k1422 in user-print in k1258 in k409 in k407 */
static void C_ccall f_1436(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1436,2,t0,t1);}
t2=C_a_i_list4(&a,4,lf[119],((C_word*)t0)[2],t1,lf[120]);
C_trace("4.1:337: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[118]+1)))(3,*((C_word*)lf[118]+1),((C_word*)t0)[3],t2);}

/* k1343 in k1357 in map-loop242 in primitive-procedure-objects in k1258 in k409 in k407 */
static void C_fcall f_1344(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_1333(t4,((C_word*)t0)[6],t3);}

/* definition-value in k409 in k407 */
static void C_ccall f_641(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_641,3,t0,t1,t2);}
t3=C_i_cadr(t2);
if(C_truep(C_i_symbolp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_caddr(t2));}
else{
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_657,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:93: cdadr");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[24]+1)))(3,*((C_word*)lf[24]+1),t4,t2);}}

/* k1297 in map-loop215 in primitive-procedure-names in k1258 in k409 in k407 */
static void C_fcall f_1298(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_1287(t4,((C_word*)t0)[6],t3);}

/* k1422 in user-print in k1258 in k409 in k407 */
static void C_ccall f_1424(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1424,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1433,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:338: procedure-parameters");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[73]+1)))(3,*((C_word*)lf[73]+1),t2,((C_word*)t0)[3]);}
else{
C_trace("4.1:341: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[118]+1)))(3,*((C_word*)lf[118]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}}

/* map-loop242 in primitive-procedure-objects in k1258 in k409 in k407 */
static void C_fcall f_1333(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1333,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1358,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word*)t0)[3],a[5]=t1,a[6]=((C_word*)t0)[4],tmp=(C_word)a,a+=7,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("4.1:311: g248");
t5=((C_word*)t0)[5];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* k1197 in define-variable! in k409 in k407 */
static void C_ccall f_1198(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[14],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1198,2,t0,t1);}
t2=C_SCHEME_UNDEFINED;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_set_block_item(t3,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1200,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=t3,a[6]=((C_word)li71),tmp=(C_word)a,a+=7,tmp));
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1237,a[2]=t3,a[3]=((C_word*)t0)[4],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:283: frame-variables");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[80]+1)))(3,*((C_word*)lf[80]+1),t5,t1);}

/* define-variable! in k409 in k407 */
static void C_ccall f_1194(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1194,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1198,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:276: first-frame");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[77]+1)))(3,*((C_word*)lf[77]+1),t5,t4);}

/* k1409 in announce-output in k1258 in k409 in k407 */
static void C_ccall f_1410(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1410,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1412,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:334: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[118]+1)))(3,*((C_word*)lf[118]+1),t2,((C_word*)t0)[3]);}

/* k1411 in k1409 in announce-output in k1258 in k409 in k407 */
static void C_ccall f_1412(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:334: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[117]+1)))(2,*((C_word*)lf[117]+1),((C_word*)t0)[2]);}

/* user-print in k1258 in k409 in k407 */
static void C_ccall f_1417(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1417,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1424,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:336: compound-procedure?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[72]+1)))(3,*((C_word*)lf[72]+1),t3,t2);}

/* apply-primitive-procedure in k1258 in k409 in k407 */
static void C_ccall f_1364(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1364,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1371,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:319: primitive-implementation");
((C_proc3)C_fast_retrieve_symbol_proc(lf[97]))(3,*((C_word*)lf[97]+1),t4,t2);}

/* k848 in cond-else-clauses in k409 in k407 */
static void C_ccall f_849(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_eqp(t1,lf[61]));}

/* k1400 in k1398 in k1396 in prompt-for-input in k1258 in k409 in k407 */
static void C_ccall f_1401(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:332: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[117]+1)))(2,*((C_word*)lf[117]+1),((C_word*)t0)[2]);}

/* cond-else-clauses in k409 in k407 */
static void C_ccall f_842(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_842,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_849,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:155: cond-predicate");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[62]+1)))(3,*((C_word*)lf[62]+1),t3,t2);}

/* announce-output in k1258 in k409 in k407 */
static void C_ccall f_1406(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1406,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1410,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:334: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[117]+1)))(2,*((C_word*)lf[117]+1),t3);}

/* k428 in k419 in eval-sequence in k409 in k407 */
static void C_ccall f_429(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_429,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_435,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:18: rest-exps");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[5]+1)))(3,*((C_word*)lf[5]+1),t2,((C_word*)t0)[4]);}

/* k426 in k419 in eval-sequence in k409 in k407 */
static void C_ccall f_427(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:15: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k419 in eval-sequence in k409 in k407 */
static void C_ccall f_421(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_421,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_427,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:15: first-exp");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[4]+1)))(3,*((C_word*)lf[4]+1),t2,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_429,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_438,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:17: first-exp");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[4]+1)))(3,*((C_word*)lf[4]+1),t3,((C_word*)t0)[4]);}}

/* cond-clauses in k409 in k407 */
static void C_ccall f_836(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_836,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* cond? in k409 in k407 */
static void C_ccall f_830(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_830,3,t0,t1,t2);}
C_trace("4.1:152: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[58]);}

/* add-binding-to-frame! in k409 in k407 */
static void C_ccall f_1000(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_1000,5,t0,t1,t2,t3,t4);}
t5=C_i_car(t4);
t6=C_a_i_cons(&a,2,t2,t5);
t7=C_i_set_car(t4,t6);
t8=C_i_cdr(t4);
t9=C_a_i_cons(&a,2,t3,t8);
t10=t1;
((C_proc2)(void*)(*((C_word*)t10+1)))(2,t10,C_i_set_cdr(t4,t9));}

/* k1601 in k1595 in k1588 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1602(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:375: list-of-values");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1445(t2,((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* k1604 in k1588 in k1576 in k1564 in k1549 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1605(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:374: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* eval-sequence in k409 in k407 */
static void C_ccall f_414(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_414,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_421,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:14: last-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(3,*((C_word*)lf[6]+1),t4,t2);}

/* k409 in k407 */
static void C_ccall f_410(C_word c,C_word t0,C_word t1){
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
C_word t57;
C_word t58;
C_word t59;
C_word t60;
C_word t61;
C_word t62;
C_word t63;
C_word t64;
C_word t65;
C_word t66;
C_word t67;
C_word t68;
C_word t69;
C_word t70;
C_word t71;
C_word t72;
C_word t73;
C_word t74;
C_word ab[210],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_410,2,t0,t1);}
t2=C_set_block_item(lf[0] /* true */,0,C_SCHEME_TRUE);
t3=C_set_block_item(lf[1] /* false */,0,C_SCHEME_FALSE);
t4=C_mutate((C_word*)lf[2]+1 /* (set! eval-sequence ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_414,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[7]+1 /* (set! eval-assignment ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_440,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[12]+1 /* (set! eval-definition ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_455,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[16]+1 /* (set! self-evaluating? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_470,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[17]+1 /* (set! variable? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_485,a[2]=((C_word)li4),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[18]+1 /* (set! quoted? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_491,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[21]+1 /* (set! cadr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_497,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[22]+1 /* (set! caddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_506,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[23]+1 /* (set! caadr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_518,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[24]+1 /* (set! cdadr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_530,a[2]=((C_word)li9),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[25]+1 /* (set! cddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_542,a[2]=((C_word)li10),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[26]+1 /* (set! cdddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_551,a[2]=((C_word)li11),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate((C_word*)lf[27]+1 /* (set! cadddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_563,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate((C_word*)lf[28]+1 /* (set! text-of-quotation ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_578,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t18=C_mutate((C_word*)lf[19]+1 /* (set! tagged-list? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_584,a[2]=((C_word)li14),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate((C_word*)lf[29]+1 /* (set! assignment? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_599,a[2]=((C_word)li15),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[11]+1 /* (set! assignment-variable ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_605,a[2]=((C_word)li16),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[10]+1 /* (set! assignment-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_611,a[2]=((C_word)li17),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[31]+1 /* (set! definition? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_617,a[2]=((C_word)li18),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate((C_word*)lf[15]+1 /* (set! definition-variable ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_623,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate((C_word*)lf[14]+1 /* (set! definition-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_641,a[2]=((C_word)li20),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate((C_word*)lf[34]+1 /* (set! lambda? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_665,a[2]=((C_word)li21),tmp=(C_word)a,a+=3,tmp));
t26=C_mutate((C_word*)lf[37]+1 /* (set! lambda-parameters ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_671,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t27=C_mutate((C_word*)lf[38]+1 /* (set! lambda-body ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_677,a[2]=((C_word)li23),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[33]+1 /* (set! make-lambda ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_683,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate((C_word*)lf[39]+1 /* (set! if? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_692,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[41]+1 /* (set! if-predicate ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_698,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[42]+1 /* (set! if-consequent ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_704,a[2]=((C_word)li27),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate((C_word*)lf[43]+1 /* (set! if-alternative ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_710,a[2]=((C_word)li28),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate((C_word*)lf[44]+1 /* (set! make-if ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_728,a[2]=((C_word)li29),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate((C_word*)lf[45]+1 /* (set! begin? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_734,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate((C_word*)lf[47]+1 /* (set! begin-actions ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_740,a[2]=((C_word)li31),tmp=(C_word)a,a+=3,tmp));
t36=C_mutate((C_word*)lf[6]+1 /* (set! last-exp? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_746,a[2]=((C_word)li32),tmp=(C_word)a,a+=3,tmp));
t37=C_mutate((C_word*)lf[4]+1 /* (set! first-exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_755,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t38=C_mutate((C_word*)lf[5]+1 /* (set! rest-exps ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_761,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t39=C_mutate((C_word*)lf[48]+1 /* (set! sequence->exp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_767,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t40=C_mutate((C_word*)lf[50]+1 /* (set! make-begin ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_788,a[2]=((C_word)li36),tmp=(C_word)a,a+=3,tmp));
t41=C_mutate((C_word*)lf[51]+1 /* (set! application? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_794,a[2]=((C_word)li37),tmp=(C_word)a,a+=3,tmp));
t42=C_mutate((C_word*)lf[52]+1 /* (set! operator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_800,a[2]=((C_word)li38),tmp=(C_word)a,a+=3,tmp));
t43=C_mutate((C_word*)lf[53]+1 /* (set! operands ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_806,a[2]=((C_word)li39),tmp=(C_word)a,a+=3,tmp));
t44=C_mutate((C_word*)lf[54]+1 /* (set! no-operands? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_812,a[2]=((C_word)li40),tmp=(C_word)a,a+=3,tmp));
t45=C_mutate((C_word*)lf[55]+1 /* (set! first-operand ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_818,a[2]=((C_word)li41),tmp=(C_word)a,a+=3,tmp));
t46=C_mutate((C_word*)lf[56]+1 /* (set! rest-operands ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_824,a[2]=((C_word)li42),tmp=(C_word)a,a+=3,tmp));
t47=C_mutate((C_word*)lf[57]+1 /* (set! cond? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_830,a[2]=((C_word)li43),tmp=(C_word)a,a+=3,tmp));
t48=C_mutate((C_word*)lf[59]+1 /* (set! cond-clauses ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_836,a[2]=((C_word)li44),tmp=(C_word)a,a+=3,tmp));
t49=C_mutate((C_word*)lf[60]+1 /* (set! cond-else-clauses ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_842,a[2]=((C_word)li45),tmp=(C_word)a,a+=3,tmp));
t50=C_mutate((C_word*)lf[62]+1 /* (set! cond-predicate ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_851,a[2]=((C_word)li46),tmp=(C_word)a,a+=3,tmp));
t51=C_mutate((C_word*)lf[63]+1 /* (set! cond-actions ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_857,a[2]=((C_word)li47),tmp=(C_word)a,a+=3,tmp));
t52=C_mutate((C_word*)lf[64]+1 /* (set! cond->if ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_863,a[2]=((C_word)li48),tmp=(C_word)a,a+=3,tmp));
t53=C_mutate((C_word*)lf[65]+1 /* (set! expand-clauses ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_872,a[2]=((C_word)li49),tmp=(C_word)a,a+=3,tmp));
t54=C_mutate((C_word*)lf[68]+1 /* (set! true? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_921,a[2]=((C_word)li50),tmp=(C_word)a,a+=3,tmp));
t55=C_mutate((C_word*)lf[69]+1 /* (set! false? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_930,a[2]=((C_word)li51),tmp=(C_word)a,a+=3,tmp));
t56=C_mutate((C_word*)lf[70]+1 /* (set! make-procedure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_936,a[2]=((C_word)li52),tmp=(C_word)a,a+=3,tmp));
t57=C_mutate((C_word*)lf[72]+1 /* (set! compound-procedure? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_942,a[2]=((C_word)li53),tmp=(C_word)a,a+=3,tmp));
t58=C_mutate((C_word*)lf[73]+1 /* (set! procedure-parameters ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_948,a[2]=((C_word)li54),tmp=(C_word)a,a+=3,tmp));
t59=C_mutate((C_word*)lf[74]+1 /* (set! procedure-body ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_954,a[2]=((C_word)li55),tmp=(C_word)a,a+=3,tmp));
t60=C_mutate((C_word*)lf[75]+1 /* (set! procedure-environment ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_960,a[2]=((C_word)li56),tmp=(C_word)a,a+=3,tmp));
t61=C_mutate((C_word*)lf[76]+1 /* (set! enclosing-environment ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_966,a[2]=((C_word)li57),tmp=(C_word)a,a+=3,tmp));
t62=C_mutate((C_word*)lf[77]+1 /* (set! first-frame ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_972,a[2]=((C_word)li58),tmp=(C_word)a,a+=3,tmp));
t63=C_mutate((C_word*)lf[78]+1 /* (set! the-empty-env ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_978,a[2]=((C_word)li59),tmp=(C_word)a,a+=3,tmp));
t64=C_mutate((C_word*)lf[79]+1 /* (set! make-frame ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_982,a[2]=((C_word)li60),tmp=(C_word)a,a+=3,tmp));
t65=C_mutate((C_word*)lf[80]+1 /* (set! frame-variables ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_988,a[2]=((C_word)li61),tmp=(C_word)a,a+=3,tmp));
t66=C_mutate((C_word*)lf[81]+1 /* (set! frame-values ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_994,a[2]=((C_word)li62),tmp=(C_word)a,a+=3,tmp));
t67=C_mutate((C_word*)lf[82]+1 /* (set! add-binding-to-frame! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1000,a[2]=((C_word)li63),tmp=(C_word)a,a+=3,tmp));
t68=C_mutate((C_word*)lf[83]+1 /* (set! extend-environment ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1021,a[2]=((C_word)li64),tmp=(C_word)a,a+=3,tmp));
t69=C_mutate((C_word*)lf[87]+1 /* (set! look-up-variable-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1060,a[2]=((C_word)li67),tmp=(C_word)a,a+=3,tmp));
t70=C_mutate((C_word*)lf[9]+1 /* (set! set-variable-value! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1127,a[2]=((C_word)li70),tmp=(C_word)a,a+=3,tmp));
t71=C_mutate((C_word*)lf[13]+1 /* (set! define-variable! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1194,a[2]=((C_word)li72),tmp=(C_word)a,a+=3,tmp));
t72=C_mutate((C_word*)lf[91]+1 /* (set! setup-environment ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1242,a[2]=((C_word)li73),tmp=(C_word)a,a+=3,tmp));
t73=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1260,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:297: setup-environment");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[91]+1)))(2,*((C_word*)lf[91]+1),t73);}

/* k1033 in extend-environment in k409 in k407 */
static void C_ccall f_1034(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1034,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,t1,((C_word*)t0)[3]));}

/* rest-operands in k409 in k407 */
static void C_ccall f_824(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_824,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* f_1320 in primitive-procedure-objects in k1258 in k409 in k407 */
static void C_ccall f_1320(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_1320,3,t0,t1,t2);}
t3=C_i_cadr(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_list2(&a,2,lf[96],t3));}

/* k407 */
static void C_ccall f_408(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_408,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_410,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k443 in eval-assignment in k409 in k407 */
static void C_ccall f_444(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[8]);}

/* eval-assignment in k409 in k407 */
static void C_ccall f_440(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[8],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_440,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_444,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_447,a[2]=t4,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:26: assignment-variable");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t5,t2);}

/* k446 in eval-assignment in k409 in k407 */
static void C_ccall f_447(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_447,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_450,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_453,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:27: assignment-value");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(3,*((C_word*)lf[10]+1),t3,((C_word*)t0)[4]);}

/* assignment-variable in k409 in k407 */
static void C_ccall f_605(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_605,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k887 in expand-clauses in k409 in k407 */
static void C_ccall f_889(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_889,2,t0,t1);}
if(C_truep(t1)){
if(C_truep(C_i_nullp(((C_word*)t0)[2]))){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_901,a[2]=((C_word*)t0)[3],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:169: cond-actions");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[63]+1)))(3,*((C_word*)lf[63]+1),t2,((C_word*)t0)[4]);}
else{
C_trace("4.1:170: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(4,*((C_word*)lf[66]+1),((C_word*)t0)[3],lf[67],((C_word*)t0)[5]);}}
else{
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_910,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:171: cond-predicate");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[62]+1)))(3,*((C_word*)lf[62]+1),t2,((C_word*)t0)[4]);}}

/* k434 in k428 in k419 in eval-sequence in k409 in k407 */
static void C_ccall f_435(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:18: eval-sequence");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(4,*((C_word*)lf[2]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1651 in k1649 in k1646 in k1258 in k409 in k407 */
static void C_ccall f_1652(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* k1649 in k1646 in k1258 in k409 in k407 */
static void C_ccall f_1650(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1650,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1652,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1655,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[124]))(2,*((C_word*)lf[124]+1),t3);}

/* k437 in k419 in eval-sequence in k409 in k407 */
static void C_ccall f_438(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:17: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1653 in k1649 in k1646 in k1258 in k409 in k407 */
static void C_ccall f_1655(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* expand-clauses in k409 in k407 */
static void C_ccall f_872(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_872,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,*((C_word*)lf[1]+1));}
else{
t3=C_i_car(t2);
t4=C_i_cdr(t2);
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_889,a[2]=t4,a[3]=t1,a[4]=t3,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:167: cond-else-clauses");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[60]+1)))(3,*((C_word*)lf[60]+1),t5,t3);}}

/* k869 in cond->if in k409 in k407 */
static void C_ccall f_870(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:161: expand-clauses");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[65]+1)))(3,*((C_word*)lf[65]+1),((C_word*)t0)[2],t1);}

/* rest-exps in k409 in k407 */
static void C_ccall f_761(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_761,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* sequence->exp in k409 in k407 */
static void C_ccall f_767(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_767,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_780,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:135: last-exp?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(3,*((C_word*)lf[6]+1),t3,t2);}}

/* quoted? in k409 in k407 */
static void C_ccall f_491(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_491,3,t0,t1,t2);}
C_trace("4.1:51: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[20]);}

/* cadr in k409 in k407 */
static void C_ccall f_497(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_497,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_car(t3));}

/* cond->if in k409 in k407 */
static void C_ccall f_863(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_863,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_870,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:161: cond-clauses");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[59]+1)))(3,*((C_word*)lf[59]+1),t3,t2);}

/* scan in k1197 in define-variable! in k409 in k407 */
static void C_fcall f_1200(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word *a;
loop:
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_1200,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
C_trace("4.1:279: add-binding-to-frame!");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[82]+1)))(5,*((C_word*)lf[82]+1),t1,((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t4=C_i_car(t2);
t5=C_eqp(((C_word*)t0)[2],t4);
if(C_truep(t5)){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_i_set_car(t3,((C_word*)t0)[3]));}
else{
t6=C_i_cdr(t2);
t7=C_i_cdr(t3);
C_trace("4.1:282: scan");
t9=t1;
t10=t6;
t11=t7;
t1=t9;
t2=t10;
t3=t11;
goto loop;}}}

/* k1631 in k1624 in k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1632(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1632,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1635,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1638,a[2]=t2,a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:387: procedure-parameters");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[73]+1)))(3,*((C_word*)lf[73]+1),t3,((C_word*)t0)[4]);}

/* k1634 in k1631 in k1624 in k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1635(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:384: eval-sequence");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(4,*((C_word*)lf[2]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k1637 in k1631 in k1624 in k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1638(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1638,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1641,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:389: procedure-environment");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[75]+1)))(3,*((C_word*)lf[75]+1),t2,((C_word*)t0)[4]);}

/* first-exp in k409 in k407 */
static void C_ccall f_755(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_755,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* cond-actions in k409 in k407 */
static void C_ccall f_857(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_857,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* variable? in k409 in k407 */
static void C_ccall f_485(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_485,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_symbolp(t2));}

/* cond-predicate in k409 in k407 */
static void C_ccall f_851(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_851,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(t2));}

/* k1493 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1494(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:355: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1624 in k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1626(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1626,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1632,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:385: procedure-body");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[74]+1)))(3,*((C_word*)lf[74]+1),t2,((C_word*)t0)[4]);}
else{
C_trace("4.1:390: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[66]+1)))(4,*((C_word*)lf[66]+1),((C_word*)t0)[2],lf[123],((C_word*)t0)[4]);}}

/* k1490 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1491(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:355: true?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[68]+1)))(3,*((C_word*)lf[68]+1),((C_word*)t0)[2],t1);}

/* last-exp? in k409 in k407 */
static void C_ccall f_746(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_746,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_nullp(t3));}

/* begin-actions in k409 in k407 */
static void C_ccall f_740(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_740,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* self-evaluating? in k409 in k407 */
static void C_ccall f_470(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_470,3,t0,t1,t2);}
if(C_truep(C_i_numberp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,t2);}
else{
t3=C_i_stringp(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,(C_truep(t3)?t2:*((C_word*)lf[1]+1)));}}

/* k1481 in k1474 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1482(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:356: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* apply in k1258 in k409 in k407 */
static void C_ccall f_1610(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_1610,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1617,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:381: primitive-procedure?");
((C_proc3)C_fast_retrieve_symbol_proc(lf[95]))(3,*((C_word*)lf[95]+1),t4,t2);}

/* k1487 in k1474 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1488(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:357: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1617(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1617,2,t0,t1);}
if(C_truep(t1)){
C_trace("4.1:382: apply-primitive-procedure");
((C_proc4)C_fast_retrieve_symbol_proc(lf[106]))(4,*((C_word*)lf[106]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1626,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("4.1:383: compound-procedure?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[72]+1)))(3,*((C_word*)lf[72]+1),t2,((C_word*)t0)[3]);}}

/* k1474 in k1540 in k1531 in k1522 in k1513 in k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1476(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1476,2,t0,t1);}
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1482,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:356: if-consequent");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[42]+1)))(3,*((C_word*)lf[42]+1),t2,((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1488,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace("4.1:357: if-alternative");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[43]+1)))(3,*((C_word*)lf[43]+1),t2,((C_word*)t0)[4]);}}

/* definition-variable in k409 in k407 */
static void C_ccall f_623(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_623,3,t0,t1,t2);}
t3=C_i_cadr(t2);
if(C_truep(C_i_symbolp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_cadr(t2));}
else{
C_trace("4.1:88: caadr");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[23]+1)))(3,*((C_word*)lf[23]+1),t1,t2);}}

/* k1460 in k1457 in k1450 in list-of-values in eval in k1258 in k409 in k407 */
static void C_ccall f_1461(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1461,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* k1466 in k1450 in list-of-values in eval in k1258 in k409 in k407 */
static void C_ccall f_1467(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:350: eval");
((C_proc4)C_fast_retrieve_symbol_proc(lf[3]))(4,*((C_word*)lf[3]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k1463 in k1457 in k1450 in list-of-values in eval in k1258 in k409 in k407 */
static void C_ccall f_1464(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:351: list-of-values");
t2=((C_word*)((C_word*)t0)[2])[1];
f_1445(t2,((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* application? in k409 in k407 */
static void C_ccall f_794(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_794,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_pairp(t2));}

/* definition? in k409 in k407 */
static void C_ccall f_617(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_617,3,t0,t1,t2);}
C_trace("4.1:84: tagged-list?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(4,*((C_word*)lf[19]+1),t1,t2,lf[32]);}

/* assignment-value in k409 in k407 */
static void C_ccall f_611(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_611,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* k778 in sequence->exp in k409 in k407 */
static void C_ccall f_780(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(t1)){
C_trace("4.1:135: first-seq");
((C_proc3)C_fast_retrieve_symbol_proc(lf[49]))(3,*((C_word*)lf[49]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}
else{
C_trace("4.1:136: make-begin");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[50]+1)))(3,*((C_word*)lf[50]+1),((C_word*)t0)[2],((C_word*)t0)[3]);}}

/* make-begin in k409 in k407 */
static void C_ccall f_788(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_788,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_a_i_cons(&a,2,lf[46],t2));}

/* operator in k409 in k407 */
static void C_ccall f_800(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_800,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_car(*((C_word*)lf[52]+1)));}

/* operands in k409 in k407 */
static void C_ccall f_806(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_806,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cdr(t2));}

/* k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1500(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1500,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[3]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1506,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:361: variable?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[17]+1)))(3,*((C_word*)lf[17]+1),t2,((C_word*)t0)[3]);}}

/* k1640 in k1637 in k1631 in k1624 in k1615 in apply in k1258 in k409 in k407 */
static void C_ccall f_1641(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("4.1:386: extend-environment");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[83]+1)))(5,*((C_word*)lf[83]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* k1504 in k1498 in eval in k1258 in k409 in k407 */
static void C_ccall f_1506(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1506,2,t0,t1);}
if(C_truep(t1)){
C_trace("4.1:361: lookup-variable-value");
((C_proc4)C_fast_retrieve_symbol_proc(lf[121]))(4,*((C_word*)lf[121]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1515,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
C_trace("4.1:362: quoted?");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[18]+1)))(3,*((C_word*)lf[18]+1),t2,((C_word*)t0)[3]);}}

/* k1646 in k1258 in k409 in k407 */
static void C_ccall f_1648(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_1648,2,t0,t1);}
t2=C_mutate((C_word*)lf[114]+1 /* (set! the-global-environment ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1650,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("4.1:398: driver-loop");
((C_proc2)C_fast_retrieve_symbol_proc(lf[111]))(2,*((C_word*)lf[111]+1),t3);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[190] = {
{"f_1524:_34_2e1",(void*)f_1524},
{"f_728:_34_2e1",(void*)f_728},
{"f_1515:_34_2e1",(void*)f_1515},
{"f_1083:_34_2e1",(void*)f_1083},
{"f_1089:_34_2e1",(void*)f_1089},
{"f_734:_34_2e1",(void*)f_734},
{"f_710:_34_2e1",(void*)f_710},
{"f_465:_34_2e1",(void*)f_465},
{"f_1542:_34_2e1",(void*)f_1542},
{"f_462:_34_2e1",(void*)f_462},
{"f_468:_34_2e1",(void*)f_468},
{"f_704:_34_2e1",(void*)f_704},
{"f_901:_34_2e1",(void*)f_901},
{"f_921:_34_2e1",(void*)f_921},
{"f_1180:_34_2e1",(void*)f_1180},
{"f_1186:_34_2e1",(void*)f_1186},
{"f_1189:_34_2e1",(void*)f_1189},
{"f_455:_34_2e1",(void*)f_455},
{"f_453:_34_2e1",(void*)f_453},
{"f_1533:_34_2e1",(void*)f_1533},
{"f_450:_34_2e1",(void*)f_450},
{"f_459:_34_2e1",(void*)f_459},
{"f_1060:_34_2e1",(void*)f_1060},
{"f_1064:_34_2e1",(void*)f_1064},
{"f_1068:_34_2e1",(void*)f_1068},
{"f_954:_34_2e1",(void*)f_954},
{"f_1101:_34_2e1",(void*)f_1101},
{"f_1560:_34_2e1",(void*)f_1560},
{"f_1566:_34_2e1",(void*)f_1566},
{"toplevel:_34_2e1",(void*)C_toplevel},
{"f_1551:_34_2e1",(void*)f_1551},
{"f_1557:_34_2e1",(void*)f_1557},
{"f_936:_34_2e1",(void*)f_936},
{"f_542:_34_2e1",(void*)f_542},
{"f_930:_34_2e1",(void*)f_930},
{"f_1127:_34_2e1",(void*)f_1127},
{"f_1584:_34_2e1",(void*)f_1584},
{"f_1072:_34_2e1",(void*)f_1072},
{"f_1287:_34_2e1",(void*)f_1287},
{"f_966:_34_2e1",(void*)f_966},
{"f_530:_34_2e1",(void*)f_530},
{"f_960:_34_2e1",(void*)f_960},
{"f_1572:_34_2e1",(void*)f_1572},
{"f_1578:_34_2e1",(void*)f_1578},
{"f_1021:_34_2e1",(void*)f_1021},
{"f_1278:_34_2e1",(void*)f_1278},
{"f_994:_34_2e1",(void*)f_994},
{"f_1148:_34_2e1",(void*)f_1148},
{"f_1262:_34_2e1",(void*)f_1262},
{"f_1260:_34_2e1",(void*)f_1260},
{"f_1268:_34_2e1",(void*)f_1268},
{"f_948:_34_2e1",(void*)f_948},
{"f_599:_34_2e1",(void*)f_599},
{"f_942:_34_2e1",(void*)f_942},
{"f_1135:_34_2e1",(void*)f_1135},
{"f_1131:_34_2e1",(void*)f_1131},
{"f_1596:_34_2e1",(void*)f_1596},
{"f_1599:_34_2e1",(void*)f_1599},
{"f_1253:_34_2e1",(void*)f_1253},
{"f_1590:_34_2e1",(void*)f_1590},
{"f_1250:_34_2e1",(void*)f_1250},
{"f_1256:_34_2e1",(void*)f_1256},
{"f_584:_34_2e1",(void*)f_584},
{"f_978:_34_2e1",(void*)f_978},
{"f_972:_34_2e1",(void*)f_972},
{"f_1246:_34_2e1",(void*)f_1246},
{"f_1240:_34_2e1",(void*)f_1240},
{"f_1242:_34_2e1",(void*)f_1242},
{"f_1248:_34_2e1",(void*)f_1248},
{"f_578:_34_2e1",(void*)f_578},
{"f_1237:_34_2e1",(void*)f_1237},
{"f_683:_34_2e1",(void*)f_683},
{"f_988:_34_2e1",(void*)f_988},
{"f_982:_34_2e1",(void*)f_982},
{"f_1399:_34_2e1",(void*)f_1399},
{"f_1397:_34_2e1",(void*)f_1397},
{"f_1393:_34_2e1",(void*)f_1393},
{"f_671:_34_2e1",(void*)f_671},
{"f_677:_34_2e1",(void*)f_677},
{"f_665:_34_2e1",(void*)f_665},
{"f_1092:_34_2e1",(void*)f_1092},
{"f_518:_34_2e1",(void*)f_518},
{"f_657:_34_2e1",(void*)f_657},
{"f_910:_34_2e1",(void*)f_910},
{"f_919:_34_2e1",(void*)f_919},
{"f_916:_34_2e1",(void*)f_916},
{"f_913:_34_2e1",(void*)f_913},
{"f_506:_34_2e1",(void*)f_506},
{"f_812:_34_2e1",(void*)f_812},
{"f_818:_34_2e1",(void*)f_818},
{"f_1358:_34_2e1",(void*)f_1358},
{"f_692:_34_2e1",(void*)f_692},
{"f_698:_34_2e1",(void*)f_698},
{"f_1381:_34_2e1",(void*)f_1381},
{"f_1383:_34_2e1",(void*)f_1383},
{"f_1385:_34_2e1",(void*)f_1385},
{"f_1388:_34_2e1",(void*)f_1388},
{"f_563:_34_2e1",(void*)f_563},
{"f_1371:_34_2e1",(void*)f_1371},
{"f_1375:_34_2e1",(void*)f_1375},
{"f_1379:_34_2e1",(void*)f_1379},
{"f_551:_34_2e1",(void*)f_551},
{"f_1452:_34_2e1",(void*)f_1452},
{"f_1458:_34_2e1",(void*)f_1458},
{"f_1441:_34_2e1",(void*)f_1441},
{"f_1445:_34_2e1",(void*)f_1445},
{"f_1317:_34_2e1",(void*)f_1317},
{"f_1433:_34_2e1",(void*)f_1433},
{"f_1436:_34_2e1",(void*)f_1436},
{"f_1344:_34_2e1",(void*)f_1344},
{"f_641:_34_2e1",(void*)f_641},
{"f_1298:_34_2e1",(void*)f_1298},
{"f_1424:_34_2e1",(void*)f_1424},
{"f_1333:_34_2e1",(void*)f_1333},
{"f_1198:_34_2e1",(void*)f_1198},
{"f_1194:_34_2e1",(void*)f_1194},
{"f_1410:_34_2e1",(void*)f_1410},
{"f_1412:_34_2e1",(void*)f_1412},
{"f_1417:_34_2e1",(void*)f_1417},
{"f_1364:_34_2e1",(void*)f_1364},
{"f_849:_34_2e1",(void*)f_849},
{"f_1401:_34_2e1",(void*)f_1401},
{"f_842:_34_2e1",(void*)f_842},
{"f_1406:_34_2e1",(void*)f_1406},
{"f_429:_34_2e1",(void*)f_429},
{"f_427:_34_2e1",(void*)f_427},
{"f_421:_34_2e1",(void*)f_421},
{"f_836:_34_2e1",(void*)f_836},
{"f_830:_34_2e1",(void*)f_830},
{"f_1000:_34_2e1",(void*)f_1000},
{"f_1602:_34_2e1",(void*)f_1602},
{"f_1605:_34_2e1",(void*)f_1605},
{"f_414:_34_2e1",(void*)f_414},
{"f_410:_34_2e1",(void*)f_410},
{"f_1034:_34_2e1",(void*)f_1034},
{"f_824:_34_2e1",(void*)f_824},
{"f_1320:_34_2e1",(void*)f_1320},
{"f_408:_34_2e1",(void*)f_408},
{"f_444:_34_2e1",(void*)f_444},
{"f_440:_34_2e1",(void*)f_440},
{"f_447:_34_2e1",(void*)f_447},
{"f_605:_34_2e1",(void*)f_605},
{"f_889:_34_2e1",(void*)f_889},
{"f_435:_34_2e1",(void*)f_435},
{"f_1652:_34_2e1",(void*)f_1652},
{"f_1650:_34_2e1",(void*)f_1650},
{"f_438:_34_2e1",(void*)f_438},
{"f_1655:_34_2e1",(void*)f_1655},
{"f_872:_34_2e1",(void*)f_872},
{"f_870:_34_2e1",(void*)f_870},
{"f_761:_34_2e1",(void*)f_761},
{"f_767:_34_2e1",(void*)f_767},
{"f_491:_34_2e1",(void*)f_491},
{"f_497:_34_2e1",(void*)f_497},
{"f_863:_34_2e1",(void*)f_863},
{"f_1200:_34_2e1",(void*)f_1200},
{"f_1632:_34_2e1",(void*)f_1632},
{"f_1635:_34_2e1",(void*)f_1635},
{"f_1638:_34_2e1",(void*)f_1638},
{"f_755:_34_2e1",(void*)f_755},
{"f_857:_34_2e1",(void*)f_857},
{"f_485:_34_2e1",(void*)f_485},
{"f_851:_34_2e1",(void*)f_851},
{"f_1494:_34_2e1",(void*)f_1494},
{"f_1626:_34_2e1",(void*)f_1626},
{"f_1491:_34_2e1",(void*)f_1491},
{"f_746:_34_2e1",(void*)f_746},
{"f_740:_34_2e1",(void*)f_740},
{"f_470:_34_2e1",(void*)f_470},
{"f_1482:_34_2e1",(void*)f_1482},
{"f_1610:_34_2e1",(void*)f_1610},
{"f_1488:_34_2e1",(void*)f_1488},
{"f_1617:_34_2e1",(void*)f_1617},
{"f_1476:_34_2e1",(void*)f_1476},
{"f_623:_34_2e1",(void*)f_623},
{"f_1461:_34_2e1",(void*)f_1461},
{"f_1467:_34_2e1",(void*)f_1467},
{"f_1464:_34_2e1",(void*)f_1464},
{"f_794:_34_2e1",(void*)f_794},
{"f_617:_34_2e1",(void*)f_617},
{"f_611:_34_2e1",(void*)f_611},
{"f_780:_34_2e1",(void*)f_780},
{"f_788:_34_2e1",(void*)f_788},
{"f_800:_34_2e1",(void*)f_800},
{"f_806:_34_2e1",(void*)f_806},
{"f_1500:_34_2e1",(void*)f_1500},
{"f_1641:_34_2e1",(void*)f_1641},
{"f_1506:_34_2e1",(void*)f_1506},
{"f_1648:_34_2e1",(void*)f_1648},
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
o|eliminated procedure checks: 6 
o|safe globals: (setup-environment define-variable! set-variable-value! look-up-variable-value extend-environment add-binding-to-frame! frame-values frame-variables make-frame the-empty-env first-frame enclosing-environment procedure-environment procedure-body procedure-parameters compound-procedure? make-procedure false? true? expand-clauses cond->if cond-actions cond-predicate cond-else-clauses cond-clauses cond? rest-operands first-operand no-operands? operands operator application? make-begin sequence->exp rest-exps first-exp last-exp? begin-actions begin? make-if if-alternative if-consequent if-predicate if? make-lambda lambda-body lambda-parameters lambda? definition-value definition-variable definition? assignment-value assignment-variable assignment? tagged-list? text-of-quotation cadddr cdddr cddr cdadr caadr caddr cadr quoted? variable? self-evaluating? eval-definition eval-assignment eval-sequence false true) 
o|Removed `not' forms: 1 
o|propagated global variable: g227231 primitive-procedures 
o|propagated global variable: g254258 primitive-procedures 
o|contracted procedure: "(4.1:365) eval-if297" 
o|replaced variables: 193 
o|removed binding forms: 100 
o|removed binding forms: 157 
o|simplifications: ((##core#call . 136) (if . 2)) 
o|  call simplifications:
o|    apply
o|    ##sys#check-list	2
o|    ##sys#setslot	2
o|    ##sys#slot	4
o|    =
o|    length	4
o|    <
o|    set-car!	3
o|    set-cdr!
o|    not
o|    list	9
o|    cdddr
o|    null?	9
o|    cadddr	2
o|    cons	10
o|    cddr	2
o|    caddr	4
o|    pair?	4
o|    eq?	9
o|    cadr	10
o|    cdr	31
o|    car	20
o|    symbol?	3
o|    number?
o|    string?
o|contracted procedure: k475 
o|contracted procedure: k481 
o|contracted procedure: k503 
o|contracted procedure: k515 
o|contracted procedure: k512 
o|contracted procedure: k527 
o|contracted procedure: k524 
o|contracted procedure: k539 
o|contracted procedure: k536 
o|contracted procedure: k548 
o|contracted procedure: k560 
o|contracted procedure: k557 
o|contracted procedure: k575 
o|contracted procedure: k572 
o|contracted procedure: k569 
o|contracted procedure: k589 
o|contracted procedure: k596 
o|contracted procedure: k638 
o|contracted procedure: k628 
o|contracted procedure: k662 
o|contracted procedure: k646 
o|contracted procedure: k659 
o|contracted procedure: k689 
o|contracted procedure: k725 
o|contracted procedure: k715 
o|contracted procedure: k752 
o|contracted procedure: k772 
o|contracted procedure: k877 
o|contracted procedure: k880 
o|contracted procedure: k882 
o|contracted procedure: k893 
o|contracted procedure: k927 
o|contracted procedure: k1018 
o|contracted procedure: k1015 
o|contracted procedure: k1003 
o|contracted procedure: k1012 
o|contracted procedure: k1009 
o|contracted procedure: k1054 
o|contracted procedure: k1057 
o|contracted procedure: k1026 
o|contracted procedure: k1048 
o|contracted procedure: k1051 
o|contracted procedure: k1038 
o|contracted procedure: k1076 
o|contracted procedure: k1093 
o|contracted procedure: k1121 
o|contracted procedure: k1105 
o|contracted procedure: k1115 
o|contracted procedure: k1118 
o|contracted procedure: k1140 
o|contracted procedure: k1168 
o|contracted procedure: k1152 
o|contracted procedure: k1162 
o|contracted procedure: k1165 
o|contracted procedure: k1173 
o|contracted procedure: k1205 
o|contracted procedure: k1230 
o|contracted procedure: k1214 
o|contracted procedure: k1224 
o|contracted procedure: k1227 
o|contracted procedure: k1657 
o|contracted procedure: k1660 
o|contracted procedure: k1663 
o|contracted procedure: k1666 
o|contracted procedure: k1274 
o|contracted procedure: k1281 
o|contracted procedure: k1292 
o|contracted procedure: k1314 
o|contracted procedure: k1311 
o|contracted procedure: k1295 
o|contracted procedure: k1304 
o|propagated global variable: g227231 primitive-procedures 
o|contracted procedure: k1325 
o|contracted procedure: k1327 
o|contracted procedure: k1338 
o|contracted procedure: k1341 
o|contracted procedure: k1350 
o|contracted procedure: k1360 
o|propagated global variable: g254258 primitive-procedures 
o|contracted procedure: k1429 
o|simplifications: ((let . 14)) 
o|removed binding forms: 78 
o|customizable procedures: (list-of-values296 k1343 map-loop242260 k1297 map-loop215232 scan195 scan181 env-loop179 env-loop164 scan166) 
o|calls to known targets: 21 
o|identified direct recursive calls: f_1068 1 
o|identified direct recursive calls: f_1135 1 
o|identified direct recursive calls: f_1200 1 
o|fast box initializations: 8 
*/
/* end of file */
