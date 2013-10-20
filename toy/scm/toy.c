/* Generated from toy.scm by the CHICKEN compiler
   http://www.call-cc.org
   2013-10-15 19:59
   Version 4.8.0.4 (stability/4.8.0) (rev 578619b)
   macosx-unix-clang-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2013-07-15 on aeryn.xorinia.dim (Darwin)
   command line: toy.scm
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[61];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,9),40,99,97,100,114,32,120,49,41,0,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,99,97,100,100,114,32,120,51,41,0,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,11),40,99,97,100,100,100,114,32,120,53,41,0,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,10),40,99,100,100,100,114,32,120,55,41,0,0,0,0,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,18),40,102,95,50,53,49,32,112,117,115,104,45,118,97,108,49,54,41,0,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,13),40,102,95,50,51,49,32,109,115,103,49,50,41,0,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,18),40,109,97,107,101,45,115,116,97,99,107,32,115,105,122,101,57,41,0,0,0,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,23),40,115,116,97,99,107,45,100,105,115,112,108,97,121,32,115,116,97,99,107,50,50,41,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,31),40,115,116,97,99,107,45,112,117,115,104,32,115,116,97,99,107,50,52,32,112,117,115,104,95,118,97,108,50,53,41,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,19),40,115,116,97,99,107,45,112,111,112,32,115,116,97,99,107,50,57,41,0,0,0,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,22),40,115,116,97,99,107,45,108,101,110,103,116,104,32,115,116,97,99,107,51,51,41,0,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,33),40,101,120,116,101,110,100,45,101,110,118,32,110,101,119,45,101,110,118,51,53,32,98,97,115,101,45,101,110,118,51,54,41,0,0,0,0,0,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,23),40,102,114,97,109,101,45,118,97,114,45,99,111,117,110,116,32,101,110,118,51,56,41,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,18),40,101,110,118,45,108,101,110,103,116,104,32,101,110,118,52,48,41,0,0,0,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,36),40,105,110,63,45,105,116,101,114,32,118,97,114,45,110,97,109,101,52,53,32,108,105,115,116,52,54,32,99,111,117,110,116,52,55,41,0,0,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,23),40,105,110,63,32,118,97,114,45,110,97,109,101,52,50,32,108,105,115,116,52,51,41,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,59),40,99,111,109,112,105,108,101,45,108,111,111,107,117,112,45,105,116,101,114,32,118,97,114,45,110,97,109,101,53,55,32,101,110,118,53,56,32,114,101,116,117,114,110,53,57,32,102,114,97,109,101,45,110,117,109,54,48,41,0,0,0,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,42),40,99,111,109,112,105,108,101,45,108,111,111,107,117,112,32,118,97,114,45,110,97,109,101,53,51,32,101,110,118,53,52,32,114,101,116,117,114,110,53,53,41,0,0,0,0,0,0};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,25),40,108,97,109,98,100,97,45,112,97,114,97,109,101,116,101,114,115,32,101,120,112,54,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,19),40,108,97,109,98,100,97,45,98,111,100,121,32,101,120,112,54,54,41,0,0,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,16),40,105,102,45,106,117,100,103,101,32,101,120,112,54,56,41};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,21),40,105,102,45,99,111,110,115,101,113,117,101,110,116,32,101,120,112,55,48,41,0,0,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,22),40,105,102,45,97,108,116,101,114,110,97,116,105,118,101,32,101,120,112,55,50,41,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,27),40,97,115,115,105,103,110,109,101,110,116,45,118,97,114,105,97,98,108,101,32,101,120,112,55,52,41,0,0,0,0,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,24),40,97,115,115,105,103,110,109,101,110,116,45,118,97,108,117,101,32,101,120,112,55,54,41};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,27),40,100,101,102,105,110,105,116,105,111,110,45,118,97,114,105,97,98,108,101,32,101,120,112,55,56,41,0,0,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,24),40,100,101,102,105,110,105,116,105,111,110,45,118,97,108,117,101,32,101,120,112,56,48,41};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,52),40,99,111,109,112,105,108,101,45,100,101,102,105,110,105,116,105,111,110,32,118,97,114,105,97,98,108,101,56,50,32,118,97,108,117,101,56,51,32,101,110,118,56,52,32,110,101,120,116,56,53,41,0,0,0,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,51),40,99,111,109,112,105,108,101,45,97,114,103,117,109,101,110,116,115,45,105,116,101,114,32,97,114,103,117,109,101,110,116,115,57,49,32,101,110,118,57,50,32,114,101,115,117,108,116,57,51,41,0,0,0,0,0};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,37),40,99,111,109,112,105,108,101,45,97,114,103,117,109,101,110,116,115,32,97,114,103,117,109,101,110,116,115,56,56,32,101,110,118,56,57,41,0,0,0};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,51),40,99,111,109,112,105,108,101,45,112,114,111,99,101,100,117,114,101,32,112,114,111,99,57,54,32,97,114,103,117,109,101,110,116,115,57,55,32,101,110,118,57,56,32,110,101,120,116,57,57,41,0,0,0,0,0};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,17),40,102,95,53,53,56,32,110,49,48,56,32,109,49,48,57,41,0,0,0,0,0,0,0};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,17),40,102,95,54,53,54,32,110,49,49,57,32,109,49,50,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,31),40,99,111,109,112,105,108,101,32,101,120,112,49,48,50,32,101,110,118,49,48,51,32,110,101,120,116,49,48,52,41,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,20),40,115,101,116,117,112,45,115,121,109,98,111,108,45,116,97,98,108,101,41,0,0,0,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,31),40,100,105,115,112,108,97,121,45,105,110,115,116,114,117,99,116,105,111,110,115,32,105,110,115,116,115,49,50,53,41,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_284)
static void C_ccall f_284(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_289)
static void C_ccall f_289(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_627)
static void C_ccall f_627(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_625)
static void C_ccall f_625(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_736)
static void C_ccall f_736(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_733)
static void C_ccall f_733(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_730)
static void C_ccall f_730(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_425)
static void C_ccall f_425(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_629)
static void C_ccall f_629(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_617)
static void C_ccall f_617(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_614)
static void C_ccall f_614(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_611)
static void C_ccall f_611(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_546)
static void C_ccall f_546(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_544)
static void C_ccall f_544(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_413)
static void C_ccall f_413(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_419)
static void C_ccall f_419(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_407)
static void C_ccall f_407(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_401)
static void C_ccall f_401(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_608)
static void C_ccall f_608(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_304)
static void C_ccall f_304(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_176)
static void C_ccall f_176(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_178)
static void C_ccall f_178(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_174)
static void C_ccall f_174(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_503)
static void C_ccall f_503(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_507)
static void C_fcall f_507(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_278)
static void C_ccall f_278(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_271)
static void C_ccall f_271(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_728)
static void C_ccall f_728(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_715)
static void C_ccall f_715(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_713)
static void C_ccall f_713(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_325)
static void C_ccall f_325(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_329)
static void C_fcall f_329(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_251)
static void C_ccall f_251(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_319)
static void C_ccall f_319(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_596)
static void C_ccall f_596(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_310)
static void C_ccall f_310(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_199)
static void C_ccall f_199(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_699)
static void C_ccall f_699(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_695)
static void C_ccall f_695(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_187)
static void C_ccall f_187(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_226)
static void C_ccall f_226(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_673)
static void C_ccall f_673(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_675)
static void C_ccall f_675(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_538)
static void C_ccall f_538(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_534)
static void C_ccall f_534(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_363)
static void C_fcall f_363(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_214)
static void C_ccall f_214(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_359)
static void C_ccall f_359(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_708)
static void C_ccall f_708(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_558)
static void C_ccall f_558(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_468)
static void C_ccall f_468(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_464)
static void C_ccall f_464(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5) C_noret;
C_noret_decl(f_656)
static void C_ccall f_656(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_651)
static void C_ccall f_651(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_375)
static void C_ccall f_375(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_458)
static void C_ccall f_458(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_452)
static void C_ccall f_452(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_641)
static void C_ccall f_641(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_446)
static void C_ccall f_446(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_440)
static void C_ccall f_440(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_293)
static void C_ccall f_293(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_649)
static void C_ccall f_649(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_230)
static void C_ccall f_230(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_231)
static void C_ccall f_231(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_298)
static void C_ccall f_298(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_638)
static void C_ccall f_638(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_529)
static void C_ccall f_529(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_526)
static void C_ccall f_526(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_280)
static void C_ccall f_280(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;

C_noret_decl(trf_507)
static void C_fcall trf_507(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_507(void *dummy){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
f_507(t0,t1,t2,t3,t4);}

C_noret_decl(trf_329)
static void C_fcall trf_329(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_329(void *dummy){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
f_329(t0,t1,t2,t3,t4);}

C_noret_decl(trf_363)
static void C_fcall trf_363(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_363(void *dummy){
C_word t5=C_pick(0);
C_word t4=C_pick(1);
C_word t3=C_pick(2);
C_word t2=C_pick(3);
C_word t1=C_pick(4);
C_word t0=C_pick(5);
C_adjust_stack(-6);
f_363(t0,t1,t2,t3,t4,t5);}

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

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

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

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

/* k283 in stack-push in k175 in k173 */
static void C_ccall f_284(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:32: g26");
t2=t1;
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],((C_word*)t0)[3]);}

/* stack-pop in k175 in k173 */
static void C_ccall f_289(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_289,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_293,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:33: stack");
t4=t2;
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,lf[8]);}

/* k626 in k624 in compile in k175 in k173 */
static void C_ccall f_627(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_627,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_629,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
C_trace("toy.scm:176: if-judge");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[24]+1)))(3,*((C_word*)lf[24]+1),t2,((C_word*)t0)[6]);}

/* k624 in compile in k175 in k173 */
static void C_ccall f_625(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_625,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_627,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
C_trace("toy.scm:175: if-alternative");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[26]+1)))(3,*((C_word*)lf[26]+1),t2,((C_word*)t0)[5]);}

/* k735 in k175 in k173 */
static void C_ccall f_736(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:224: display-instructions");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[53]+1)))(3,*((C_word*)lf[53]+1),((C_word*)t0)[2],t1);}

/* k731 in k727 in k175 in k173 */
static void C_ccall f_733(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k729 in k727 in k175 in k173 */
static void C_ccall f_730(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* if-alternative in k175 in k173 */
static void C_ccall f_425(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_425,3,t0,t1,t2);}
t3=C_i_cdddr(t2);
t4=C_i_nullp(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,(C_truep(t4)?lf[27]:C_i_cadddr(t2)));}

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
if(!C_demand_2(656)){
C_save(t1);
C_rereclaim2(656*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,61);
lf[0]=C_h_intern(&lf[0],4,"cadr");
lf[1]=C_h_intern(&lf[1],5,"caddr");
lf[2]=C_h_intern(&lf[2],6,"cadddr");
lf[3]=C_h_intern(&lf[3],5,"cdddr");
lf[4]=C_h_intern(&lf[4],10,"make-stack");
lf[5]=C_h_intern(&lf[5],6,"length");
lf[6]=C_h_intern(&lf[6],5,"stack");
lf[7]=C_h_intern(&lf[7],4,"push");
lf[8]=C_h_intern(&lf[8],3,"pop");
lf[9]=C_h_intern(&lf[9],11,"make-vector");
lf[10]=C_h_intern(&lf[10],13,"stack-display");
lf[11]=C_h_intern(&lf[11],7,"display");
lf[12]=C_h_intern(&lf[12],10,"stack-push");
lf[13]=C_h_intern(&lf[13],9,"stack-pop");
lf[14]=C_h_intern(&lf[14],12,"stack-length");
lf[15]=C_h_intern(&lf[15],10,"extend-env");
lf[16]=C_h_intern(&lf[16],15,"frame-var-count");
lf[17]=C_h_intern(&lf[17],10,"env-length");
lf[18]=C_h_intern(&lf[18],3,"in\077");
lf[19]=C_h_intern(&lf[19],14,"compile-lookup");
lf[20]=C_h_intern(&lf[20],5,"error");
lf[21]=C_decode_literal(C_heaptop,"\376B\000\000\016Undefined var ");
lf[22]=C_h_intern(&lf[22],17,"lambda-parameters");
lf[23]=C_h_intern(&lf[23],11,"lambda-body");
lf[24]=C_h_intern(&lf[24],8,"if-judge");
lf[25]=C_h_intern(&lf[25],13,"if-consequent");
lf[26]=C_h_intern(&lf[26],14,"if-alternative");
lf[27]=C_h_intern(&lf[27],5,"false");
lf[28]=C_h_intern(&lf[28],19,"assignment-variable");
lf[29]=C_h_intern(&lf[29],16,"assignment-value");
lf[30]=C_h_intern(&lf[30],19,"definition-variable");
lf[31]=C_h_intern(&lf[31],16,"definition-value");
lf[32]=C_h_intern(&lf[32],18,"compile-definition");
lf[33]=C_h_intern(&lf[33],6,"assign");
lf[34]=C_h_intern(&lf[34],7,"compile");
lf[35]=C_h_intern(&lf[35],17,"compile-arguments");
lf[36]=C_h_intern(&lf[36],3,"car");
lf[37]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\010argument\376\377\016");
lf[38]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\005frame\376\377\016");
lf[39]=C_h_intern(&lf[39],17,"compile-procedure");
lf[40]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\005apply\376\377\016");
lf[41]=C_h_intern(&lf[41],3,"ref");
lf[42]=C_h_intern(&lf[42],5,"quote");
lf[43]=C_h_intern(&lf[43],8,"constant");
lf[44]=C_h_intern(&lf[44],6,"lambda");
lf[45]=C_h_intern(&lf[45],6,"return");
lf[46]=C_h_intern(&lf[46],7,"closure");
lf[47]=C_h_intern(&lf[47],2,"if");
lf[48]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\004test\376\377\016");
lf[49]=C_h_intern(&lf[49],4,"set!");
lf[50]=C_h_intern(&lf[50],6,"define");
lf[51]=C_h_intern(&lf[51],18,"setup-symbol-table");
lf[52]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\377\016\376\377\016");
lf[53]=C_h_intern(&lf[53],20,"display-instructions");
lf[54]=C_h_intern(&lf[54],5,"Done!");
lf[55]=C_h_intern(&lf[55],7,"newline");
lf[56]=C_h_intern(&lf[56],12,"symbol-table");
lf[57]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\003\000\000\002\376\001\000\000\001y\376\003\000\000\002\376\001\000\000\001x\376\377\016\376\377\016");
lf[58]=C_h_intern(&lf[58],1,"x");
lf[59]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\001\000\000\006lambda\376\003\000\000\002\376\003\000\000\002\376\001\000\000\001a\376\377\016\376\003\000\000\002\376\001\000\000\001a\376\377\016");
lf[60]=C_h_intern(&lf[60],25,"\003sysimplicit-exit-handler");
C_register_lf2(lf,61,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_174,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k628 in k626 in k624 in compile in k175 in k173 */
static void C_ccall f_629(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_629,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_638,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_641,a[2]=t2,a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("toy.scm:179: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),t3,((C_word*)t0)[5],((C_word*)t0)[3],((C_word*)t0)[6]);}

/* k616 in k610 in compile in k175 in k173 */
static void C_ccall f_617(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:167: extend-env");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(4,*((C_word*)lf[15]+1),((C_word*)t0)[2],t1,((C_word*)t0)[3]);}

/* k613 in k610 in compile in k175 in k173 */
static void C_ccall f_614(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:166: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,((C_word*)t0)[4]);}

/* k610 in compile in k175 in k173 */
static void C_ccall f_611(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_611,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_614,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_617,a[2]=t2,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:167: lambda-parameters");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[22]+1)))(3,*((C_word*)lf[22]+1),t3,((C_word*)t0)[5]);}

/* compile in k175 in k173 */
static void C_ccall f_546(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
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
C_word ab[9],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_546,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_symbolp(t2))){
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_558,a[2]=((C_word)li31),tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:155: compile-lookup");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(5,*((C_word*)lf[19]+1),t1,t2,t3,t5);}
else{
if(C_truep(C_i_pairp(t2))){
t5=C_i_car(t2);
t6=C_eqp(t5,lf[42]);
if(C_truep(t6)){
t7=C_i_cadr(t2);
t8=C_a_i_list2(&a,2,lf[43],t7);
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,C_a_i_cons(&a,2,t8,t4));}
else{
t7=C_eqp(t5,lf[44]);
if(C_truep(t7)){
t8=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_596,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t9=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_611,a[2]=t8,a[3]=t4,a[4]=t3,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:166: lambda-body");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[23]+1)))(3,*((C_word*)lf[23]+1),t9,t2);}
else{
t8=C_eqp(t5,lf[47]);
if(C_truep(t8)){
t9=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_625,a[2]=t1,a[3]=t3,a[4]=t4,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:174: if-consequent");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[25]+1)))(3,*((C_word*)lf[25]+1),t9,t2);}
else{
t9=C_eqp(t5,lf[49]);
if(C_truep(t9)){
t10=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_649,a[2]=t4,a[3]=t3,a[4]=t1,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:182: assignment-variable");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[28]+1)))(3,*((C_word*)lf[28]+1),t10,t2);}
else{
t10=C_eqp(t5,lf[50]);
if(C_truep(t10)){
t11=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_673,a[2]=t1,a[3]=t3,a[4]=t4,a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:195: definition-variable");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[30]+1)))(3,*((C_word*)lf[30]+1),t11,t2);}
else{
t11=C_i_car(t2);
t12=C_i_cdr(t2);
C_trace("toy.scm:200: compile-procedure");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[39]+1)))(6,*((C_word*)lf[39]+1),t1,t11,t12,t3,t4);}}}}}}
else{
t5=C_a_i_list2(&a,2,lf[43],t2);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_a_i_cons(&a,2,t5,t4));}}}

/* k543 in k537 in compile-procedure in k175 in k173 */
static void C_ccall f_544(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_544,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,t1,((C_word*)t0)[3]));}

/* if-judge in k175 in k173 */
static void C_ccall f_413(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_413,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* if-consequent in k175 in k173 */
static void C_ccall f_419(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_419,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* lambda-body in k175 in k173 */
static void C_ccall f_407(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_407,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* lambda-parameters in k175 in k173 */
static void C_ccall f_401(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_401,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k607 in k595 in compile in k175 in k173 */
static void C_ccall f_608(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[19],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_608,2,t0,t1);}
t2=C_i_length(t1);
t3=C_a_i_plus(&a,2,C_fix(1),t2);
t4=C_a_i_list2(&a,2,lf[45],t3);
t5=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_list3(&a,3,lf[46],((C_word*)t0)[3],t4));}

/* extend-env in k175 in k173 */
static void C_ccall f_304(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word ab[3],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_304,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_cons(&a,2,t2,t3));}

/* k175 in k173 */
static void C_ccall f_176(C_word c,C_word t0,C_word t1){
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
C_word ab[93],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_176,2,t0,t1);}
t2=C_mutate((C_word*)lf[0]+1 /* (set! cadr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_178,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[1]+1 /* (set! caddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_187,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[2]+1 /* (set! cadddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_199,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[3]+1 /* (set! cdddr ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_214,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[4]+1 /* (set! make-stack ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_226,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[10]+1 /* (set! stack-display ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_271,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[12]+1 /* (set! stack-push ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_280,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[13]+1 /* (set! stack-pop ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_289,a[2]=((C_word)li9),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[14]+1 /* (set! stack-length ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_298,a[2]=((C_word)li10),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[15]+1 /* (set! extend-env ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_304,a[2]=((C_word)li11),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[16]+1 /* (set! frame-var-count ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_310,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[17]+1 /* (set! env-length ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_319,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[18]+1 /* (set! in? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_325,a[2]=((C_word)li15),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[19]+1 /* (set! compile-lookup ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_359,a[2]=((C_word)li17),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate((C_word*)lf[22]+1 /* (set! lambda-parameters ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_401,a[2]=((C_word)li18),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate((C_word*)lf[23]+1 /* (set! lambda-body ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_407,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));
t18=C_mutate((C_word*)lf[24]+1 /* (set! if-judge ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_413,a[2]=((C_word)li20),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate((C_word*)lf[25]+1 /* (set! if-consequent ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_419,a[2]=((C_word)li21),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[26]+1 /* (set! if-alternative ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_425,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[28]+1 /* (set! assignment-variable ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_440,a[2]=((C_word)li23),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[29]+1 /* (set! assignment-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_446,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate((C_word*)lf[30]+1 /* (set! definition-variable ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_452,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate((C_word*)lf[31]+1 /* (set! definition-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_458,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate((C_word*)lf[32]+1 /* (set! compile-definition ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_464,a[2]=((C_word)li27),tmp=(C_word)a,a+=3,tmp));
t26=C_mutate((C_word*)lf[35]+1 /* (set! compile-arguments ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_503,a[2]=((C_word)li29),tmp=(C_word)a,a+=3,tmp));
t27=C_mutate((C_word*)lf[39]+1 /* (set! compile-procedure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_534,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[34]+1 /* (set! compile ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_546,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate((C_word*)lf[51]+1 /* (set! setup-symbol-table ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_695,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[53]+1 /* (set! display-instructions ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_699,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[56]+1 /* (set! symbol-table ...) */,lf[57]);
t32=C_mutate((C_word*)lf[58]+1 /* (set! x ...) */,lf[59]);
t33=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_728,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t34=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_736,a[2]=t33,tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:224: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),t34,*((C_word*)lf[58]+1),*((C_word*)lf[56]+1),C_SCHEME_END_OF_LIST);}

/* cadr in k175 in k173 */
static void C_ccall f_178(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_178,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_car(t3));}

/* k173 */
static void C_ccall f_174(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_174,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_176,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* compile-arguments in k175 in k173 */
static void C_ccall f_503(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[6],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_503,4,t0,t1,t2,t3);}
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_507,a[2]=t5,a[3]=((C_word)li28),tmp=(C_word)a,a+=4,tmp));
C_trace("toy.scm:143: compile-arguments-iter");
t7=((C_word*)t5)[1];
f_507(t7,t1,t2,t3,lf[38]);}

/* compile-arguments-iter in compile-arguments in k175 in k173 */
static void C_fcall f_507(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_507,NULL,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_nullp(t2))){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t4);}
else{
t5=C_i_cdr(t2);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_526,a[2]=t4,a[3]=((C_word*)t0)[2],a[4]=t1,a[5]=t5,a[6]=t3,tmp=(C_word)a,a+=7,tmp);
t7=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_529,a[2]=t6,tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:140: car");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[36]+1)))(5,*((C_word*)lf[36]+1),t7,t2,t3,lf[37]);}}

/* k277 in stack-display in k175 in k173 */
static void C_ccall f_278(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:31: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),((C_word*)t0)[2],t1);}

/* stack-display in k175 in k173 */
static void C_ccall f_271(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_271,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_278,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:31: stack");
t4=t2;
((C_proc3)C_fast_retrieve_proc(t4))(3,t4,t3,lf[6]);}

/* k727 in k175 in k173 */
static void C_ccall f_728(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_728,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_730,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_733,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[60]))(2,*((C_word*)lf[60]+1),t3);}

/* k714 in k712 in display-instructions in k175 in k173 */
static void C_ccall f_715(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("toy.scm:218: display-instructions");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[53]+1)))(3,*((C_word*)lf[53]+1),((C_word*)t0)[3],t2);}

/* k712 in display-instructions in k175 in k173 */
static void C_ccall f_713(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_713,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_715,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=C_i_car(((C_word*)t0)[2]);
C_trace("toy.scm:217: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),t2,t3);}

/* in? in k175 in k173 */
static void C_ccall f_325(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[6],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_325,4,t0,t1,t2,t3);}
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_329,a[2]=t5,a[3]=((C_word)li14),tmp=(C_word)a,a+=4,tmp));
C_trace("toy.scm:65: in?-iter");
t7=((C_word*)t5)[1];
f_329(t7,t1,t2,t3,C_fix(0));}

/* in?-iter in in? in k175 in k173 */
static void C_fcall f_329(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
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
a=C_alloc(4);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_329,NULL,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_nullp(t3))){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_fix(-1));}
else{
t5=C_i_car(t3);
t6=C_eqp(t2,t5);
if(C_truep(t6)){
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,t4);}
else{
t7=C_i_cdr(t3);
t8=C_a_i_plus(&a,2,t4,C_fix(1));
C_trace("toy.scm:64: in?-iter");
t10=t1;
t11=t2;
t12=t7;
t13=t8;
t1=t10;
t2=t11;
t3=t12;
t4=t13;
goto loop;}}}

/* f_251 */
static void C_ccall f_251(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_251,3,t0,t1,t2);}
t3=C_i_vector_set(((C_word*)t0)[2],((C_word*)((C_word*)t0)[3])[1],t2);
t4=C_a_i_plus(&a,2,((C_word*)((C_word*)t0)[3])[1],C_fix(1));
t5=C_mutate(((C_word *)((C_word*)t0)[3])+1,t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,t5);}

/* env-length in k175 in k173 */
static void C_ccall f_319(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_319,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_length(t2));}

/* k595 in compile in k175 in k173 */
static void C_ccall f_596(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_596,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_608,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:170: lambda-parameters");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[22]+1)))(2,*((C_word*)lf[22]+1),t2);}

/* frame-var-count in k175 in k173 */
static void C_ccall f_310(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_310,3,t0,t1,t2);}
t3=C_i_car(t2);
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_length(t3));}

/* cadddr in k175 in k173 */
static void C_ccall f_199(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_199,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=C_i_cdr(t4);
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_i_car(t5));}

/* display-instructions in k175 in k173 */
static void C_ccall f_699(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_699,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_708,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace("toy.scm:213: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[55]+1)))(2,*((C_word*)lf[55]+1),t3);}
else{
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_713,a[2]=t2,a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:216: newline");
((C_proc2)C_fast_retrieve_proc(*((C_word*)lf[55]+1)))(2,*((C_word*)lf[55]+1),t3);}}

/* setup-symbol-table in k175 in k173 */
static void C_ccall f_695(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_695,2,t0,t1);}
t2=t1;
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[52]);}

/* caddr in k175 in k173 */
static void C_ccall f_187(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_187,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_car(t4));}

/* make-stack in k175 in k173 */
static void C_ccall f_226(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_226,3,t0,t1,t2);}
t3=C_fix(0);
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_230,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:17: make-vector");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[9]+1)))(3,*((C_word*)lf[9]+1),t5,t2);}

/* k672 in compile in k175 in k173 */
static void C_ccall f_673(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_673,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_675,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:196: definition-value");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[31]+1)))(3,*((C_word*)lf[31]+1),t2,((C_word*)t0)[5]);}

/* k674 in k672 in compile in k175 in k173 */
static void C_ccall f_675(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:197: compile-definition");
((C_proc6)C_fast_retrieve_proc(*((C_word*)lf[32]+1)))(6,*((C_word*)lf[32]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1,((C_word*)t0)[4],((C_word*)t0)[5]);}

/* k537 in compile-procedure in k175 in k173 */
static void C_ccall f_538(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_538,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_544,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:147: compile-arguments");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[35]+1)))(4,*((C_word*)lf[35]+1),t2,((C_word*)t0)[3],((C_word*)t0)[4]);}

/* compile-procedure in k175 in k173 */
static void C_ccall f_534(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word ab[5],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_534,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_538,a[2]=t1,a[3]=t3,a[4]=t4,tmp=(C_word)a,a+=5,tmp);
C_trace("toy.scm:146: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),t6,t2,t4,lf[40]);}

/* compile-lookup-iter in compile-lookup in k175 in k173 */
static void C_fcall f_363(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_363,NULL,6,t0,t1,t2,t3,t4,t5);}
if(C_truep(C_i_nullp(t3))){
C_trace("toy.scm:71: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[20]+1)))(4,*((C_word*)lf[20]+1),t1,lf[21],t2);}
else{
t6=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_375,a[2]=t3,a[3]=t5,a[4]=((C_word*)t0)[2],a[5]=t1,a[6]=t2,a[7]=t4,tmp=(C_word)a,a+=8,tmp);
t7=C_i_car(t3);
C_trace("toy.scm:72: in?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[18]+1)))(4,*((C_word*)lf[18]+1),t6,t2,t7);}}

/* cdddr in k175 in k173 */
static void C_ccall f_214(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_214,3,t0,t1,t2);}
t3=C_i_cdr(t2);
t4=C_i_cdr(t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_i_cdr(t4));}

/* compile-lookup in k175 in k173 */
static void C_ccall f_359(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_359,5,t0,t1,t2,t3,t4);}
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_363,a[2]=t6,a[3]=((C_word)li16),tmp=(C_word)a,a+=4,tmp));
C_trace("toy.scm:77: compile-lookup-iter");
t8=((C_word*)t6)[1];
f_363(t8,t1,t2,t3,t4,C_fix(0));}

/* k707 in display-instructions in k175 in k173 */
static void C_ccall f_708(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:214: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(3,*((C_word*)lf[11]+1),((C_word*)t0)[2],lf[54]);}

/* f_558 in compile in k175 in k173 */
static void C_ccall f_558(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word ab[9],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_558,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_a_i_list3(&a,3,lf[41],t2,t3));}

/* k467 in compile-definition in k175 in k173 */
static void C_ccall f_468(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_468,2,t0,t1);}
if(C_truep(C_i_nequalp(t1,C_fix(-1)))){
t2=C_i_car(((C_word*)t0)[2]);
t3=C_i_length(t2);
t4=C_a_i_list3(&a,3,lf[33],C_fix(0),t3);
t5=C_a_i_cons(&a,2,t4,((C_word*)t0)[3]);
C_trace("toy.scm:117: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[2],t5);}
else{
t2=C_a_i_list3(&a,3,lf[33],C_fix(0),t1);
t3=C_a_i_cons(&a,2,t2,((C_word*)t0)[3]);
C_trace("toy.scm:119: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[2],t3);}}

/* compile-definition in k175 in k173 */
static void C_ccall f_464(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4,C_word t5){
C_word tmp;
C_word t6;
C_word t7;
C_word t8;
C_word ab[6],*a=ab;
if(c!=6) C_bad_argc_2(c,6,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr6,(void*)f_464,6,t0,t1,t2,t3,t4,t5);}
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_468,a[2]=t4,a[3]=t5,a[4]=t1,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
t7=C_i_car(t4);
C_trace("toy.scm:115: in?");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[18]+1)))(4,*((C_word*)lf[18]+1),t6,t2,t7);}

/* f_656 in k650 in k648 in compile in k175 in k173 */
static void C_ccall f_656(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[12],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_656,4,t0,t1,t2,t3);}
t4=C_a_i_list3(&a,3,lf[33],t2,t3);
t5=C_a_i_cons(&a,2,t4,((C_word*)t0)[2]);
C_trace("toy.scm:187: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),t1,((C_word*)t0)[3],((C_word*)t0)[4],t5);}

/* k650 in k648 in compile in k175 in k173 */
static void C_ccall f_651(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_651,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_656,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=((C_word)li32),tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:184: compile-lookup");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(5,*((C_word*)lf[19]+1),((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[3],t2);}

/* k374 in compile-lookup-iter in compile-lookup in k175 in k173 */
static void C_ccall f_375(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_375,2,t0,t1);}
if(C_truep(C_i_nequalp(t1,C_fix(-1)))){
t2=C_i_cdr(((C_word*)t0)[2]);
t3=C_a_i_plus(&a,2,((C_word*)t0)[3],C_fix(1));
C_trace("toy.scm:74: compile-lookup-iter");
t4=((C_word*)((C_word*)t0)[4])[1];
f_363(t4,((C_word*)t0)[5],((C_word*)t0)[6],t2,((C_word*)t0)[7],t3);}
else{
C_trace("toy.scm:75: return");
t2=((C_word*)t0)[7];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[5],((C_word*)t0)[3],t1);}}

/* definition-value in k175 in k173 */
static void C_ccall f_458(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_458,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* definition-variable in k175 in k173 */
static void C_ccall f_452(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_452,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k640 in k628 in k626 in k624 in compile in k175 in k173 */
static void C_ccall f_641(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:178: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* assignment-value in k175 in k173 */
static void C_ccall f_446(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_446,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_caddr(t2));}

/* assignment-variable in k175 in k173 */
static void C_ccall f_440(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_440,3,t0,t1,t2);}
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_i_cadr(t2));}

/* k292 in stack-pop in k175 in k173 */
static void C_ccall f_293(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:33: g30");
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k648 in compile in k175 in k173 */
static void C_ccall f_649(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_649,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_651,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
C_trace("toy.scm:183: assignment-value");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[29]+1)))(3,*((C_word*)lf[29]+1),t2,((C_word*)t0)[5]);}

/* k229 in make-stack in k175 in k173 */
static void C_ccall f_230(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_230,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_231,a[2]=((C_word*)t0)[3],a[3]=t1,a[4]=((C_word)li5),tmp=(C_word)a,a+=5,tmp));}

/* f_231 in k229 in make-stack in k175 in k173 */
static void C_ccall f_231(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[5],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_231,3,t0,t1,t2);}
t3=C_eqp(t2,lf[5]);
if(C_truep(t3)){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,((C_word*)((C_word*)t0)[2])[1]);}
else{
t4=C_eqp(t2,lf[6]);
if(C_truep(t4)){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,((C_word*)t0)[3]);}
else{
t5=C_eqp(t2,lf[7]);
if(C_truep(t5)){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_251,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[2],a[4]=((C_word)li4),tmp=(C_word)a,a+=5,tmp));}
else{
t6=C_eqp(t2,lf[8]);
if(C_truep(t6)){
t7=C_a_i_minus(&a,2,((C_word*)((C_word*)t0)[2])[1],C_fix(1));
t8=C_mutate(((C_word *)((C_word*)t0)[2])+1,t7);
t9=t1;
((C_proc2)(void*)(*((C_word*)t9+1)))(2,t9,((C_word*)((C_word*)t0)[2])[1]);}
else{
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_SCHEME_UNDEFINED);}}}}}

/* stack-length in k175 in k173 */
static void C_ccall f_298(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_298,3,t0,t1,t2);}
C_trace("toy.scm:34: stack");
t3=t2;
((C_proc3)C_fast_retrieve_proc(t3))(3,t3,t1,lf[5]);}

/* k637 in k628 in k626 in k624 in compile in k175 in k173 */
static void C_ccall f_638(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_638,2,t0,t1);}
t2=C_a_i_cons(&a,2,lf[48],t1);
C_trace("toy.scm:177: compile");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(5,*((C_word*)lf[34]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t2);}

/* k528 in compile-arguments-iter in compile-arguments in k175 in k173 */
static void C_ccall f_529(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("toy.scm:140: compile");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[34]+1)))(3,*((C_word*)lf[34]+1),((C_word*)t0)[2],t1);}

/* k525 in compile-arguments-iter in compile-arguments in k175 in k173 */
static void C_ccall f_526(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_526,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)t0)[2]);
C_trace("toy.scm:138: compile-arguments-iter");
t3=((C_word*)((C_word*)t0)[3])[1];
f_507(t3,((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[6],t2);}

/* stack-push in k175 in k173 */
static void C_ccall f_280(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_280,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_284,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace("toy.scm:32: stack");
t5=t2;
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,lf[7]);}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[71] = {
{"f_284:toy_2escm",(void*)f_284},
{"f_289:toy_2escm",(void*)f_289},
{"f_627:toy_2escm",(void*)f_627},
{"f_625:toy_2escm",(void*)f_625},
{"f_736:toy_2escm",(void*)f_736},
{"f_733:toy_2escm",(void*)f_733},
{"f_730:toy_2escm",(void*)f_730},
{"f_425:toy_2escm",(void*)f_425},
{"toplevel:toy_2escm",(void*)C_toplevel},
{"f_629:toy_2escm",(void*)f_629},
{"f_617:toy_2escm",(void*)f_617},
{"f_614:toy_2escm",(void*)f_614},
{"f_611:toy_2escm",(void*)f_611},
{"f_546:toy_2escm",(void*)f_546},
{"f_544:toy_2escm",(void*)f_544},
{"f_413:toy_2escm",(void*)f_413},
{"f_419:toy_2escm",(void*)f_419},
{"f_407:toy_2escm",(void*)f_407},
{"f_401:toy_2escm",(void*)f_401},
{"f_608:toy_2escm",(void*)f_608},
{"f_304:toy_2escm",(void*)f_304},
{"f_176:toy_2escm",(void*)f_176},
{"f_178:toy_2escm",(void*)f_178},
{"f_174:toy_2escm",(void*)f_174},
{"f_503:toy_2escm",(void*)f_503},
{"f_507:toy_2escm",(void*)f_507},
{"f_278:toy_2escm",(void*)f_278},
{"f_271:toy_2escm",(void*)f_271},
{"f_728:toy_2escm",(void*)f_728},
{"f_715:toy_2escm",(void*)f_715},
{"f_713:toy_2escm",(void*)f_713},
{"f_325:toy_2escm",(void*)f_325},
{"f_329:toy_2escm",(void*)f_329},
{"f_251:toy_2escm",(void*)f_251},
{"f_319:toy_2escm",(void*)f_319},
{"f_596:toy_2escm",(void*)f_596},
{"f_310:toy_2escm",(void*)f_310},
{"f_199:toy_2escm",(void*)f_199},
{"f_699:toy_2escm",(void*)f_699},
{"f_695:toy_2escm",(void*)f_695},
{"f_187:toy_2escm",(void*)f_187},
{"f_226:toy_2escm",(void*)f_226},
{"f_673:toy_2escm",(void*)f_673},
{"f_675:toy_2escm",(void*)f_675},
{"f_538:toy_2escm",(void*)f_538},
{"f_534:toy_2escm",(void*)f_534},
{"f_363:toy_2escm",(void*)f_363},
{"f_214:toy_2escm",(void*)f_214},
{"f_359:toy_2escm",(void*)f_359},
{"f_708:toy_2escm",(void*)f_708},
{"f_558:toy_2escm",(void*)f_558},
{"f_468:toy_2escm",(void*)f_468},
{"f_464:toy_2escm",(void*)f_464},
{"f_656:toy_2escm",(void*)f_656},
{"f_651:toy_2escm",(void*)f_651},
{"f_375:toy_2escm",(void*)f_375},
{"f_458:toy_2escm",(void*)f_458},
{"f_452:toy_2escm",(void*)f_452},
{"f_641:toy_2escm",(void*)f_641},
{"f_446:toy_2escm",(void*)f_446},
{"f_440:toy_2escm",(void*)f_440},
{"f_293:toy_2escm",(void*)f_293},
{"f_649:toy_2escm",(void*)f_649},
{"f_230:toy_2escm",(void*)f_230},
{"f_231:toy_2escm",(void*)f_231},
{"f_298:toy_2escm",(void*)f_298},
{"f_638:toy_2escm",(void*)f_638},
{"f_529:toy_2escm",(void*)f_529},
{"f_526:toy_2escm",(void*)f_526},
{"f_280:toy_2escm",(void*)f_280},
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
o|safe globals: (x symbol-table display-instructions setup-symbol-table compile compile-procedure compile-arguments compile-definition definition-value definition-variable assignment-value assignment-variable if-alternative if-consequent if-judge lambda-body lambda-parameters compile-lookup in? env-length frame-var-count extend-env stack-length stack-pop stack-push stack-display make-stack cdddr cadddr caddr cadr) 
o|replaced variables: 81 
o|removed binding forms: 37 
o|removed binding forms: 62 
o|simplifications: ((if . 1) (##core#call . 82)) 
o|  call simplifications:
o|    symbol?
o|    pair?
o|    list	8
o|    cdddr
o|    cadddr
o|    caddr	4
o|    cadr	5
o|    =	2
o|    null?	5
o|    length	4
o|    cons	9
o|    eq?	10
o|    -
o|    vector-set!
o|    +	4
o|    cdr	14
o|    car	11
o|contracted procedure: k184 
o|contracted procedure: k196 
o|contracted procedure: k193 
o|contracted procedure: k211 
o|contracted procedure: k208 
o|contracted procedure: k205 
o|contracted procedure: k223 
o|contracted procedure: k220 
o|contracted procedure: k236 
o|contracted procedure: k242 
o|contracted procedure: k248 
o|contracted procedure: k254 
o|contracted procedure: k257 
o|contracted procedure: k263 
o|contracted procedure: k267 
o|contracted procedure: k316 
o|contracted procedure: k334 
o|contracted procedure: k353 
o|contracted procedure: k340 
o|contracted procedure: k347 
o|contracted procedure: k350 
o|contracted procedure: k368 
o|contracted procedure: k379 
o|contracted procedure: k386 
o|contracted procedure: k389 
o|contracted procedure: k395 
o|contracted procedure: k437 
o|contracted procedure: k430 
o|contracted procedure: k472 
o|contracted procedure: k488 
o|contracted procedure: k485 
o|contracted procedure: k482 
o|contracted procedure: k479 
o|contracted procedure: k497 
o|contracted procedure: k494 
o|contracted procedure: k500 
o|contracted procedure: k512 
o|contracted procedure: k519 
o|contracted procedure: k522 
o|contracted procedure: k551 
o|contracted procedure: k565 
o|contracted procedure: k568 
o|contracted procedure: k573 
o|contracted procedure: k583 
o|contracted procedure: k580 
o|contracted procedure: k588 
o|contracted procedure: k604 
o|contracted procedure: k601 
o|contracted procedure: k598 
o|contracted procedure: k621 
o|contracted procedure: k634 
o|contracted procedure: k645 
o|contracted procedure: k664 
o|contracted procedure: k661 
o|contracted procedure: k669 
o|contracted procedure: k683 
o|contracted procedure: k686 
o|contracted procedure: k692 
o|contracted procedure: k704 
o|contracted procedure: k720 
o|contracted procedure: k723 
o|simplifications: ((let . 8)) 
o|removed binding forms: 61 
o|customizable procedures: (compile-arguments-iter90 compile-lookup-iter56 in?-iter44) 
o|calls to known targets: 6 
o|identified direct recursive calls: f_329 1 
o|fast box initializations: 3 
*/
/* end of file */
