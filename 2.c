/* Generated from 2.2 by the CHICKEN compiler
   http://www.call-cc.org
   2013-10-05 18:25
   Version 4.8.0.4 (stability/4.8.0) (rev 578619b)
   macosx-unix-clang-x86-64 [ 64bit manyargs dload ptables ]
   compiled 2013-07-15 on aeryn.xorinia.dim (Darwin)
   command line: 2.2
   used units: library eval
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[22];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,20),40,108,105,115,116,45,114,101,102,32,105,116,101,109,115,49,32,110,50,41,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,27),40,108,101,110,103,116,104,45,105,116,101,114,32,105,116,101,109,115,56,32,99,111,117,110,116,57,41,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,15),40,108,101,110,103,116,104,32,105,116,101,109,115,54,41,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,24),40,97,112,112,101,110,100,32,108,105,115,116,49,49,50,32,108,105,115,116,50,49,51,41};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,18),40,108,97,115,116,45,112,97,105,114,32,108,105,115,116,49,53,41,0,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,30),40,114,101,118,101,114,115,101,45,105,116,101,114,32,108,105,115,116,49,57,32,114,101,115,117,108,116,50,48,41,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,16),40,114,101,118,101,114,115,101,32,108,105,115,116,49,55,41};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,23),40,102,111,114,45,101,97,99,104,45,108,111,111,112,50,55,32,103,51,52,51,56,41,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,25),40,102,111,114,45,101,97,99,104,32,112,114,111,99,50,51,32,105,116,101,109,115,50,52,41,0,0,0,0,0,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,22),40,99,111,117,110,116,45,108,101,97,118,101,115,32,105,116,101,109,115,52,52,41,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,36),40,100,101,101,112,45,114,101,118,101,114,115,101,45,105,116,101,114,32,105,116,101,109,115,53,49,32,114,101,115,117,108,116,53,50,41,0,0,0,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,22),40,100,101,101,112,45,114,101,118,101,114,115,101,32,105,116,101,109,115,52,57,41,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,16),40,102,114,105,110,103,101,32,105,116,101,109,115,53,56,41};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,31),40,102,105,108,116,101,114,32,112,114,101,100,105,99,97,116,101,54,52,32,115,101,113,117,101,110,99,101,54,53,41,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,38),40,97,99,99,117,109,117,108,97,116,101,32,111,112,55,48,32,105,110,105,116,105,97,108,55,49,32,115,101,113,117,101,110,99,101,55,50,41,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,35),40,102,95,53,55,57,32,116,104,105,115,45,99,111,101,102,102,55,54,32,104,105,103,104,101,114,45,116,101,114,109,115,55,55,41,0,0,0,0,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,40),40,104,111,114,110,101,114,45,101,118,97,108,32,120,55,52,32,99,111,101,102,102,105,99,105,101,110,116,45,115,101,113,117,101,110,99,101,55,53,41};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,49,49,52,32,103,49,50,54,49,51,50,41,0,0,0};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,20),40,109,97,112,45,108,111,111,112,56,56,32,103,49,48,48,49,48,54,41,0,0,0,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,33),40,97,99,99,117,109,117,108,97,116,101,45,110,32,111,112,55,57,32,105,110,105,116,56,48,32,115,101,113,115,56,49,41,0,0,0,0,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,29),40,109,97,112,45,108,111,111,112,49,54,57,32,103,49,56,49,49,56,57,32,103,49,56,50,49,57,48,41,0,0,0};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,23),40,100,111,116,45,112,114,111,100,117,99,116,32,118,49,54,53,32,119,49,54,54,41,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,21),40,109,97,112,45,108,111,111,112,49,52,48,32,103,49,53,50,49,53,56,41,0,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_274)
static void C_ccall f_274(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_622)
static void C_fcall f_622(C_word t0,C_word t1) C_noret;
C_noret_decl(f_709)
static void C_fcall f_709(C_word t0,C_word t1) C_noret;
C_noret_decl(f_552)
static void C_ccall f_552(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_467)
static void C_ccall f_467(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_295)
static void C_ccall f_295(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_647)
static void C_fcall f_647(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_645)
static void C_ccall f_645(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_290)
static void C_ccall f_290(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_498)
static void C_ccall f_498(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_696)
static void C_ccall f_696(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_362)
static void C_fcall f_362(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_698)
static void C_fcall f_698(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_765)
static void C_fcall f_765(C_word t0,C_word t1) C_noret;
C_noret_decl(f_489)
static void C_ccall f_489(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_752)
static void C_ccall f_752(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_754)
static void C_fcall f_754(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_516)
static void C_ccall f_516(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_385)
static void C_ccall f_385(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_658)
static void C_fcall f_658(C_word t0,C_word t1) C_noret;
C_noret_decl(f_603)
static void C_ccall f_603(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_600)
static void C_ccall f_600(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_501)
static void C_ccall f_501(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_609)
static void C_ccall f_609(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_224)
static void C_ccall f_224(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_226)
static void C_ccall f_226(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_228)
static void C_ccall f_228(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_422)
static void C_fcall f_422(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_573)
static void C_ccall f_573(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_579)
static void C_ccall f_579(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_253)
static void C_fcall f_253(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_320)
static void C_fcall f_320(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_568)
static void C_ccall f_568(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_353)
static void C_ccall f_353(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_371)
static void C_ccall f_371(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_249)
static void C_ccall f_249(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_447)
static void C_ccall f_447(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_344)
static void C_ccall f_344(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_743)
static void C_ccall f_743(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_740)
static void C_ccall f_740(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_746)
static void C_ccall f_746(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_587)
static void C_ccall f_587(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_684)
static void C_ccall f_684(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_682)
static void C_ccall f_682(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_680)
static void C_ccall f_680(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_418)
static void C_ccall f_418(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_316)
static void C_ccall f_316(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_538)
static void C_ccall f_538(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_611)
static void C_fcall f_611(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_404)
static void C_ccall f_404(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_407)
static void C_ccall f_407(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_529)
static void C_ccall f_529(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_622)
static void C_fcall trf_622(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_622(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_622(t0,t1);}

C_noret_decl(trf_709)
static void C_fcall trf_709(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_709(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_709(t0,t1);}

C_noret_decl(trf_647)
static void C_fcall trf_647(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_647(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_647(t0,t1,t2);}

C_noret_decl(trf_362)
static void C_fcall trf_362(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_362(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_362(t0,t1,t2);}

C_noret_decl(trf_698)
static void C_fcall trf_698(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_698(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_698(t0,t1,t2,t3);}

C_noret_decl(trf_765)
static void C_fcall trf_765(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_765(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_765(t0,t1);}

C_noret_decl(trf_754)
static void C_fcall trf_754(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_754(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_754(t0,t1,t2);}

C_noret_decl(trf_658)
static void C_fcall trf_658(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_658(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_658(t0,t1);}

C_noret_decl(trf_422)
static void C_fcall trf_422(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_422(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_422(t0,t1,t2,t3);}

C_noret_decl(trf_253)
static void C_fcall trf_253(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_253(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_253(t0,t1,t2,t3);}

C_noret_decl(trf_320)
static void C_fcall trf_320(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_320(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_320(t0,t1,t2,t3);}

C_noret_decl(trf_611)
static void C_fcall trf_611(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_611(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_611(t0,t1,t2);}

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

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

/* append in k225 in k223 */
static void C_ccall f_274(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_274,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}
else{
t4=C_i_car(t2);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_290,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
t6=C_i_cdr(t2);
C_trace("2.2:30: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(4,*((C_word*)lf[2]+1),t5,t6,t3);}}

/* k621 in map-loop114 in k599 in accumulate-n in k225 in k223 */
static void C_fcall f_622(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_611(t4,((C_word*)t0)[6],t3);}

/* k708 in map-loop169 in dot-product in k681 in k679 in k225 in k223 */
static void C_fcall f_709(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=C_slot(((C_word*)t0)[5],C_fix(1));
t5=((C_word*)((C_word*)t0)[6])[1];
f_698(t5,((C_word*)t0)[7],t3,t4);}

/* accumulate in k225 in k223 */
static void C_ccall f_552(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[5],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_552,5,t0,t1,t2,t3,t4);}
if(C_truep(C_i_nullp(t4))){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,t3);}
else{
t5=C_i_car(t4);
t6=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_568,a[2]=t2,a[3]=t1,a[4]=t5,tmp=(C_word)a,a+=5,tmp);
t7=C_i_cdr(t4);
C_trace("2.2:154: accumulate");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(5,*((C_word*)lf[11]+1),t6,t2,t3,t7);}}

/* fringe in k225 in k223 */
static void C_ccall f_467(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_467,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_END_OF_LIST);}
else{
t3=C_i_car(t2);
if(C_truep(C_i_pairp(t3))){
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_498,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t5=C_i_car(t2);
C_trace("2.2:126: fringe");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[8]+1)))(3,*((C_word*)lf[8]+1),t4,t5);}
else{
t4=C_i_car(t2);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_489,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
t6=C_i_cdr(t2);
C_trace("2.2:124: fringe");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[8]+1)))(3,*((C_word*)lf[8]+1),t5,t6);}}}

/* last-pair in k225 in k223 */
static void C_ccall f_295(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_295,3,t0,t1,t2);}
t3=C_i_cdr(t2);
if(C_truep(C_i_nullp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_car(t2));}
else{
t4=C_i_cdr(t2);
C_trace("2.2:36: last-pair");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[3]+1)))(3,*((C_word*)lf[3]+1),t1,t4);}}

/* map-loop88 in accumulate-n in k225 in k223 */
static void C_fcall f_647(C_word t0,C_word t1,C_word t2){
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
C_save_and_reclaim((void*)trf_647,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_slot(t2,C_fix(0));
t4=C_i_car(t3);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_658,a[2]=((C_word*)t0)[2],a[3]=t5,a[4]=t2,a[5]=((C_word*)t0)[3],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t7=t6;
f_658(t7,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t5));}
else{
t7=C_mutate(((C_word *)((C_word*)t0)[4])+1,t5);
t8=t6;
f_658(t8,t7);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* k643 in accumulate-n in k225 in k223 */
static void C_ccall f_645(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:180: accumulate");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(5,*((C_word*)lf[11]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* k289 in append in k225 in k223 */
static void C_ccall f_290(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_290,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* k497 in fringe in k225 in k223 */
static void C_ccall f_498(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_498,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_501,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_i_cdr(((C_word*)t0)[3]);
C_trace("2.2:127: fringe");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[8]+1)))(3,*((C_word*)lf[8]+1),t2,t3);}

/* k694 in dot-product in k681 in k679 in k225 in k223 */
static void C_ccall f_696(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:192: accumulate");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(5,*((C_word*)lf[11]+1),((C_word*)t0)[2],*((C_word*)lf[17]+1),C_fix(0),t1);}

/* for-each-loop27 in k352 in for-each in k225 in k223 */
static void C_fcall f_362(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_362,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_371,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=C_slot(t2,C_fix(0));
C_trace("2.2:79: g28");
t5=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_UNDEFINED);}}

/* map-loop169 in dot-product in k681 in k679 in k225 in k223 */
static void C_fcall f_698(C_word t0,C_word t1,C_word t2,C_word t3){
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
C_word ab[15],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_698,NULL,4,t0,t1,t2,t3);}
t4=C_i_pairp(t2);
t5=(C_truep(t4)?C_i_pairp(t3):C_SCHEME_FALSE);
if(C_truep(t5)){
t6=C_slot(t2,C_fix(0));
t7=C_slot(t3,C_fix(0));
t8=C_a_i_times(&a,2,t6,t7);
t9=C_a_i_cons(&a,2,t8,C_SCHEME_END_OF_LIST);
t10=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_709,a[2]=((C_word*)t0)[2],a[3]=t9,a[4]=t2,a[5]=t3,a[6]=((C_word*)t0)[3],a[7]=t1,tmp=(C_word)a,a+=8,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t11=t10;
f_709(t11,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t9));}
else{
t11=C_mutate(((C_word *)((C_word*)t0)[4])+1,t9);
t12=t10;
f_709(t12,t11);}}
else{
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,((C_word*)((C_word*)t0)[4])[1]);}}

/* k764 in map-loop140 in k225 in k223 */
static void C_fcall f_765(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_754(t4,((C_word*)t0)[6],t3);}

/* k488 in fringe in k225 in k223 */
static void C_ccall f_489(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_489,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* k750 in k225 in k223 */
static void C_ccall f_752(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:186: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(3,*((C_word*)lf[19]+1),((C_word*)t0)[2],t1);}

/* map-loop140 in k225 in k223 */
static void C_fcall f_754(C_word t0,C_word t1,C_word t2){
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
C_save_and_reclaim((void*)trf_754,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_slot(t2,C_fix(0));
t4=C_i_cdr(t3);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_765,a[2]=((C_word*)t0)[2],a[3]=t5,a[4]=t2,a[5]=((C_word*)t0)[3],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t7=t6;
f_765(t7,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t5));}
else{
t7=C_mutate(((C_word *)((C_word*)t0)[4])+1,t5);
t8=t6;
f_765(t8,t7);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* filter in k225 in k223 */
static void C_ccall f_516(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_516,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,*((C_word*)lf[9]+1));}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_529,a[2]=t3,a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t3);
C_trace("2.2:143: predicate");
t6=t2;
((C_proc3)C_fast_retrieve_proc(t6))(3,t6,t4,t5);}}

/* count-leaves in k225 in k223 */
static void C_ccall f_385(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_385,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(0));}
else{
if(C_truep(C_i_pairp(t2))){
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_404,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_car(t2);
C_trace("2.2:91: count-leaves");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(3,*((C_word*)lf[6]+1),t3,t4);}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_fix(1));}}}

/* k657 in map-loop88 in accumulate-n in k225 in k223 */
static void C_fcall f_658(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
t2=C_mutate(((C_word *)((C_word*)t0)[2])+1,((C_word*)t0)[3]);
t3=C_slot(((C_word*)t0)[4],C_fix(1));
t4=((C_word*)((C_word*)t0)[5])[1];
f_647(t4,((C_word*)t0)[6],t3);}

/* k602 in k599 in accumulate-n in k225 in k223 */
static void C_ccall f_603(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_603,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* k599 in accumulate-n in k225 in k223 */
static void C_ccall f_600(C_word c,C_word t0,C_word t1){
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
C_word ab[21],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_600,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_603,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_SCHEME_END_OF_LIST;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_SCHEME_FALSE;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=((C_word*)t0)[3];
t8=C_i_check_list_2(t7,lf[15]);
t9=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_609,a[2]=t2,a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],tmp=(C_word)a,a+=5,tmp);
t10=C_SCHEME_UNDEFINED;
t11=(*a=C_VECTOR_TYPE|1,a[1]=t10,tmp=(C_word)a,a+=2,tmp);
t12=C_set_block_item(t11,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_611,a[2]=t6,a[3]=t11,a[4]=t4,a[5]=((C_word)li17),tmp=(C_word)a,a+=6,tmp));
t13=((C_word*)t11)[1];
f_611(t13,t9,t7);}

/* k500 in k497 in fringe in k225 in k223 */
static void C_ccall f_501(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:126: append");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[2]+1)))(4,*((C_word*)lf[2]+1),((C_word*)t0)[2],((C_word*)t0)[3],t1);}

/* k607 in k599 in accumulate-n in k225 in k223 */
static void C_ccall f_609(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:181: accumulate-n");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(5,*((C_word*)lf[13]+1),((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* k223 */
static void C_ccall f_224(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_224,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_226,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k225 in k223 */
static void C_ccall f_226(C_word c,C_word t0,C_word t1){
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
C_word ab[57],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_226,2,t0,t1);}
t2=C_mutate((C_word*)lf[0]+1 /* (set! list-ref ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_228,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[1]+1 /* (set! length ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_249,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[2]+1 /* (set! append ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_274,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[3]+1 /* (set! last-pair ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_295,a[2]=((C_word)li4),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[4]+1 /* (set! reverse ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_316,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[5]+1 /* (set! for-each ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_344,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[6]+1 /* (set! count-leaves ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_385,a[2]=((C_word)li9),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[7]+1 /* (set! deep-reverse ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_418,a[2]=((C_word)li11),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[8]+1 /* (set! fringe ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_467,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t11=C_set_block_item(lf[9] /* nil */,0,C_SCHEME_END_OF_LIST);
t12=C_mutate((C_word*)lf[10]+1 /* (set! filter ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_516,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[11]+1 /* (set! accumulate ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_552,a[2]=((C_word)li14),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[12]+1 /* (set! horner-eval ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_573,a[2]=((C_word)li16),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[13]+1 /* (set! accumulate-n ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_587,a[2]=((C_word)li19),tmp=(C_word)a,a+=3,tmp));
t16=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_680,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t17=C_SCHEME_END_OF_LIST;
t18=(*a=C_VECTOR_TYPE|1,a[1]=t17,tmp=(C_word)a,a+=2,tmp);
t19=C_SCHEME_FALSE;
t20=(*a=C_VECTOR_TYPE|1,a[1]=t19,tmp=(C_word)a,a+=2,tmp);
t21=lf[21];
t22=C_i_check_list_2(t21,lf[15]);
t23=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_752,a[2]=t16,tmp=(C_word)a,a+=3,tmp);
t24=C_SCHEME_UNDEFINED;
t25=(*a=C_VECTOR_TYPE|1,a[1]=t24,tmp=(C_word)a,a+=2,tmp);
t26=C_set_block_item(t25,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_754,a[2]=t20,a[3]=t25,a[4]=t18,a[5]=((C_word)li22),tmp=(C_word)a,a+=6,tmp));
t27=((C_word*)t25)[1];
f_754(t27,t23,t21);}

/* list-ref in k225 in k223 */
static void C_ccall f_228(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_228,4,t0,t1,t2,t3);}
if(C_truep(C_i_nequalp(t3,C_fix(0)))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_i_car(t2));}
else{
t4=C_i_cdr(t2);
t5=C_a_i_minus(&a,2,t3,C_fix(1));
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_i_list_ref(t4,t5));}}

/* deep-reverse-iter in deep-reverse in k225 in k223 */
static void C_fcall f_422(C_word t0,C_word t1,C_word t2,C_word t3){
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
a=C_alloc(6);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_422,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}
else{
if(C_truep(C_i_pairp(t2))){
t4=C_i_cdr(t2);
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_447,a[2]=t3,a[3]=((C_word*)t0)[2],a[4]=t1,a[5]=t4,tmp=(C_word)a,a+=6,tmp);
t6=C_i_car(t2);
C_trace("2.2:108: reverse");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[4]+1)))(3,*((C_word*)lf[4]+1),t5,t6);}
else{
t4=C_i_cdr(t2);
t5=C_i_car(t2);
t6=C_a_i_cons(&a,2,t5,t3);
C_trace("2.2:109: deep-reverse-iter");
t11=t1;
t12=t4;
t13=t6;
t1=t11;
t2=t12;
t3=t13;
goto loop;}}}

/* horner-eval in k225 in k223 */
static void C_ccall f_573(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[4],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_573,4,t0,t1,t2,t3);}
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_579,a[2]=t2,a[3]=((C_word)li15),tmp=(C_word)a,a+=4,tmp);
C_trace("2.2:165: accumulate");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[11]+1)))(5,*((C_word*)lf[11]+1),t1,t4,C_fix(0),t3);}

/* f_579 in horner-eval in k225 in k223 */
static void C_ccall f_579(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_579,4,t0,t1,t2,t3);}
t4=C_a_i_times(&a,2,((C_word*)t0)[2],t3);
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_a_i_plus(&a,2,t2,t4));}

/* length-iter in length in k225 in k223 */
static void C_fcall f_253(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word *a;
loop:
a=C_alloc(4);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_253,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}
else{
t4=C_i_cdr(t2);
t5=C_a_i_plus(&a,2,t3,C_fix(1));
C_trace("2.2:20: length-iter");
t7=t1;
t8=t4;
t9=t5;
t1=t7;
t2=t8;
t3=t9;
goto loop;}}

/* reverse-iter in reverse in k225 in k223 */
static void C_fcall f_320(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
loop:
a=C_alloc(3);
C_check_for_interrupt;
if(!C_stack_probe(a)){
C_save_and_reclaim((void*)trf_320,NULL,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t2))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,t3);}
else{
t4=C_i_cdr(t2);
t5=C_i_car(t2);
t6=C_a_i_cons(&a,2,t5,t3);
C_trace("2.2:45: reverse-iter");
t8=t1;
t9=t4;
t10=t6;
t1=t8;
t2=t9;
t3=t10;
goto loop;}}

/* k567 in accumulate in k225 in k223 */
static void C_ccall f_568(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:153: op");
t2=((C_word*)t0)[2];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[3],((C_word*)t0)[4],t1);}

/* k352 in for-each in k225 in k223 */
static void C_ccall f_353(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_353,2,t0,t1);}
t2=((C_word*)t0)[2];
t3=C_i_cdr(((C_word*)t0)[3]);
t4=C_i_check_list_2(t3,lf[5]);
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_362,a[2]=t6,a[3]=t2,a[4]=((C_word)li7),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_362(t8,((C_word*)t0)[4],t3);}

/* k370 in for-each-loop27 in k352 in for-each in k225 in k223 */
static void C_ccall f_371(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
t2=C_slot(((C_word*)t0)[2],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_362(t3,((C_word*)t0)[4],t2);}

/* length in k225 in k223 */
static void C_ccall f_249(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_249,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_253,a[2]=t4,a[3]=((C_word)li1),tmp=(C_word)a,a+=4,tmp));
C_trace("2.2:23: length-iter");
t6=((C_word*)t4)[1];
f_253(t6,t1,t2,C_fix(0));}

/* k446 in deep-reverse-iter in deep-reverse in k225 in k223 */
static void C_ccall f_447(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_447,2,t0,t1);}
t2=C_a_i_cons(&a,2,t1,((C_word*)t0)[2]);
C_trace("2.2:107: deep-reverse-iter");
t3=((C_word*)((C_word*)t0)[3])[1];
f_422(t3,((C_word*)t0)[4],((C_word*)t0)[5],t2);}

/* for-each in k225 in k223 */
static void C_ccall f_344(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_344,4,t0,t1,t2,t3);}
if(C_truep(C_i_nullp(t3))){
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_TRUE);}
else{
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_353,a[2]=t2,a[3]=t3,a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t5=C_i_car(t3);
C_trace("2.2:78: proc");
t6=t2;
((C_proc3)C_fast_retrieve_proc(t6))(3,t6,t4,t5);}}

/* k741 in k681 in k679 in k225 in k223 */
static void C_ccall f_743(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* k739 in k681 in k679 in k225 in k223 */
static void C_ccall f_740(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* k745 in k679 in k225 in k223 */
static void C_ccall f_746(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
C_trace("2.2:187: display");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[19]+1)))(3,*((C_word*)lf[19]+1),((C_word*)t0)[2],t1);}

/* accumulate-n in k225 in k223 */
static void C_ccall f_587(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
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
C_word ab[23],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_587,5,t0,t1,t2,t3,t4);}
t5=C_i_car(t4);
if(C_truep(C_i_nullp(t5))){
t6=C_fast_retrieve(lf[14]);
t7=t1;
((C_proc2)(void*)(*((C_word*)t7+1)))(2,t7,C_SCHEME_END_OF_LIST);}
else{
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_600,a[2]=t1,a[3]=t4,a[4]=t2,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
t7=C_SCHEME_END_OF_LIST;
t8=(*a=C_VECTOR_TYPE|1,a[1]=t7,tmp=(C_word)a,a+=2,tmp);
t9=C_SCHEME_FALSE;
t10=(*a=C_VECTOR_TYPE|1,a[1]=t9,tmp=(C_word)a,a+=2,tmp);
t11=t4;
t12=C_i_check_list_2(t11,lf[15]);
t13=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_645,a[2]=t6,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
t14=C_SCHEME_UNDEFINED;
t15=(*a=C_VECTOR_TYPE|1,a[1]=t14,tmp=(C_word)a,a+=2,tmp);
t16=C_set_block_item(t15,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_647,a[2]=t10,a[3]=t15,a[4]=t8,a[5]=((C_word)li18),tmp=(C_word)a,a+=6,tmp));
t17=((C_word*)t15)[1];
f_647(t17,t13,t11);}}

/* dot-product in k681 in k679 in k225 in k223 */
static void C_ccall f_684(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
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
C_word t14;
C_word ab[15],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_684,4,t0,t1,t2,t3);}
t4=C_SCHEME_END_OF_LIST;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_SCHEME_FALSE;
t7=(*a=C_VECTOR_TYPE|1,a[1]=t6,tmp=(C_word)a,a+=2,tmp);
t8=C_i_check_list_2(t2,lf[15]);
t9=C_i_check_list_2(t3,lf[15]);
t10=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_696,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t11=C_SCHEME_UNDEFINED;
t12=(*a=C_VECTOR_TYPE|1,a[1]=t11,tmp=(C_word)a,a+=2,tmp);
t13=C_set_block_item(t12,0,(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_698,a[2]=t7,a[3]=t12,a[4]=t5,a[5]=((C_word)li20),tmp=(C_word)a,a+=6,tmp));
t14=((C_word*)t12)[1];
f_698(t14,t10,t2,t3);}

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
if(!C_demand_2(266)){
C_save(t1);
C_rereclaim2(266*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,22);
lf[0]=C_h_intern(&lf[0],8,"list-ref");
lf[1]=C_h_intern(&lf[1],6,"length");
lf[2]=C_h_intern(&lf[2],6,"append");
lf[3]=C_h_intern(&lf[3],9,"last-pair");
lf[4]=C_h_intern(&lf[4],7,"reverse");
lf[5]=C_h_intern(&lf[5],8,"for-each");
lf[6]=C_h_intern(&lf[6],12,"count-leaves");
lf[7]=C_h_intern(&lf[7],12,"deep-reverse");
lf[8]=C_h_intern(&lf[8],6,"fringe");
lf[9]=C_h_intern(&lf[9],3,"nil");
lf[10]=C_h_intern(&lf[10],6,"filter");
lf[11]=C_h_intern(&lf[11],10,"accumulate");
lf[12]=C_h_intern(&lf[12],11,"horner-eval");
lf[13]=C_h_intern(&lf[13],12,"accumulate-n");
lf[14]=C_h_intern(&lf[14],3,"\357\277\274");
lf[15]=C_h_intern(&lf[15],3,"map");
lf[16]=C_h_intern(&lf[16],11,"dot-product");
lf[17]=C_h_intern(&lf[17],1,"+");
lf[18]=C_h_intern(&lf[18],25,"\003sysimplicit-exit-handler");
lf[19]=C_h_intern(&lf[19],7,"display");
lf[20]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\001\376\003\000\000\002\376\377\001\000\000\000\002\376\003\000\000\002\376\377\001\000\000\000\003\376\377\016\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\004\376\003\000\000\002\376\377\001\000\000\000\005\376\003\000\000\002\376\377"
"\001\000\000\000\006\376\377\016\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\007\376\003\000\000\002\376\377\001\000\000\000\010\376\003\000\000\002\376\377\001\000\000\000\011\376\377\016\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\012\376\003\000\000\002\376\377\001\000\000\000"
"\013\376\003\000\000\002\376\377\001\000\000\000\014\376\377\016\376\377\016");
lf[21]=C_decode_literal(C_heaptop,"\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\001\376\003\000\000\002\376\377\001\000\000\000\002\376\377\016\376\003\000\000\002\376\003\000\000\002\376\377\001\000\000\000\003\376\003\000\000\002\376\377\001\000\000\000\004\376\377\016\376\377\016");
C_register_lf2(lf,22,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_224,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k681 in k679 in k225 in k223 */
static void C_ccall f_682(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_682,2,t0,t1);}
t2=C_mutate((C_word*)lf[16]+1 /* (set! dot-product ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_684,a[2]=((C_word)li21),tmp=(C_word)a,a+=3,tmp));
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_740,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_743,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[18]))(2,*((C_word*)lf[18]+1),t4);}

/* k679 in k225 in k223 */
static void C_ccall f_680(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_680,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_682,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_746,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace("2.2:187: accumulate-n");
((C_proc5)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(5,*((C_word*)lf[13]+1),t3,*((C_word*)lf[17]+1),C_fix(0),lf[20]);}

/* deep-reverse in k225 in k223 */
static void C_ccall f_418(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_418,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_422,a[2]=t4,a[3]=((C_word)li10),tmp=(C_word)a,a+=4,tmp));
C_trace("2.2:112: deep-reverse-iter");
t6=((C_word*)t4)[1];
f_422(t6,t1,t2,C_SCHEME_END_OF_LIST);}

/* reverse in k225 in k223 */
static void C_ccall f_316(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_316,3,t0,t1,t2);}
t3=C_SCHEME_UNDEFINED;
t4=(*a=C_VECTOR_TYPE|1,a[1]=t3,tmp=(C_word)a,a+=2,tmp);
t5=C_set_block_item(t4,0,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_320,a[2]=t4,a[3]=((C_word)li5),tmp=(C_word)a,a+=4,tmp));
C_trace("2.2:48: reverse-iter");
t6=((C_word*)t4)[1];
f_320(t6,t1,t2,C_SCHEME_END_OF_LIST);}

/* k537 in k527 in filter in k225 in k223 */
static void C_ccall f_538(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_538,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_cons(&a,2,((C_word*)t0)[3],t1));}

/* map-loop114 in k599 in accumulate-n in k225 in k223 */
static void C_fcall f_611(C_word t0,C_word t1,C_word t2){
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
C_save_and_reclaim((void*)trf_611,NULL,3,t0,t1,t2);}
if(C_truep(C_i_pairp(t2))){
t3=C_slot(t2,C_fix(0));
t4=C_i_cdr(t3);
t5=C_a_i_cons(&a,2,t4,C_SCHEME_END_OF_LIST);
t6=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_622,a[2]=((C_word*)t0)[2],a[3]=t5,a[4]=t2,a[5]=((C_word*)t0)[3],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
if(C_truep(((C_word*)((C_word*)t0)[2])[1])){
t7=t6;
f_622(t7,C_i_setslot(((C_word*)((C_word*)t0)[2])[1],C_fix(1),t5));}
else{
t7=C_mutate(((C_word *)((C_word*)t0)[4])+1,t5);
t8=t6;
f_622(t8,t7);}}
else{
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,((C_word*)((C_word*)t0)[4])[1]);}}

/* k403 in count-leaves in k225 in k223 */
static void C_ccall f_404(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_404,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_407,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_i_cdr(((C_word*)t0)[3]);
C_trace("2.2:92: count-leaves");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[6]+1)))(3,*((C_word*)lf[6]+1),t2,t3);}

/* k406 in k403 in count-leaves in k225 in k223 */
static void C_ccall f_407(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_407,2,t0,t1);}
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_a_i_plus(&a,2,((C_word*)t0)[3],t1));}

/* k527 in filter in k225 in k223 */
static void C_ccall f_529(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_529,2,t0,t1);}
if(C_truep(t1)){
t2=C_i_car(((C_word*)t0)[2]);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_538,a[2]=((C_word*)t0)[3],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_i_cdr(((C_word*)t0)[2]);
C_trace("2.2:145: filter");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),t3,((C_word*)t0)[4],t4);}
else{
t2=C_i_cdr(((C_word*)t0)[2]);
C_trace("2.2:146: filter");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),((C_word*)t0)[3],((C_word*)t0)[4],t2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[54] = {
{"f_274:_32_2e2",(void*)f_274},
{"f_622:_32_2e2",(void*)f_622},
{"f_709:_32_2e2",(void*)f_709},
{"f_552:_32_2e2",(void*)f_552},
{"f_467:_32_2e2",(void*)f_467},
{"f_295:_32_2e2",(void*)f_295},
{"f_647:_32_2e2",(void*)f_647},
{"f_645:_32_2e2",(void*)f_645},
{"f_290:_32_2e2",(void*)f_290},
{"f_498:_32_2e2",(void*)f_498},
{"f_696:_32_2e2",(void*)f_696},
{"f_362:_32_2e2",(void*)f_362},
{"f_698:_32_2e2",(void*)f_698},
{"f_765:_32_2e2",(void*)f_765},
{"f_489:_32_2e2",(void*)f_489},
{"f_752:_32_2e2",(void*)f_752},
{"f_754:_32_2e2",(void*)f_754},
{"f_516:_32_2e2",(void*)f_516},
{"f_385:_32_2e2",(void*)f_385},
{"f_658:_32_2e2",(void*)f_658},
{"f_603:_32_2e2",(void*)f_603},
{"f_600:_32_2e2",(void*)f_600},
{"f_501:_32_2e2",(void*)f_501},
{"f_609:_32_2e2",(void*)f_609},
{"f_224:_32_2e2",(void*)f_224},
{"f_226:_32_2e2",(void*)f_226},
{"f_228:_32_2e2",(void*)f_228},
{"f_422:_32_2e2",(void*)f_422},
{"f_573:_32_2e2",(void*)f_573},
{"f_579:_32_2e2",(void*)f_579},
{"f_253:_32_2e2",(void*)f_253},
{"f_320:_32_2e2",(void*)f_320},
{"f_568:_32_2e2",(void*)f_568},
{"f_353:_32_2e2",(void*)f_353},
{"f_371:_32_2e2",(void*)f_371},
{"f_249:_32_2e2",(void*)f_249},
{"f_447:_32_2e2",(void*)f_447},
{"f_344:_32_2e2",(void*)f_344},
{"f_743:_32_2e2",(void*)f_743},
{"f_740:_32_2e2",(void*)f_740},
{"f_746:_32_2e2",(void*)f_746},
{"f_587:_32_2e2",(void*)f_587},
{"f_684:_32_2e2",(void*)f_684},
{"toplevel:_32_2e2",(void*)C_toplevel},
{"f_682:_32_2e2",(void*)f_682},
{"f_680:_32_2e2",(void*)f_680},
{"f_418:_32_2e2",(void*)f_418},
{"f_316:_32_2e2",(void*)f_316},
{"f_538:_32_2e2",(void*)f_538},
{"f_611:_32_2e2",(void*)f_611},
{"f_404:_32_2e2",(void*)f_404},
{"f_407:_32_2e2",(void*)f_407},
{"f_529:_32_2e2",(void*)f_529},
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
S|  map		4
S|  for-each		1
o|eliminated procedure checks: 16 
o|dropping redundant toplevel assignment: length 
o|safe globals: (accumulate-n horner-eval accumulate filter nil fringe deep-reverse count-leaves for-each reverse last-pair append length list-ref) 
o|Removed `not' forms: 2 
o|replaced variables: 85 
o|removed binding forms: 28 
o|removed binding forms: 66 
o|simplifications: ((if . 1) (##core#call . 94)) 
o|  call simplifications:
o|    ##sys#setslot	4
o|    *	2
o|    ##sys#check-list	6
o|    pair?	9
o|    ##sys#slot	12
o|    cons	11
o|    null?	11
o|    +	3
o|    =
o|    cdr	17
o|    -
o|    list-ref
o|    car	16
o|contracted procedure: k233 
o|contracted procedure: k243 
o|contracted procedure: k246 
o|contracted procedure: k258 
o|contracted procedure: k265 
o|contracted procedure: k268 
o|contracted procedure: k279 
o|contracted procedure: k286 
o|contracted procedure: k292 
o|contracted procedure: k313 
o|contracted procedure: k300 
o|contracted procedure: k310 
o|contracted procedure: k325 
o|contracted procedure: k332 
o|contracted procedure: k338 
o|contracted procedure: k335 
o|contracted procedure: k349 
o|contracted procedure: k354 
o|contracted procedure: k356 
o|contracted procedure: k367 
o|contracted procedure: k376 
o|contracted procedure: k379 
o|contracted procedure: k382 
o|contracted procedure: k390 
o|contracted procedure: k396 
o|contracted procedure: k409 
o|contracted procedure: k412 
o|contracted procedure: k427 
o|contracted procedure: k433 
o|contracted procedure: k440 
o|contracted procedure: k443 
o|contracted procedure: k449 
o|contracted procedure: k455 
o|contracted procedure: k461 
o|contracted procedure: k458 
o|contracted procedure: k472 
o|contracted procedure: k512 
o|contracted procedure: k478 
o|contracted procedure: k503 
o|contracted procedure: k506 
o|contracted procedure: k485 
o|contracted procedure: k491 
o|contracted procedure: k521 
o|contracted procedure: k534 
o|contracted procedure: k540 
o|contracted procedure: k546 
o|contracted procedure: k549 
o|contracted procedure: k557 
o|contracted procedure: k564 
o|contracted procedure: k570 
o|contracted procedure: k584 
o|contracted procedure: k677 
o|contracted procedure: k592 
o|contracted procedure: k605 
o|contracted procedure: k616 
o|contracted procedure: k638 
o|contracted procedure: k635 
o|contracted procedure: k619 
o|contracted procedure: k628 
o|contracted procedure: k641 
o|contracted procedure: k652 
o|contracted procedure: k674 
o|contracted procedure: k671 
o|contracted procedure: k655 
o|contracted procedure: k664 
o|contracted procedure: k690 
o|contracted procedure: k692 
o|contracted procedure: k733 
o|contracted procedure: k703 
o|contracted procedure: k728 
o|contracted procedure: k731 
o|contracted procedure: k725 
o|contracted procedure: k706 
o|contracted procedure: k715 
o|contracted procedure: k718 
o|contracted procedure: k748 
o|contracted procedure: k759 
o|contracted procedure: k781 
o|contracted procedure: k778 
o|contracted procedure: k762 
o|contracted procedure: k771 
o|simplifications: ((let . 19)) 
o|removed binding forms: 81 
o|customizable procedures: (k764 map-loop140157 k708 map-loop169188 k657 map-loop88105 k621 map-loop114131 deep-reverse-iter50 for-each-loop2737 reverse-iter18 length-iter7) 
o|calls to known targets: 25 
o|identified direct recursive calls: f_253 1 
o|identified direct recursive calls: f_320 1 
o|identified direct recursive calls: f_422 1 
o|fast box initializations: 8 
*/
/* end of file */
