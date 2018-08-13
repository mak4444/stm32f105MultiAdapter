REQUIRE THUMB2_MOD ~mak\ARM7\SRC\thumb2.f

MODULE: THUMB2_MOD

[IFNDEF] M! : M! ! ; [THEN]

ALSO FORTH

' NOOP VALUE INCT_V

: A; 
 INCT_V
 ['] NOOP TO INCT_V
 EXECUTE
 ;

[UNDEFINED] ASM_INTERPRET
[IF]  : ASM_INTERPRET INTERPRET ;
[THEN]

: P:_INTERPRET ( cfa -- ... )
    >R  A;  R> TO INCT_V
       >IN M@ >R 
\+ TRACE  ." >ASM_INT"
\ + TRACE TRGO TRACE
    ASM_INTERPRET
               R> >IN M@ >R >IN M!  \ монипул€ции с >IN ради обработчика
       A;      R> >IN M!           \ ошибок
;

: P: >IN M@ '
  SWAP  >IN M! PARSE-NAME 1- CREATED , POSTPONE \
 DOES>
\ + TRACE  TRACE
 M@
\+ TRACE ." P:=" DUP H.
 P:_INTERPRET ;

PREVIOUS

: lsl ?, IF lsl$ ELSE ['] lsl, P:_INTERPRET THEN ;
: asl lsl ;
: lsr ?, IF lsr$ ELSE ['] lsr, P:_INTERPRET THEN ;
: asr ?, IF asr$ ELSE ['] asr, P:_INTERPRET THEN ;
: ror ?, IF ror$ ELSE ['] ror, P:_INTERPRET THEN ;

: cpsie cps_PAR cpsie, ;
: cpsid cps_PAR cpsid, ;

P: MRS,
P: MSR,

P: DSB,
P: DMB,
P: ISB,

P: rev.w,
P: clz.w,
P: rev16.w,
P: rbit.w,
P: revsh.w,

P: rev,
P: clz,
P: rev16,
P: rbit,
P: revsh,

P: NEG,
P: NEG.W,

P: NEGS,
P: NEGS.W,
P: MULS, 
P: MUL,
P: MUL.W,


P: ADDW,
P: SUBW,

P: MOVW,
P: MOVT,

P: sdiv,
P: udiv,
P: MLS,
P: MLA,
P: smull,
P: smlal,
P: umull,
P: umlal,

P: RORS.W,
P: RORS,
P: ROR.W,

P: asrs.w,
P: ASRS, 
P: asr.w,

P: LSLS.W,
P: LSLS,
P: LSL.W,

P: LSRS.W,
P: LSRS,
P: LSR.W,

P: str.w,
P: str,
P: ldr.w,
P: ldr,
P: ldrsh.w,
P: ldrsh,
P: strb.w,
P: strb,
P: ldrb.w,
P: ldrb,
P: ldrsb,
P: ldrsb.w,
P: strh.w,
P: strh,
P: ldrh.w,
P: ldrh,

P: LDREX,
P: LDREXB,
P: LDREXH,

P: strd,
P: ldrd,

P: strex,
P: strexb,
P: strexh,

P: itttt,
P: ittt,	
P: ittte,
P: itt,	
P: ittet,
P: itte,	
P: ittee,
P: it,	
P: itett,
P: itet,	
P: itete,
P: ite,	
P: iteet,
P: itee,	
P: iteee,

P: B.N,

P: beq.n,
P: bne.n,
P: bcs.n,
P: bcc.n,
P: bmi.n,
P: bpl.n,
P: bvs.n,
P: bvc.n,
P: bhi.n,
P: bls.n,
P: bge.n,
P: blt.n,
P: bgt.n,
P: ble.n,

P: beq.w,
P: bne.w,
P: bcs.w,
P: bcc.w,
P: bmi.w,
P: bpl.w,
P: bvs.w,
P: bvc.w,
P: bhi.w,
P: bls.w,
P: bge.w,
P: blt.w,
P: bgt.w,
P: ble.w,

P: beq,
P: bne,
P: bcs,
P: bcc,
P: bmi,
P: bpl,
P: bvs,
P: bvc,
P: bhi,
P: bls,
P: bge,
P: blt,
P: bgt,
P: ble,


P: cbz, 
P: cbnz,

P: POP.W,
P: POP,
P: PUSH.W,
P: PUSH,

P: stmia.w,
P: stmia,

P: stmea.w,
P: stm.w,

P: ldmia.w,
P: ldmia,


P: ldmfd.w,
P: ldm.w,

P: stmdb.w,
P: stmdb,
P: ldmdb.w,
P: ldmdb,



P: bl,
P: B.W,
P: B,

P: BX,
P: BLX,

P: NOP,
P: NOP.W,
P: clrex,
P: yield,
P: wfe,
P: wfi,
P: sev,

P: AND.W,
P: ANDS.W,
P: ANDS,
P: AND,
P: BIC.W,
P: BICS.W,
P: BICS,
P: BIC,
P: ORR.W,
P: ORRS.W,
P: ORRS,
P: ORR,
P: ORNS,	
P: ORN,	
P: EOR.W,
P: EORS.W,
P: EORS, 
P: EOR, 
P: ADD.W,
P: ADDS.W,
P: ADDS,
P: ADD,
P: ADC.W,
P: ADCS.W,
P: ADCS,
P: ADC,
P: SBC.W,
P: SBC,
P: SBCS.W,
P: SBCS,
P: SUB.W,
P: SUBS.W,
P: SUBS,
P: SUB,
P: RSB.W,
P: RSBS.W,
P: RSBS,
P: RSB,

P: mov.w,
P: MOV,
P: movs.w,
P: movs,
P: mvn.w,
P: mvn,
P: MVNS.W,
P: MVNS, 

P: tst.w,
P: TST,
P: teq,	
P: cmn.w,
P: CMN,  
P: cmp.w,
P: CMP,  


P: sxth.w,
P: uxth.w,
P: sxtb16,
P: uxtb16,
P: sxtb.w,
P: uxtb.w,

P: sxtah,
P: uxtah,
P: sxtab16,
P: uxtab16,
P: sxtab,
P: uxtab,

P: sxth,
P: uxth,
P: sxtb,
P: uxtb,

P: tbb,
P: tbH,
P: ssat,
P: usat,
P: ssat16,
P: usat16,

P: sbfx,
P: ubfx,
P: bfi,
P: bfc,

P: SVC,
P: SWI,
P: bkpt,
P: ADR,
P: ADR.W,

P: .WORD,

P: VMRS,

P: VMOV,

P: VMOV.F32,
P: VABS.F32,
P: VNEG.F32,
P: VSQRT.F32,
P: VCVTB.F32.F16,
P: VCVTT.F32.F16,
P: VCVTB.F16.F32,
P: VCVTT.F16.F32,
P: VCMP.F32,
P: VCMPE.F32,
P: VCVT.F32.S32, 
P: VCVT.F32.U32, 
P: VCVT.U32.F32, 
P: VCVTR.U32.F32,
P: VCVTR.S32.F32,
P: VCVT.S32.F32,

P: VMLA.F32,
P: VMLS.F32,
P: VNMLA.F32,
P: VNMLS.F32,
P: VMUL.F32,
P: VNMUL.F32,
P: VADD.F32,
P: VSUB.F32,
P: VDIV.F32,
P: VFMNS.F32,
P: VFMNA.F32,
P: VFMA.F32,
P: VFMS.F32,


P: VLDMIA,

;MODULE

