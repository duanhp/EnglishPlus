{$IFNDEF UNIDACPRO}
{$I ..\Odac.inc}
unit Devart.Odac.OraCallCLR;
{$ENDIF}

interface

uses
  System.Runtime.InteropServices, System.Text,
  {$IFNDEF LITE}MTSCall, {$ENDIF}
  {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF};


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function obindps(cursor: IntPtr; opcode: ub1; sqlvar: PAnsiChar; sqlvl: sb4;
               pvctx: pub1; progvl: sb4; ftype: sword; scale: sword; indp: psb2;
               alen: pub2; arcode: pub2; pv_skip: sb4; ind_skip: sb4; alen_skip: sb4;
               rc_skip: sb4; maxsiz: ub4; cursiz: pub4; fmt: IntPtr; fmtl: sb4;
               fmtt: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function obreak(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ocan(cursor: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oclose(cursor: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ocof(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ocom(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ocon(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function odefinps(cursor: IntPtr; opcode: ub1; pos: sword; bufctx: IntPtr;
                bufl: sb4; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
                fmtl: sb4; fmtt: sword; rlen: pub2; rcode: pub2; pv_skip: sb4;
                ind_skip: sb4; alen_skip: sb4; rc_skip: sb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function odessp(lda: IntPtr; objnam: PAnsiChar; onlen: size_t; rsv1: pub1;
              rsv1ln: size_t; rsv2: pub1; rsv2ln: size_t; ovrld: pub2; pos: pub2;
              level: pub2; argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1;
              mode: pub1; dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
              spare: pub4; var arrsiz: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function odescr(cursor: IntPtr; pos: sword; var dbsize: sb4; var dbtype: sb2;
              cbuf: IntPtr; var cbufl: sb4; var dsize: sb4; var prec: sb2; var scale: sb2;
              var nullok: sb2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oerhms(lda: IntPtr; rcode: sb2; buf: IntPtr; bufsiz: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oermsg(rcode: sb2; buf: PAnsiChar): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oexec(cursor: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oexfet(cursor: IntPtr; nrows: ub4; cancel: sword; exact: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oexn(cursor: IntPtr; iters: sword; rowoff: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ofen(cursor: IntPtr; nrows: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ofetch(cursor: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oflng(cursor: IntPtr; pos: sword; buf: pub1; bufl: sb4; dtype: sword;
             retl: pub4; offset: sb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ogetpi(cursor: IntPtr; var piecep: ub1; var ctxpp: IntPtr; var iterp: ub4;
              var indexp: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oopt(cursor: IntPtr; rbopt: sword; waitopt: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function opinit(mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function olog(lda: IntPtr; hda: PHDA; uid: PAnsiChar; uidl: sword; pswd: PAnsiChar;
            pswdl: sword; conn: PAnsiChar; connl: sword; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ologof(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oopen(cursor: IntPtr; lda: IntPtr; dbn: IntPtr; dbnl: sword; arsize: sword;
             uid: IntPtr; uidl: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function oparse(cursor: IntPtr; sqlstm: PAnsiChar; sqllen: sb4; defflg: sword;
              lngflg: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function orol(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function osetpi(cursor: IntPtr; piece: ub1; bufp: IntPtr; var lenp: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlld2(lda: IntPtr; cname: PAnsiChar; cnlen: psb4); external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqllda(lda: IntPtr); external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function onbset(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function onbtst(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function onbclr(lda: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function ognfd(lda: IntPtr; fdp: IntPtr): sword; external;

  { OBSOLETE DEFINE CALLS }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function obndra(cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword;
                 progv: pub1; progvl: sword; ftype: sword; scale: sword;
                 indp: psb2; alen: pub2; arcode: pub2; maxsiz: ub4;
                 cursiz: pub4; fmt: IntPtr; fmtl: sword; fmtt: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function obndrn(cursor: IntPtr; sqlvn: sword; progv: pub1; progvl: sword;
              ftype: sword; scale: sword; indp: psb2; fmt: PAnsiChar; fmtl: sword;
              fmtt: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function obndrv(cursor: IntPtr; sqlvar: PAnsiChar; sqlvl: sword; progv: IntPtr;
              progvl: sword; ftype: sword; scale: sword; indp: psb2; fmt: IntPtr;
              fmtl: sword; fmtt: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function odefin(cursor: IntPtr; pos: sword; buf: IntPtr; bufl: sword; ftype: sword;
              scale: sword; indp: psb2; fmt: IntPtr; fmtl: sword; fmtt: sword;
              rlen: pub2; rcode: pub2): sword; external;





  [DllImport(OCIDLL, EntryPoint='OCIAttrGet', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIAttrGet1(trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
                  sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, EntryPoint='OCIAttrGet', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIAttrGet2(trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
                  sizep: pub4; attrtype: ub4; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, EntryPoint='OCIAttrSet', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIAttrSet1(trgthndlp: IntPtr; trghndltyp: ub4; attributep: IntPtr;
                  size: ub4; attrtype: ub4; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, EntryPoint='OCIAttrSet', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIAttrSet2(trgthndlp: IntPtr; trghndltyp: ub4; var attributep: Integer;
                  size: ub4; attrtype: ub4; errhp: pOCIError): sword; external;

  {[DllImport(OCIDLL, EntryPoint='OCIAttrSet', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIAttrSet3(trgthndlp: IntPtr; trghndltyp: ub4; attributep: PChar;
                  size: ub4; attrtype: ub4; errhp: pOCIError): sword; external;}

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBindArrayOfStruct(bindp: pOCIBind; errhp: pOCIError; pvskip: ub4;
                            indskip: ub4; alskip: ub4; rcskip: ub4): sword; external;

   [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBindByName(stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
                     placeholder: IntPtr; placeh_len: sb4; valuep: IntPtr; value_sz: sb4;
                     dty: ub2; indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
                     curelep: pub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBindByPos(stmtp: pOCIStmt; var bindpp: pOCIBind; errhp: pOCIError;
                    position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
                    indp: IntPtr; alenp: pub2; rcodep: pub2; maxarr_len: ub4;
                    curelep: pub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBindDynamic(bindp: pOCIBind; errhp: pOCIError; ictxp: IntPtr;
                      icbfp: IntPtr; octxp: IntPtr; ocbfp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBindObject(bindp: pOCIBind; errhp: pOCIError; const otype: pOCIType;
                     pgvpp: IntPtr; pvszsp: pub4; indpp: IntPtr; indszp: pub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIBreak(hndlp: pOCIHandle; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDefineArrayOfStruct(defnp: pOCIDefine; errhp: pOCIError;
                              pvskip: ub4; indskip: ub4; rlskip: ub4; rcskip: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDefineByPos(stmtp: pOCIStmt; var defnpp: pOCIDefine; errhp: pOCIError;
                      position: ub4; valuep: IntPtr; value_sz: sb4; dty: ub2;
                      indp: IntPtr; rlenp: pub2; rcodep: pub2; mode: ub4): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDefineDynamic(defnp: pOCIDefine; errhp: pOCIError; octxp: IntPtr;
                        ocbfp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDefineObject(defnp: pOCIDefine; errhp: pOCIError; const otype: pOCIType;
                        pgvpp: IntPtr; pvszsp: pub4; indpp: IntPtr; indszp: pub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDescribeAny(svchp: pOCISvcCtx; errhp: pOCIError; objptr: IntPtr;
                      objnm_len: ub4; objptr_typ: ub1; info_level: ub1; objtyp: ub1;  // WAR objtyp: ub1
                      dschp: pOCIDescribe): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDescriptorAlloc(parenth: IntPtr; var descpp: pOCIDescriptor;
                          dtype: ub4; xtramem_sz: size_t; usrmempp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDescriptorFree(descp: pOCIDescriptor; dtype: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIEnvInit(var envhpp: pOCIEnv; mode: ub4; xtramemsz: size_t;
                  usrmempp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIErrorGet(hndlp: pOCIHandle; recordno: ub4; sqlstate: IntPtr;
                   var errcodep: sb4; bufp: IntPtr; bufsiz: ub4; htype: ub4 ): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIHandleAlloc(parenth: IntPtr; var hndlpp: pOCIHandle; htype: ub4;
                      xtramem_sz: size_t; usrmempp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIHandleFree(hndlp: pOCIHandle; htype: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIInitialize(mode: ub4; ctxp: IntPtr; malocfp: IntPtr;
                     ralocfp: IntPtr; mfreefp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILdaToSvcCtx(var svchpp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIParamGet(hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
                   var parmdpp: pOCIParam; pos: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPasswordChange(svchp: pOCISvcCtx; errhp: pOCIError; const user_name: IntPtr;
                         usernm_len: ub4; const opasswd: IntPtr; opasswd_len: ub4;
                         const npasswd: IntPtr; npasswd_len: sb4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIReset(hndlp: pOCIHandle; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIServerAttach(srvhp: pOCIServer; errhp: pOCIError; dblink: IntPtr;
                       dblink_len: sb4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIServerDetach(srvhp: pOCIServer; errhp: pOCIError; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIServerVersion(hndlp: IntPtr; errhp: pOCIError; bufp: IntPtr; bufsz: ub4; hndltype: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionBegin(svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
                       credt: ub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionEnd(svchp: pOCISvcCtx; errhp: pOCIError; usrhp: pOCISession;
                     mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionGet(envhp: pOCIEnv; errhp: pOCIError; var svchp: pOCISvcCtx;
                     authhp: pOCIAuthInfo; poolName: IntPtr; poolName_len: ub4;
                     tagInfo: IntPtr; tagInfo_len: ub4; var retTagInfo: IntPtr;
                     var retTagInfo_len: ub4; var found: longbool; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionRelease(svchp: pOCISvcCtx; errhp: pOCIError; tag: IntPtr;
                         tag_len: ub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionPoolCreate(envhp: pOCIEnv; errhp: pOCIError; spoolhp: pOCISpool;
                            var poolName: IntPtr; var poolNameLen: ub4; connStr: IntPtr; connStrLen: ub4;
                            sessMin: ub4; sessMax: ub4; sessIncr: ub4; userid: IntPtr; useridLen: ub4;
                            password: IntPtr; passwordLen: ub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISessionPoolDestroy(spoolhp: pOCISPool; errhp: pOCIError;
                             mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransStart(svchp: pOCISvcCtx; errhp: pOCIError; timeout: word;
                     flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransRollback(svchp:pOCISvcCtx; errhp:pOCIError; flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransCommit(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransDetach(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransPrepare(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITransForget(svchp: pOCISvcCtx; errhp: pOCIError; flags: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtExecute(svchp: pOCISvcCtx; stmtp: pOCIStmt; errhp: pOCIError;
                      iters: ub4; rowoff: ub4; snap_in: pOCISnapshot; snap_out: pOCISnapshot;
                      mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtFetch(stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
                     mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtGetPieceInfo(stmtp: pOCIStmt; errhp: pOCIError; var hndlpp: pOCIHandle;
                           htypep: pub4; in_outp: pub1; iterp: pub4; idxp: pub4;
                           piecep: pub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtPrepare(stmtp: pOCIStmt; errhp: pOCIError; stmt: IntPtr;
                      stmt_len: ub4; language: ub4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtPrepare2(svchp: pOCISvcCtx; var stmtp: pOCIStmt; errhp: pOCIError;
                      stmt: IntPtr; stmt_len: ub4; key: IntPtr; key_len: ub4; language: ub4;
                      mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtRelease(stmtp: pOCIStmt; errhp: pOCIError; key: IntPtr; key_len: ub4;
                      mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtSetPieceInfo(hndlp: pOCIHandle; htype: ub4; errhp: pOCIError;
                           const bufp: IntPtr; alenp: pub4; piece: ub1;
                           const indp: IntPtr; rcodep: pub2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISvcCtxToLda(srvhp: pOCISvcCtx; errhp: pOCIError; ldap: IntPtr): sword; external;

{ LOB supports }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobAppend(svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
                    src_locp: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobAssign(envhp: pOCIEnv; errhp: pOCIError;
                     const src_locp: pOCILobLocator; var dst_locpp: pOCILobLocator): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobCharSetForm(envhp: pOCIEnv; errhp: pOCIError;
                         const locp: pOCILobLocator; var csfrm: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobCharSetId(envhp: pOCIEnv; errhp: pOCIError;
                        const locp: pOCILobLocator; csid: pub2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobCopy(svchp: pOCISvcCtx; errhp: pOCIError; dst_locp: pOCILobLocator;
                   src_locp: pOCILobLocator; amount: ub4; dst_offset: ub4;
                   src_offset: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobOpen(svchp: pOCISvcCtx; errhp: pOCIError;
                    locp: pOCILobLocator; mode: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobClose(svchp: pOCISvcCtx; errhp: pOCIError;
                    locp: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobIsOpen(svchp: pOCISvcCtx; errhp: pOCIError;
                    locp: pOCILobLocator; var flag: LongBool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobCreateTemporary(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                    csid: ub2; csfrm: ub1; lobtype: ub1; cache: tbool; duration: OCIDuration): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFreeTemporary(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobIsTemporary(envhp: pOCIEnv; errhp: pOCIError; locp: pOCILobLocator;
                    var is_temporary: LongBool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobDisableBuffering(svchp: pOCISvcCtx; errhp: pOCIError;
                              locp: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobEnableBuffering(svchp: pOCISvcCtx; errhp: pOCIError;
                             locp: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobErase(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                    amount: pub4; offset: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileClose(svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileExists(svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
                        var flag: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileGetName(envhp: pOCIEnv; errhp: pOCIError; const filep: pOCILobLocator;
                        dir_alias: IntPtr; d_length: pub2; filename: IntPtr; f_length: pub2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileIsOpen(svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
                        var flag: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileOpen(svchp: pOCISvcCtx; errhp: pOCIError; filep: pOCILobLocator;
                      mode: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFileSetName(envhp: pOCIEnv; errhp: pOCIError; filepp: ppOCILobLocator;
                         const dir_alias: IntPtr; d_length: ub2; const filename: IntPtr;
                         f_length: ub2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobFlushBuffer(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                         flag: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobGetLength(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                       var lenp: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobIsEqual(envhp: pOCIEnv; const x: pOCILobLocator; const y: pOCILobLocator;
                     is_equal: pbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobLoadFromFile(svchp: pOCISvcCtx; errhp: pOCIError;
                          dst_locp: pOCILobLocator; src_locp: pOCILobLocator;
                          amount: ub4; dst_offset: ub4; src_offset: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobLocatorIsInit(envhp: pOCIEnv; errhp: pOCIError; const locp: pOCILobLocator;
                           var is_initialized: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobRead(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                  var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; ctxp: IntPtr;
                  cbfp: IntPtr; csid: ub2; csfrm: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobRead2(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                  var byte_amtp: ub8; var char_amtp: ub8; offset: ub8; bufp: IntPtr; bufl: ub8;
                  piece: ub1; ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobTrim(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                  newlen: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCILobWrite(svchp: pOCISvcCtx; errhp: pOCIError; locp: pOCILobLocator;
                    var amtp: ub4; offset: ub4; bufp: IntPtr; bufl: ub4; piece: ub1;
                    ctxp: IntPtr; cbfp: IntPtr; csid: ub2; csfrm: ub1): sword; external;

{ Objects supports }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICacheFlush(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                     context: IntPtr; get: TGetFlushRef; var ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICacheFree(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICacheRefresh(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                       option: OCIRefreshOpt; context: IntPtr; get: TGetRefreshRef;
                       var ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICacheUnmark(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICacheUnpin(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectCopy(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                     source: IntPtr; null_source: IntPtr; target: IntPtr;
                     null_target: IntPtr; tdo: pOCIType; duration: OCIDuration;
                     option: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectExists(env: pOCIEnv; err: pOCIError; ins: IntPtr; var exist: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectFlush(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectFree(env: pOCIEnv; err: pOCIError; instance: IntPtr; flags: ub2): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectGetAttr(env: pOCIEnv; err: pOCIError; instance: IntPtr;
                        null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
                        const lengths: pub4; const name_count: ub4; const indexes: pub4;
                        const index_count: ub4; attr_null_status: pOCIInd;
                        attr_null_structp: IntPtr; attr_valuep: IntPtr; attr_tdop: ppOCIType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectGetInd(env: pOCIEnv; err: pOCIError; instance: IntPtr; null_structp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectGetObjectRef(env: pOCIEnv; err: pOCIError; pobject: IntPtr;
                             object_ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectGetProperty(env: pOCIEnv; err: pOCIError; const obj: IntPtr;
                            propertyId: OCIObjectPropId; prop: OCIObjectPropId; size: pub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectGetTypeRef(env: pOCIEnv; err: pOCIError; instance: IntPtr;
                           type_ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectIsDirty(env: pOCIEnv; err: pOCIError; ins: IntPtr; var dirty: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectIsLocked(env: pOCIEnv; err: pOCIError; ins: IntPtr; var lock: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectLock(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectMarkDelete(env: pOCIEnv; err: pOCIError; instance: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectMarkDeleteByRef(env: pOCIEnv; err: pOCIError; object_ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectMarkUpdate(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectNew(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                    typecode: OCITypeCode; tdo: pOCIType; table: IntPtr;
                    duration: OCIDuration; value: tbool; var instance: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectPin(env: pOCIEnv; err: pOCIError; object_ref: pOCIRef;
                    corhdl: pOCIComplexObject; pin_option: OCIPinOpt; pin_duration: OCIDuration;
                    lock_option: OCILockOpt; pobjectp: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectPinCountReset(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectPinTable(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                         const schema_name: IntPtr; s_n_length: ub4; const object_name: IntPtr;
                         o_n_length: ub4; not_used: IntPtr; pin_duration: OCIDuration;
                         var pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectRefresh(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectSetAttr(env: pOCIEnv; err: pOCIError; instance: IntPtr;
                        null_struct: IntPtr; tdo: pOCIType; namesp: IntPtr;
                        const lengths: pub4; const name_count: ub4; const indexes: pub4;
                        const index_count: ub4; const null_status: OCIInd;
                        const attr_null_struct: IntPtr; const attr_value: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectUnmark(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectUnmarkByRef(env: pOCIEnv; err: pOCIError; ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIObjectUnpin(env: pOCIEnv; err: pOCIError; pobject: IntPtr): sword; external;

  //function OCITypeArrayByName
  //function OCITypeArrayByRef

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITypeByName(env: pOCIEnv; err: pOCIError; const svc: pOCISvcCtx;
                     const schema_name: IntPtr; s_length: ub4; const type_name: IntPtr;
                     t_length: ub4; version_name: IntPtr; v_length: ub4;
                     pin_duration: OCIDuration; get_option: OCITypeGetOpt; var tdo: pOCIType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITypeByRef(env: pOCIEnv; err: pOCIError; const type_ref: pOCIRef;
                    pin_duration: OCIDuration; get_option: OCITypeGetOpt; tdo: pOCIType): sword; external;

{ OTS types }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollAppend(env: pOCIEnv; err: pOCIError; const elem: IntPtr;
                     const elemind: IntPtr; coll: pOCIColl): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollAssign(env: pOCIEnv; err: pOCIError; const rhs: pOCIColl;
                     lhs: pOCIColl): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollAssignElem(env: pOCIEnv; err: pOCIError; index: sb4;
                         const elem: IntPtr; const elemind: IntPtr; coll: pOCIColl): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollGetElem(env: pOCIEnv; err: pOCIError; const coll: pOCIColl;
                      index: sb4; var exists: tbool; var elem: IntPtr; var elemind: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollMax(env: pOCIEnv; const coll: pOCIColl): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollSize(env: pOCIEnv; err: pOCIError; const coll: pOCIColl; var size: sb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCICollTrim(env: pOCIEnv; err: pOCIError; trim_num: sb4; coll: pOCIColl): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateAssign(err: pOCIError; const from: pOCIDate; todate: pOCIDate): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateFromText(err: pOCIError; date_str: IntPtr; d_str_length: ub4;
                       const fmt: IntPtr; fmt_length: ub1; const lang_name: IntPtr;
                       lang_length: ub4; date: pOCIDate): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateGetDate(const date: pOCIDate; year: psb2; month: pub1; day: pub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateGetTime(const date: pOCIDate; hour: pub1; min: pub1; sec: pub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateSetDate(date: pOCIDate; year: sb2; month: ub1; day: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateSetTime(date: pOCIDate; hour: ub1; min: ub1; sec: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateToText(err: pOCIError; const date: pOCIDate; const fmt: IntPtr;
                     fmt_length: ub1; const lang_name: IntPtr; lang_length: ub4;
                     buf_size: pub4; buf: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberAssign(err: pOCIError; const from: pOCINumber; tonum: pOCINumber): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberCmp( err: pOCIError; const number1: pOCINumber;
                    const number2: pOCINumber; var result: sword): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberFromInt(err: pOCIError; var inum: int64;
                        inum_length: uword; inum_s_flag: uword; number: pOCINumber): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberFromReal(err: pOCIError; var rnum: double; rnum_length: uword;
                         number: pOCINumber): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberFromText(err: pOCIError; const str: IntPtr; str_length: ub4;
                         const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
                         nls_p_length: ub4; number: pOCINumber): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberToInt(err: pOCIError; number: pOCINumber; rsl_length: uword;
                      rsl_flag: uword; var rsl: int64): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberToReal(err: pOCIError; const number: pOCINumber; rsl_length: uword;
                       var rsl: double): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCINumberToText(err: pOCIError; number: pOCINumber;
                       const fmt: IntPtr; fmt_length: ub4; const nls_params: IntPtr;
                       nls_p_length: ub4; var buf_size: ub4; buf: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRefAssign(env: pOCIEnv; err: pOCIError; const source: pOCIRef;
                    var target: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRefClear(env: pOCIEnv; ref: pOCIRef): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRefIsEqual(env: pOCIEnv; const x: pOCIRef; const y: pOCIRef): tbool; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRefIsNull(env: pOCIEnv; const ref: pOCIRef): tbool; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRefToHex(env: pOCIEnv; err: pOCIError; const ref: pOCIRef;
                    hex: IntPtr; var hex_length: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringAllocSize(env: pOCIEnv; err: pOCIError; const vs: pOCIString; allocsize: pub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringAssign(env: pOCIEnv; err: pOCIError; const rhs: pOCIString;
                       lhs: ppOCIString): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringAssignText(env: pOCIEnv; err: pOCIError; const rhs: IntPtr;
                           rhs_len: ub4; lhs: ppOCIString): sword; external;  // Oracle documentation bag rhs_len: ub2;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringPtr(env: pOCIEnv; const vs: pOCIString): IntPtr; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringResize(env: pOCIEnv; err: pOCIError; new_size: ub4; str: ppOCIString): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStringSize(env: pOCIEnv; const vs: pOCIString): ub4; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableDelete(env: pOCIEnv; err: pOCIError; index: sb4; tbl: pOCITable): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableExists(env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
                      index: sb4; exists: pbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableFirst(env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
                     index: psb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableLast(env: pOCIEnv; err: pOCIError; const tbl: pOCITable;
                    index: psb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableNext(env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
                    var next_index: sb4; var exists: tbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITablePrev(env: pOCIEnv; err: pOCIError; index: sb4; const tbl: pOCITable;
                    prev_index: psb4; exists: pbool): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCITableSize(env: pOCIEnv; err: pOCIError; const tbl: pOCITable; var size: sb4): sword; external;

{ OCI81 }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIEnvCreate(var envhpp: pOCIEnv; mode: ub4; const ctxp: IntPtr;
                    const malocfp: IntPtr; const ralocfp: IntPtr;
                    const mfreefp: IntPtr; xtramemsz: size_t; usrmempp: IntPtr): sword; external;

{ Direct path load interface support }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathAbort(dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathColArrayEntryGet(dpca: pOCIDirPathColArray; errhp: pOCIError;
                                  rownum: ub4; colIdx: ub2; var cvalpp: pub1; clenp: ub4;
                                  cflgp: pub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathColArrayEntrySet(dpca: pOCIDirPathColArray; errhp: pOCIError;
                                  rownum: ub4; colIdx: ub2; cvalp: pub1; clen: ub4;
                                  cflg: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathColArrayRowGet(dpca: pOCIDirPathColArray; errhp: pOCIError;
                                rownum: ub4; var cvalppp: pub1; var clenpp: pub4; {TODO cvalppp: pppub1; }
                                cflgpp: ppub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathColArrayReset(dpca: pOCIDirPathColArray; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathColArrayToStream(dpca: pOCIDirPathColArray; const dpctx: pOCIDirPathCtx;
                                  dpstr: pOCIDirPathStream; errhp: pOCIError; rowcnt: ub4;
                                  rowoff: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathFinish(dpctx: pOCIDirPathCtx; errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathLoadStream(dpctx: pOCIDirPathCtx; dpstr: pOCIDirPathStream;
                            errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathPrepare(dpctx: pOCIDirPathCtx; svchp: pOCISvcCtx;
                         errhp: pOCIError): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDirPathStreamReset(dpstr: pOCIDirPathStream; errhp: pOCIError): sword; external;

{ OCI9 }

  { Timestamp and interval types support }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeConstruct(hndl: IntPtr; err: pOCIError;
                                    datetime: pOCIDateTime; year: sb2;
                                    month, day, hour, min, sec: ub1; fsec: ub4;
                                    timezone: IntPtr; timezone_length: integer): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeCheck(hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
		                            var valid: ub4): sword; external;


  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeFromText(hndl: IntPtr; err: pOCIError;
                                   date_str: IntPtr; d_str_length: integer;
                                   fmt: IntPtr; fmt_length: ub1; lang_name: IntPtr;
                                   lang_length: integer; date: pOCIDateTime): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeToText(hndl: IntPtr; err: pOCIError; date: pOCIDateTime;
                                 fmt: IntPtr; fmt_length, fsprec: ub1;
                                 lang_name: IntPtr; lang_length: integer;
                                 var buf_size: ub4; buf: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeGetDate(hndl: IntPtr; err: pOCIError;
                                  date: pOCIDateTime; var year: sb2; var month: ub1; var day: ub1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeGetTime(hndl: IntPtr; err: pOCIError;
                                  datetime: pOCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
                                  var fsec: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeGetTimeZoneOffset(hndl: IntPtr; err: pOCIError;
                                            datetime: pOCIDateTime; var hour: sb1; var minute: sb1): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeGetTimeZoneName(hndl: IntPtr; err: pOCIError;
                                          datetime: pOCIDateTime; buf: pub1;
                                          var buflen: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeAssign(hndl: IntPtr; err: pOCIError; src: pOCIDateTime;
			                           dst: pOCIDateTime): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDateTimeCompare(hndl: IntPtr; err: pOCIError; const date1: pOCIDateTime;
                                  const date2: pOCIDateTime; var result: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalFromText(hndl: IntPtr; err: pOCIError; inpstr: IntPtr;
		                               str_len: integer; result: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalToText(hndl: IntPtr; err: pOCIError; inter: pOCIInterval;
                                 lfprec, fsprec: ub1;	buffer: IntPtr; buflen: integer;
                                 var resultlen: sb4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalCheck(hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
			                          var valid: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalAssign(hndl: IntPtr; err: pOCIError; ininter: pOCIInterval;
			                           outinter: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalCompare(hndl: IntPtr; err: pOCIError; inter1: pOCIInterval;
                                  inter2: pOCIInterval; var result: sword): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalSetYearMonth(hndl: IntPtr; err: pOCIError; yr, mnth: sb4;
                                       result: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalGetYearMonth(hndl: IntPtr; err: pOCIError; var yr: sb4; var mnth: sb4;
                                       result: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalSetDaySecond(hndl: IntPtr; err: pOCIError; dy, hr,
                                       mm, ss, fsec: sb4; result: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalGetDaySecond(hndl: IntPtr; err: pOCIError; var dy: sb4; var hr: sb4;
                                       var mm: sb4; var ss: sb4; var fsec: sb4; result: pOCIInterval): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIIntervalFromNumber(hndl: IntPtr; err: pOCIError; interval: pOCIInterval;
                                     number: pOCINumber): sword; external;
  { Scrollable cursors }

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIStmtFetch2(stmtp: pOCIStmt; errhp: pOCIError; nrows: ub4; orientation: ub2;
                     scrollOffset: sb4; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPing(svchp: pOCISvcCtx; errhp: pOCIError; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeNew(svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
                 elname: PAnsiChar; elname_Len: ub4; schemaURL: PAnsiChar;
                 schemaURL_Len: ub4; var retInstance: pOCIXMLType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeCreateFromSrc(svchp: pOCISvcCtx; errhp: pOCIError; dur: OCIDuration;
                 src_type: ub1; src_ptr: IntPtr; ind: sb4; var retInstance: pOCIXMLType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeExtract(errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
              xpathexpr: PAnsiChar; xpathexpr_Len: ub4;
              nsmap: PAnsiChar; nsmap_Len: ub4;
              var retDoc: pOCIXMLType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeTransform(errhp: pOCIError; dur: OCIDuration;
               doc: pOCIXMLType; xsldoc: pOCIXMLType;
               var retDoc: pOCIXMLType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeExists(errhp: pOCIError; doc: pOCIXMLType;
                 xpathexpr: PAnsiChar; xpathexpr_Len: ub4;
                 nsmap: PAnsiChar; nsmap_Len: ub4;
                 var retval: LongWord): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeIsSchemaBased(errhp: pOCIError;
                              doc: pOCIXMLType; var retval: LongWord): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeGetSchema(errhp: pOCIError; doc: pOCIXMLType;
             var schemadoc: pOCIXMLType;
             var schemaURL: IntPtr; var schemaURL_Len: ub4;
             var rootelem: IntPtr; var rootelem_Len: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeValidate(errhp: pOCIError; doc: pOCIXMLType;
                   schemaURL: PAnsiChar; schemaURL_Len: ub4; var retval: LongWord): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeGetDOM(errhp: pOCIError; doc: pOCIXMLType; dur: OCIDuration;
                       var retDom: pOCIDOMDocument): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIXMLTypeGetFromDOM(errhp: pOCIError; domdoc: pOCIDOMDocument;
                           var retXMLType: pOCIXMLType): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIDOMFree(errhp: pOCIError; domdoc: pOCIDOMDocument): sword; external;


  [DllImport('oraclient9.dll', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamFromXMLType(errhp: pOCIError; phOCIDescriptor: IntPtr; pobject: IntPtr; res: integer): sword; external;

  [DllImport('oraclient9.dll', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamRead(errhp: pOCIError; phOCIDescriptor: IntPtr; pStr: IntPtr; var Len: int64; res: integer): sword; external;

  [DllImport('oraclient9.dll', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamClose(errhp: pOCIError; phOCIDescriptor: IntPtr): sword; external;

  {10g Oracle support}
  [DllImport(OCIDLL, EntryPoint = 'OCIPStreamFromXMLType', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamFromXMLType10(errhp: pOCIError; phOCIDescriptor: IntPtr; pobject: IntPtr; res: integer): sword; external;

  [DllImport(OCIDLL, EntryPoint = 'OCIPStreamRead', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamRead10(errhp: pOCIError; phOCIDescriptor: IntPtr; pStr: IntPtr; var Len: int64; res: integer): sword; external;

  [DllImport(OCIDLL, EntryPoint = 'OCIPStreamClose', CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIPStreamClose10(errhp: pOCIError; phOCIDescriptor: IntPtr): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]  
  function OCIClientVersion(var major_version, minor_version, update_num, patch_num, port_update_num: sword): sword; external;

  { Misc }
  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCIRowidToChar(rowidDesc: IntPtr; outbfp: IntPtr;
                                 var outbflp: ub2; errhp: pOCIError): sword; external;

  { Publish - subscribe support }
  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISubscriptionRegister(svchp: pOCISvcCtx; var subscrhpp: pOCISubscription;
    count: ub2; errhp: pOCIError; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISubscriptionUnRegister(svchp: pOCISvcCtx; subscrhp: pOCISubscription;
    errhp: pOCIError; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISubscriptionEnable(subscrhp: pOCISubscription; errhp: pOCIError; mode: ub4): sword; external;

  [DllImport(OCIDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OCISubscriptionDisable(subscrhp: pOCISubscription; errhp: pOCIError; mode: ub4): sword; external;

{$IFNDEF LITE}
  { MTS support }
  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSSvcGet(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar; var pOCISvc: pOCISvcCtx;
                   var pOCIEnv: pOCIEnv; ConFlg: ub4): sword; external;

  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSSvcRel(OCISvc: pOCISvcCtx): sword; external;

  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSJoinTxn(svchp: pOCISvcCtx; lpTrans: ICRTransactionSC): sword; external;

  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSEnlCtxGet(lpUName: PAnsiChar; lpPsswd: PAnsiChar; lpDbnam: PAnsiChar;
                               pOCISvc: pOCISvcCtx; errhp: pOCIError; dwFlags: ub4;
                               var pCtxt: pOCISvcCtx): sword; external;

  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSEnlCtxRel(pCtxt: pOCISvcCtx): sword; external;

  [DllImport(MTSDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function OraMTSSvcEnlist(OCISvc: pOCISvcCtx; OCIErr: pOCIError; lpTrans: ICRTransactionSC;
                       dwFlags: LongInt): sword; external;
{$ENDIF}
implementation
end.
