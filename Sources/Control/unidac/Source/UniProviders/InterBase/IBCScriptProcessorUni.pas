
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  IBCScriptProcessor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I IbDac.inc}

unit IBCScriptProcessorUni;
{$ENDIF}

interface

uses
  {$IFDEF CLR}Variants, {$ENDIF}
  Classes, SysUtils, DB, MemUtils, DBAccess, DAScript, CRParser, CRAccess,
{$IFNDEF UNIDACPRO}
  IBCClasses;
{$ELSE}
  IBCClassesUni;
{$ENDIF}

const
  ST_DML = 100;
  ST_DDL = 101;
  ST_START_SAVEPOINT = 102;
  ST_RELEASE_SAVEPOINT = 103;
  ST_ROLLBACK_SAVEPOINT = 104;
  ST_START_TRANSACTION = 105;
  ST_COMMIT_TRANSACTION = 106;
  ST_COMMIT_RET_TRANSACTION = 107;
  ST_ROLLBACK_TRANSACTION = 108;
  ST_ROLLBACK_RET_TRANSACTION = 109;
  ST_CONNECT = 110;
  ST_RECONNECT = 111;
  ST_SET = 112;
  ST_CREATE_DATABASE = 113;
  ST_DROP_DATABASE = 114;
  ST_BATCH_START = 115;
  ST_BATCH_EXECUTE = 116;

  prAutoDDL = 101;

type
  TCustomIBCScriptProcessor = class (TDAScriptProcessor)
  protected
    FStoredDataSet: TCustomDADataSet;  //Used to store defined DataSet and Transaction in case of DDL statements execution
    FStoredTransaction: TDATransaction;

    FAutoDDL: Boolean;
    FDDLTransaction: TDATransaction;

    FCodes: array of Integer;
    FLexemNo: integer;
    FInBatch: boolean;
    FBatchStatements: _TStringArray;
    FBeginCount: Integer;
    FInBody: boolean;

    procedure SetAutoDDL(Value: boolean); virtual; abstract;
    procedure SetDatabase(Connection: TCustomDAConnection; const Value: string); virtual; abstract;
    procedure SetRole(Connection: TCustomDAConnection; const Value: string); virtual; abstract;
    procedure SetCharset(Connection: TCustomDAConnection; const Value: string); virtual; abstract;
    procedure SetSQLDialect(Connection: TCustomDAConnection; Value: integer); virtual; abstract;
    procedure CreateDatabase(Connection: TCustomDAConnection; const Params: string); virtual; abstract;
    procedure DropDatabase(Connection: TCustomDAConnection); virtual; abstract;

    function GetParserClass: TSQLParserClass; override;
    procedure Reset; override;
    procedure ExecuteStatement(const SQL: _string; StatementType: integer; var Omit: Boolean; out BreakExec: boolean; Params: TDAParams = nil); override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
    function IsSpecificSQL(StatementType: integer): boolean; override;

    procedure DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean); override;
    procedure DoAfterStatementExecute(var SQL: _string; StatementType: integer); override;

  public
    constructor Create(Owner: TDAScript); override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  IBCParser, IBCConsts;
{$ELSE}
  IBCParserUni, IBCConstsUni;
{$ENDIF}

{ TCustomIBCScriptProcessor }

constructor TCustomIBCScriptProcessor.Create(Owner: TDAScript);
begin
  inherited Create(Owner);

  FAutoDDL := True;
  SetLength(FCodes, 2);
end;

destructor TCustomIBCScriptProcessor.Destroy;
begin
  FDDLTransaction.Free;

  inherited Destroy;
end;

function TCustomIBCScriptProcessor.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoDDL:
      FAutoDDL := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TCustomIBCScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TIBCParser;
end;

procedure TCustomIBCScriptProcessor.Reset;
begin
  inherited;

  FLexemNo := 1;
  FBeginCount := 0;
end;

procedure TCustomIBCScriptProcessor.ExecuteStatement(const SQL: _string; StatementType: integer;
  var Omit: Boolean; out BreakExec: boolean; Params: TDAParams = nil);
begin
  if not FStatementsPopulating then begin
    if FDDLTransaction = nil then
      FDDLTransaction := UsedConnection.CreateTransaction;
    FDDLTransaction.DefaultConnection := UsedConnection;
  end;

  inherited;
end;


procedure TCustomIBCScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
begin
  inherited;

  if (Code <> lcBlank) and (Code <> lcComment) then begin
    case FLexemNo of
      1:
        case Code of
          lxCOMMIT:
            StatementType := ST_COMMIT_TRANSACTION;
          lxROLLBACK:
            StatementType := ST_ROLLBACK_TRANSACTION;
          lxSAVEPOINT:
            StatementType := ST_START_SAVEPOINT;
          lxDECLARE, lxCREATE, lxALTER, lxGRANT, lxREVOKE, lxDROP:
            StatementType := ST_DDL;
          lxCONNECT:
            StatementType := ST_CONNECT;
          lxRECONNECT:
            StatementType := ST_RECONNECT;
        else
          StatementType := ST_DML;
        end;
      2:
        case Code of
          lxTERM:
            if FCodes[0] = lxSET then begin
              FDelimiterState := dsDelimiter;
              StatementType := ST_DELIMETER;
            end;
          lxGENERATOR:
            if FCodes[0] = lxSET then
              StatementType := ST_DDL;
          lxSAVEPOINT:
            if FCodes[0] = lxRELEASE then
              StatementType := ST_RELEASE_SAVEPOINT;
          lxTRANSACTION:
            if FCodes[0] = lxSET then
              StatementType := ST_START_TRANSACTION;
          lxRETAIN: begin
            if FCodes[0] = lxCOMMIT then
              StatementType := ST_COMMIT_RET_TRANSACTION
            else
              if FCodes[0] = lxROLLBACK then
                StatementType := ST_ROLLBACK_RET_TRANSACTION;
          end;
          lxPROCEDURE, lxFUNCTION, lxTRIGGER:
            if (FCurrDelimiter = ';') and
              ((FCodes[0] = lxCREATE) or (FCodes[0] = lxALTER))
            then
              StatementType := StatementType or ST_SPECIFIC_SQL;
          lxAUTODDL, lxNAMES:
            if FCodes[0] = lxSET then
              StatementType := ST_SET;
          lxDATABASE: begin
            if FCodes[0] = lxCREATE then
              StatementType := ST_CREATE_DATABASE
            else
              if FCodes[0] = lxDROP then
                StatementType := ST_DROP_DATABASE;
          end;
          lxSCHEMA:
            if FCodes[0] = lxCREATE then
              StatementType := ST_CREATE_DATABASE;
          lxSTART:
            if FCodes[0] = lxBATCH then
              StatementType := ST_BATCH_START;
          lxEXECUTE:
            if FCodes[0] = lxBATCH then
              StatementType := ST_BATCH_EXECUTE;
          lxBLOCK:
            if FCodes[0] = lxEXECUTE then
              StatementType := ST_SPECIFIC_SQL;
        end;
      3:
        case Code of
          lxSAVEPOINT:
            if (FCodes[1] = lxTO) and (FCodes[0] = lxROLLBACK) then
              StatementType := ST_ROLLBACK_SAVEPOINT;
          lxDIALECT:
            if (FCodes[1] = lxSQL) and (FCodes[0] = lxSET) then
              StatementType := ST_SET;
          lxALTER: begin
            StatementType := ST_DDL;
            FLexemNo := 1;
          end;
        end;
    end;

    if FLexemNo - 1 <= High(FCodes) then
      FCodes[FLexemNo - 1] := Code
    else begin
      if (StatementType and ST_SPECIFIC_SQL) <> 0 then
        case Code of
          lxAS:
            FInBody := True;
          lxBEGIN:
            Inc(FBeginCount);
          lxEND: begin
            Dec(FBeginCount);
            if FBeginCount <= 0 then
              FInBody := False;
          end;
        end;
    end;

    Inc(FLexemNo);
  end;
end;

function TCustomIBCScriptProcessor.IsSpecificSQL(StatementType: integer): boolean;
begin
  Result := ((StatementType and ST_SPECIFIC_SQL) <> 0) and (FInBody or (FBeginCount > 0));
end;

procedure TCustomIBCScriptProcessor.DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean);
var
  SQLParser: TParser;
  Code: integer;
  St, OldSt: _string;
  DatabaseVal, UsernameVal, PasswordVal, RoleVal, ParamsVal: _string;
  OldConnected: boolean;
  UsedTransaction: TDATransaction;
  ExplicitTrans: boolean;

  procedure BypassBlanks;
  begin
    repeat
      Code := SQLParser.GetNext(St);
    until (Code <> lcBlank) or (Code = lcEnd);
  end;

  procedure RaiseException;
  begin
    raise Exception.CreateFmt(SInvalidLexem, [St, SQLParser.CurrPos + 1 - Length(St), SQL]);
  end;

begin
  if Omit then
    Exit;

  if FInBatch and (StatementType <> ST_BATCH_EXECUTE) then begin
    SetLength(FBatchStatements, Length(FBatchStatements) + 1);
    FBatchStatements[Length(FBatchStatements) - 1] := SQL;
    Omit := True;
  end;

  case StatementType of
    ST_START_SAVEPOINT: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxSAVEPOINT
      BypassBlanks; // Name
      if Code <> lcIdent then
        RaiseException;

      TDBAccessUtils.Savepoint(TDAScriptUtils.UsedTransaction(FOwner), St);
      Omit := True;
    end;

    ST_RELEASE_SAVEPOINT: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxRELEASE
      BypassBlanks; // lxSAVEPOINT
      BypassBlanks; // Name
      if Code <> lcIdent then
        RaiseException;

      TDBAccessUtils.ReleaseSavepoint(TDAScriptUtils.UsedTransaction(FOwner), St);
      Omit := True;
    end;

    ST_ROLLBACK_SAVEPOINT: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxROLLBACK
      BypassBlanks; // lxTO
      BypassBlanks; // lxSAVEPOINT
      BypassBlanks; // Name
      if Code <> lcIdent then
        RaiseException;

      TDBAccessUtils.RollbackToSavepoint(TDAScriptUtils.UsedTransaction(FOwner), St);
      Omit := True;
    end;

    ST_START_TRANSACTION: begin
      TDAScriptUtils.UsedTransaction(FOwner).StartTransaction;
      Omit := True;
    end;

    ST_COMMIT_TRANSACTION: begin
      if not TDAScriptUtils.UsedTransaction(FOwner).Active then
        TDAScriptUtils.UsedTransaction(FOwner).StartTransaction;
      TDAScriptUtils.UsedTransaction(FOwner).Commit;
      Omit := True;
    end;

    ST_ROLLBACK_TRANSACTION: begin
      if not TDAScriptUtils.UsedTransaction(FOwner).Active then
        TDAScriptUtils.UsedTransaction(FOwner).StartTransaction;
      TDAScriptUtils.UsedTransaction(FOwner).Rollback;
      Omit := True;
    end;

    ST_COMMIT_RET_TRANSACTION: begin
      TDBAccessUtils.CommitRetaining(TDAScriptUtils.UsedTransaction(FOwner));
      Omit := True;
    end;

    ST_ROLLBACK_RET_TRANSACTION: begin
      TDBAccessUtils.RollbackRetaining(TDAScriptUtils.UsedTransaction(FOwner));
      Omit := True;
    end;

    ST_CONNECT: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxCONNECT
      BypassBlanks; // Database
      if Code = lcIdent then
        DatabaseVal := IBCSQLInfo.UnQuote(St)
      else
      if Code = lcString then
        DatabaseVal := _DequotedStr(St, '''')
      else
        RaiseException;

      while True do begin
        BypassBlanks; // Param name
        if Code = lcEnd then
          Break;
        if Code <> lcIdent then
          RaiseException;
        OldSt := St;

        BypassBlanks; // Param value
        if Code = lcIdent then
          St := IBCSQLInfo.UnQuote(St)
        else
        if Code = lcString then
          St := _DequotedStr(St, '''')
        else
          RaiseException;
        if _SameText(OldSt, 'USER') then
          UsernameVal := St
        else
          if _SameText(OldSt, 'PASSWORD') then
            PasswordVal := St
          else
            if _SameText(OldSt, 'ROLE') then
              RoleVal := St;
      end;

      with UsedConnection do begin
        Disconnect;
        Username := UsernameVal;
        Password := PasswordVal;
        SetDatabase(UsedConnection, DatabaseVal);
        SetRole(UsedConnection, RoleVal);
        Connect;
      end;
      Omit := True;
    end;

    ST_RECONNECT: begin
      with UsedConnection do begin
        Disconnect;
        Connect;
      end;
      Omit := True;
    end;

    ST_SET: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxSET
      BypassBlanks; // Param name
      case Code of
        lxAUTODDL: begin
          BypassBlanks; // Param value
          SetAutoDDL(_SameText(St, 'ON'));
        end;
        lxSQL: begin
          BypassBlanks; // lxDIALECT
          BypassBlanks; // Param value
          if Code <> lcNumber then
            RaiseException;
          with UsedConnection do begin
            OldConnected := Connected;
            Disconnect;
            SetSQLDialect(UsedConnection, StrToInt(St));
            Connected := OldConnected;
          end;
        end;
        lxNAMES: begin
          BypassBlanks; // Param value
          with UsedConnection do begin
            OldConnected := Connected;
            Disconnect;
            SetCharset(UsedConnection, St);
            Connected := OldConnected;
          end;
        end;
      end;
      Omit := True;
    end;

    ST_CREATE_DATABASE: begin
      SQLParser := GetSQLParser(SQL);

      BypassBlanks; // lxCREATE
      BypassBlanks; // lxDATABASE or lxSCHEMA
      BypassBlanks; // Database
      if Code = lcIdent then
        DatabaseVal := IBCSQLInfo.UnQuote(St)
      else
      if Code = lcString then
        DatabaseVal := _DequotedStr(St, '''')
      else
        RaiseException;

      BypassBlanks; // Params
      ParamsVal := '';
      while (Code <> lcEnd) do begin
        ParamsVal := ParamsVal + St;
        Code := SQLParser.GetNext(St);
      end;

      UsedConnection.Disconnect;
      SetDatabase(UsedConnection, DatabaseVal);
      CreateDatabase(UsedConnection, ParamsVal);
      Omit := True;
    end;

    ST_DROP_DATABASE: begin
      DropDatabase(UsedConnection);
      Omit := True;
    end;

    ST_COMMENT:
      Omit := True;

    ST_BATCH_START: begin
      FInBatch := True;
      SetLength(FBatchStatements, 0);
      Omit := True;
    end;

    ST_BATCH_EXECUTE: begin
      UsedTransaction := TDAScriptUtils.UsedTransaction(FOwner);
      if not UsedTransaction.Active then begin
        UsedTransaction.StartTransaction;
        ExplicitTrans := True;
      end
      else
        ExplicitTrans := False;

      TDBAccessUtils.CheckConnection(GetCommand);
      TGDSCommand(TDBAccessUtils.GetICommand(GetCommand)).ExecuteBatch(FBatchStatements);

      if FOwner.AutoCommit then begin
        if ExplicitTrans then
          UsedTransaction.Commit
        else
          TDBAccessUtils.CommitRetaining(UsedTransaction);
      end;

      FInBatch := False;
      SetLength(FBatchStatements, 0);
      Omit := True;
    end;
  end;

  if FAutoDDL and not FInBatch and (StatementType and not ST_SPECIFIC_SQL = ST_DDL) then begin
    FStoredDataSet := FOwner.DataSet;
    FOwner.DataSet := nil;
    FStoredTransaction := TDBAccessUtils.UsedTransaction(GetCommand);
    TDBAccessUtils.SetTransaction(GetCommand, FDDLTransaction);
  end;
end;


procedure TCustomIBCScriptProcessor.DoAfterStatementExecute(var SQL: _string; StatementType: integer);
begin

  if FAutoDDL and not FInBatch and (StatementType and not ST_SPECIFIC_SQL = ST_DDL) and not FOwner.AutoCommit then
    try
      try
        FDDLTransaction.Commit;
      except
        if FDDLTransaction.Active then
          FDDLTransaction.Rollback;
        raise;
      end;
    finally
      FOwner.DataSet := FStoredDataSet;
      TDBAccessUtils.SetTransaction(GetCommand, FStoredTransaction);
    end;

  inherited;
end;

end.
 
