{******************************************}
{                                          }
{             FastScript v1.9              }
{        DAC classes and functions         }
{                                          }
{          Created by: Devart              }
{        E-mail: support@devart.com        }
{                                          }
{******************************************}

unit fs_idacrtti;

interface

{$i fs.inc}

uses
  SysUtils, Classes, fs_iinterpreter, fs_itools, fs_idbrtti, DB,
  MemData, MemDS, DBAccess;

type
  TfsNotifyEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject);
    function GetMethod: Pointer; override;
  end;

  TfsLoginEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; Username, Password: string);
    function GetMethod: Pointer; override;
  end;

  TfsUpdateErrorEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    function GetMethod: Pointer; override;
  end;

  TfsUpdateRecordEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(DataSet: TDataSet; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    function GetMethod: Pointer; override;
  end;

  TfsAfterExecuteEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; Result: boolean);
    function GetMethod: Pointer; override;
  end;
  
  TfsUpdateExecuteEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TDataSet; StatementTypes: TStatementTypes; Params: TDAParams);
    function GetMethod: Pointer; override;
  end;
  
implementation

type
  TfsDAConnectionErrorEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; E: EDAError; var Fail: boolean);
    function GetMethod: Pointer; override;
  end;

  TfsConnectionLostEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause; var RetryMode: TRetryMode);
    function GetMethod: Pointer; override;
  end;

  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TfsDAConnectionErrorEvent }

procedure TfsDAConnectionErrorEvent.DoEvent(Sender: TObject; E: EDAError; var Fail: boolean);
begin
  CallHandler([Sender, E, Fail]);
  Fail := Handler.Params[2].Value;
end;

function TfsDAConnectionErrorEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsNotifyEvent }

procedure TfsNotifyEvent.DoEvent(Sender: TObject);
begin
  CallHandler([Sender]);
end;

function TfsNotifyEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsLoginEvent }

procedure TfsLoginEvent.DoEvent(Sender: TObject; Username, Password: string);
begin
  CallHandler([Sender, Username, Password]);
end;

function TfsLoginEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsConnectionLostEvent }

procedure TfsConnectionLostEvent.DoEvent(Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause; var RetryMode: TRetryMode);
begin
  CallHandler([Sender, Component, Integer(ConnLostCause), Integer(RetryMode)]);
  RetryMode := Handler.Params[3].Value;
end;

function TfsConnectionLostEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsUpdateErrorEvent }

procedure TfsUpdateErrorEvent.DoEvent(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  CallHandler([DataSet, E, Integer(UpdateKind), Integer(UpdateAction)]);
  UpdateAction := Handler.Params[3].Value;
end;

function TfsUpdateErrorEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsUpdateRecordEvent }

procedure TfsUpdateRecordEvent.DoEvent(DataSet: TDataSet; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  CallHandler([DataSet, Integer(UpdateKind), Integer(UpdateAction)]);
  UpdateAction := Handler.Params[3].Value;
end;

function TfsUpdateRecordEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsAfterExecuteEvent }

procedure TfsAfterExecuteEvent.DoEvent(Sender: TObject; Result: boolean);
begin
  CallHandler([Sender, Result]);
end;

function TfsAfterExecuteEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TfsUpdateExecuteEvent }

procedure TfsUpdateExecuteEvent.DoEvent(Sender: TDataSet; StatementTypes: TStatementTypes; Params: TDAParams);
begin
  CallHandler([Sender, 0{StatementTypes}, Params]);
end;

function TfsUpdateExecuteEvent.GetMethod: Pointer;
begin
  Result := @TfsFieldGetTextEvent.DoEvent;
end;

{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  
  with AScript do
  begin
    with AddClass(TCustomDAConnection, 'TComponent') do begin
      AddMethod('procedure Open', CallMethod);
      AddMethod('procedure Close', CallMethod);
      
      AddMethod('procedure Connect', CallMethod);
      AddMethod('procedure Disconnect', CallMethod);
      
      AddMethod('function ExecSQL(Text: string; const Params: array of variant): variant', CallMethod);
      
      AddMethod('procedure GetTableNames(List: TStrings)', CallMethod);
      AddMethod('procedure GetDatabaseNames(List: TStrings)', CallMethod);
      AddMethod('procedure GetStoredProcNames(List: TStrings)', CallMethod);

      AddMethod('procedure StartTransaction', CallMethod);
      AddMethod('procedure Commit', CallMethod);
      AddMethod('procedure Rollback', CallMethod);

      AddMethod('procedure ApplyUpdates', CallMethod);
      //AddMethod('procedure ApplyUpdates(DataSets: array of TCustomDADataSet)', CallMethod);
      
      AddMethod('function CreateDataSet: TCustomDADataSet', CallMethod);

      AddMethod('procedure RemoveFromPool', CallMethod);
      AddMethod('procedure MonitorMessage(const Msg: string)', CallMethod);
      
      AddProperty('InTransaction', 'boolean', GetProp);
      
      AddIndexProperty('DataSets', 'Integer', 'TDataSet', CallMethod, True);
      AddProperty('DataSetCount', 'Integer', GetProp);

      AddEvent('OnError', TfsDAConnectionErrorEvent);
      AddEvent('OnConnectionLost', TfsConnectionLostEvent);
      AddEvent('AfterConnect', TfsNotifyEvent);
      AddEvent('BeforeConnect', TfsNotifyEvent);
      AddEvent('AfterDisconnect', TfsNotifyEvent);
      AddEvent('BeforeDisconnect', TfsNotifyEvent);
      AddEvent('OnLogin', TfsLoginEvent);
    end;
    AddClass(TDAConnectionOptions, 'TPersistent');
    AddClass(TPoolingOptions, 'TPersistent');

    AddEnum('TDANumericType', 'ntFloat, ntBCD'{$IFNDEF VER130} + ', ntFmtBCD'{$ENDIF});
    AddEnum('TConnLostCause', 'clUnknown, clExecute, clOpen, clRefresh, clApply, clServiceQuery, clTransStart, clConnectionApply, clConnect');
    AddEnum('TRetryMode', 'rmRaise, rmReconnect, rmReconnectExecute');

    AddEnum('TLabelSet', 'lsCustom, lsEnglish, lsFrench, lsGerman, lsItalian, lsPolish, lsPortuguese, lsRussian, lsSpanish');
   
    with AddClass(TCustomConnectDialog, 'TComponent') do begin
      AddMethod('function Execute: boolean', CallMethod);
      AddMethod('procedure GetServerList(List: TStrings)', CallMethod);

      AddProperty('Connection', 'TCustomDAConnection', GetProp);
      AddProperty('Retries', 'word', GetProp, SetProp);
      AddProperty('SavePassword', 'boolean', GetProp, SetProp);
      AddProperty('StoreLogInfo', 'boolean', GetProp, SetProp);
      AddProperty('DialogClass', 'string', GetProp, SetProp);
      AddProperty('Caption', 'string', GetProp, SetProp);
      AddProperty('UsernameLabel', 'string', GetProp, SetProp);
      AddProperty('PasswordLabel', 'string', GetProp, SetProp);
      AddProperty('ServerLabel', 'string', GetProp, SetProp);
      AddProperty('ConnectButton', 'string', GetProp, SetProp);
      AddProperty('CancelButton', 'string', GetProp, SetProp);
      AddProperty('LabelSet', 'TLabelSet', GetProp, SetProp);
    end;

    with AddClass(TMemDataSet, 'TDataSet') do begin
      AddMethod('procedure Prepare', CallMethod);
      AddMethod('procedure UnPrepare', CallMethod);
      AddMethod('procedure CheckPrepared', CallMethod); 

      AddMethod('function UpdateStatus: TUpdateStatus', CallMethod); 
      AddMethod('function UpdateResult: TUpdateAction', CallMethod);
      AddMethod('procedure ApplyUpdates', CallMethod);
      AddMethod('procedure CommitUpdates', CallMethod);
      AddMethod('procedure CancelUpdates', CallMethod);
      AddMethod('procedure RestoreUpdates', CallMethod);
      AddMethod('procedure RevertRecord', CallMethod);

      AddMethod('procedure SaveToXML(const FileName: string)', CallMethod);

      AddProperty('Prepared', 'boolean', GetProp, SetProp);
      AddProperty('RecordCount', 'Integer', GetProp);
      
      AddEvent('OnUpdateError', TfsUpdateErrorEvent);
      AddEvent('OnUpdateRecord', TfsUpdateRecordEvent);
    end;
    AddEnum('TUpdateStatus', 'usUnmodified, usModified, usInserted, usDeleted');
    AddEnum('TUpdateAction', 'uaFail, uaAbort, uaSkip, uaRetry, uaApplied');

    with AddClass(TCustomDADataSet, 'TMemDataSet') do begin
      AddMethod('procedure Execute', CallMethod);
      AddMethod('function Executing', CallMethod);
      AddMethod('function Fetching', CallMethod);

      AddMethod('procedure RefreshRecord', CallMethod);

      AddMethod('function FindMacro(const Value: string): TMacro', CallMethod);
      AddMethod('function MacroByName(const Value: string): TMacro', CallMethod);

      AddMethod('procedure SaveSQL', CallMethod);
      AddMethod('procedure RestoreSQL', CallMethod);
      AddMethod('function SQLSaved: boolean', CallMethod);

      AddMethod('procedure AddWhere(Condition: string)', CallMethod);
      AddMethod('procedure DeleteWhere', CallMethod);
      AddMethod('procedure SetOrderBy(Fields: string)', CallMethod);
      AddMethod('function GetOrderBy: string', CallMethod);

      AddEvent('AfterExecute', TfsAfterExecuteEvent);
      AddEvent('BeforeUpdateExecute', TfsUpdateExecuteEvent);
      AddEvent('AfterUpdateExecute', TfsUpdateExecuteEvent);
    end;

    AddClass(TDAParams, 'TParams');
    AddEnum('TStatementType', 'stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh, stCheck, stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate');
    AddEnumSet('TStatementTypes', 'stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh, stCheck, stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate');

    AddClass(TDADataSetOptions, 'TPersistent');
    AddEnum('TCompressBlobMode', 'cbNone, cbClient, cbServer, cbClientServer');

    with AddClass(TMacro, 'TCollectionItem') do begin
      AddProperty('AsDateTime', 'TDateTime', GetProp, SetProp);
      AddProperty('AsFloat', 'double', GetProp, SetProp);
      AddProperty('AsInteger', 'integer', GetProp, SetProp);
      AddProperty('AsString', 'string', GetProp, SetProp);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  DAConnection: TCustomDAConnection;
  CustomConnectDialog: TCustomConnectDialog;
  MemDataSet: TMemDataSet;
  CustomDADataSet: TCustomDADataSet;
begin
  Result := 0;

  if ClassType = TCustomDAConnection then begin
    DAConnection := TCustomDAConnection(Instance);
    if MethodName = 'OPEN' then
      DAConnection.Open
    else
    if MethodName = 'CLOSE' then
      DAConnection.Close
    else
    if MethodName = 'CONNECT' then
      DAConnection.Connect
    else
    if MethodName = 'DISCONNECT' then
      DAConnection.Disconnect
    else
    if MethodName = 'EXECSQL' then
      Result := DAConnection.ExecSQL(Caller.Params[0], [Caller.Params[1]])
    else
    if MethodName = 'GETTABLENAMES' then
      DAConnection.GetTableNames(TStrings(Integer(Caller.Params[0])))
    else
    if MethodName = 'GETDATABASENAMES' then
      DAConnection.GetDatabaseNames(TStrings(Integer(Caller.Params[0])))
    else
    if MethodName = 'GETSTOREDPROCNAMES' then
      DAConnection.GetStoredProcNames(TStrings(Integer(Caller.Params[0])))
    else
    if MethodName = 'STARTTRANSACTION' then
      DAConnection.StartTransaction
    else
    if MethodName = 'COMMIT' then
      DAConnection.Commit
    else
    if MethodName = 'ROLLBACK' then
      DAConnection.Rollback
    else
    if MethodName = 'APPLYUPDATES' then
      DAConnection.ApplyUpdates
    else
    if MethodName = 'CREATEDATASET' then
      Result := Integer(DAConnection.CreateDataSet)
    else
    if MethodName = 'REMOVEFROMPOOL' then
      DAConnection.RemoveFromPool
    else
    if MethodName = 'MONITORMESSAGE' then
      DAConnection.MonitorMessage(Caller.Params[0])
    else
    if MethodName = 'DATASETS.GET' then
      Result := Integer(DAConnection.DataSets[Caller.Params[0]])
  end
  else
  if ClassType = TCustomConnectDialog then begin
    CustomConnectDialog := TCustomConnectDialog(Instance);
    if MethodName = 'EXECUTE' then
      Result := Integer(CustomConnectDialog.Execute)
    else
    if MethodName = 'GETSERVERLIST' then
      CustomConnectDialog.GetServerList(TStrings(Integer(Caller.Params[0])))
  end
  else
  if ClassType = TMemDataSet then begin
    MemDataSet := TMemDataSet(Instance);
    if MethodName = 'PREPARE' then
      MemDataSet.Prepare
    else
    if MethodName = 'UNPREPARE' then
      MemDataSet.UnPrepare
    else
    if MethodName = 'CHECKPREPARED' then
      MemDataSet.CheckPrepared
    else
    if MethodName = 'UPDATESTATUS' then
      Result := Integer(MemDataSet.UpdateStatus)
    else
    if MethodName = 'UPDATERESULT' then
      Result := Integer(MemDataSet.UpdateResult)
    else
    if MethodName = 'APPLYUPDATES' then
      MemDataSet.ApplyUpdates
    else
    if MethodName = 'COMMITUPDATES' then
      MemDataSet.CommitUpdates
    else
    if MethodName = 'CANCELUPDATES' then
      MemDataSet.CancelUpdates
    else
    if MethodName = 'RESTOREUPDATES' then
      MemDataSet.RestoreUpdates
    else
    if MethodName = 'REVERTRECORD' then
      MemDataSet.RevertRecord
    else
    if MethodName = 'SAVETOXML' then
      MemDataSet.SaveToXML(String(Caller.Params[0]))
  end
  else
  if ClassType = TCustomDADataSet then begin
    CustomDADataSet := TCustomDADataSet(Instance);
    if MethodName = 'EXECUTE' then
      CustomDADataSet.Execute
    else
    if MethodName = 'EXECUTING' then
      Result := CustomDADataSet.Executing
    else
    if MethodName = 'FETCHING' then
      Result := CustomDADataSet.Fetching
    else
    if MethodName = 'REFRESHRECORD' then
      CustomDADataSet.RefreshRecord
    else
    if MethodName = 'FINDMACRO' then
      Result := Integer(CustomDADataSet.FindMacro(String(Caller.Params[0])))
    else
    if MethodName = 'MACROBYNAME' then
      Result := Integer(CustomDADataSet.MacroByName(String(Caller.Params[0])))
    else
    if MethodName = 'SAVESQL' then
      CustomDADataSet.SaveSQL
    else
    if MethodName = 'RESTORESQL' then
      CustomDADataSet.RestoreSQL
    else
    if MethodName = 'SQLSAVED' then
      Result := CustomDADataSet.SQLSaved
    else
    if MethodName = 'ADDWHERE' then
      CustomDADataSet.AddWhere(String(Caller.Params[0]))
    else
    if MethodName = 'DELETEWHERE' then
      CustomDADataSet.DeleteWhere
    else
    if MethodName = 'SETORDERBY' then
      CustomDADataSet.SetOrderBy(String(Caller.Params[0]))
    else
    if MethodName = 'GETORDERBY' then
      Result := CustomDADataSet.GetOrderBy;
  end;
end;


function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
var
  CustomDAConnection: TCustomDAConnection;
  CustomConnectDialog: TCustomConnectDialog;
begin
  Result := 0;

  if ClassType = TCustomDAConnection then begin
    CustomDAConnection := TCustomDAConnection(Instance);
    if PropName = 'INTRANSACTION' then
      Result := CustomDAConnection.InTransaction
    else
    if PropName = 'DATASETCOUNT' then
      Result := CustomDAConnection.DataSetCount;
  end
  else
  if ClassType = TCustomConnectDialog then begin
    CustomConnectDialog := TCustomConnectDialog(Instance);
    if PropName = 'CONNECTION' then
      Result := Integer(CustomConnectDialog.Connection)
    else
    if PropName = 'RETRIES' then
      Result := CustomConnectDialog.Retries
    else
    if PropName = 'SAVEPASSWORD' then
      Result := CustomConnectDialog.SavePassword
    else
    if PropName = 'STORELOGINFO' then
      Result := CustomConnectDialog.StoreLogInfo
    else
    if PropName = 'DIALOGCLASS' then
      Result := CustomConnectDialog.DialogClass
    else
    if PropName = 'CAPTION' then
      Result := CustomConnectDialog.Caption
    else
    if PropName = 'USERNAMELABEL' then
      Result := CustomConnectDialog.UsernameLabel
    else
    if PropName = 'PASSWORDLABEL' then
      Result := CustomConnectDialog.PasswordLabel
    else
    if PropName = 'SERVERLABEL' then
      Result := CustomConnectDialog.ServerLabel
    else
    if PropName = 'CONNECTBUTTON' then
      Result := CustomConnectDialog.ConnectButton
    else
    if PropName = 'CANCELBUTTON' then
      Result := CustomConnectDialog.CancelButton
    else
    if PropName = 'LABELSET' then
      Result := Integer(CustomConnectDialog.LabelSet)
  end
  else
  if ClassType = TMemDataSet then begin
    if PropName = 'PREPARED' then
      Result := TMemDataSet(Instance).Prepared
    else
    if PropName = 'RECORDCOUNT' then
      Result := TMemDataSet(Instance).RecordCount;
  end
  else
  if ClassType = TMacro then begin
    if PropName = 'ASDATETIME' then
      Result := TMacro(Instance).AsDateTime
    else
    if PropName = 'ASFLOAT' then
      Result := TMacro(Instance).AsFloat
    else
    if PropName = 'ASINTEGER' then
      Result := TMacro(Instance).AsInteger
    else
    if PropName = 'ASSTRING' then
      Result := TMacro(Instance).AsString;
  end;
end;

procedure TFunctions.SetProp(Instance: TObject; ClassType: TClass;
  const PropName: String; Value: Variant);
var
  CustomConnectDialog: TCustomConnectDialog;
begin
  if ClassType = TCustomConnectDialog then begin
    CustomConnectDialog := TCustomConnectDialog(Instance);
    if PropName = 'RETRIES' then
      CustomConnectDialog.Retries := Value
    else
    if PropName = 'SAVEPASSWORD' then
      CustomConnectDialog.SavePassword := Value
    else
    if PropName = 'STORELOGINFO' then
      CustomConnectDialog.StoreLogInfo := Value
    else
    if PropName = 'DIALOGCLASS' then
      CustomConnectDialog.DialogClass := Value
    else
    if PropName = 'CAPTION' then
      CustomConnectDialog.Caption := Value
    else
    if PropName = 'USERNAMELABEL' then
      CustomConnectDialog.UsernameLabel := Value
    else
    if PropName = 'PASSWORDLABEL' then
      CustomConnectDialog.PasswordLabel := Value
    else
    if PropName = 'SERVERLABEL' then
      CustomConnectDialog.ServerLabel := Value
    else
    if PropName = 'CONNECTBUTTON' then
      CustomConnectDialog.ConnectButton := Value
    else
    if PropName = 'CANCELBUTTON' then
      CustomConnectDialog.CancelButton := Value
    else
    if PropName = 'LABELSET' then
      CustomConnectDialog.LabelSet := TLabelSet(Value);
  end
  else
  if ClassType = TMemDataSet then begin
    if PropName = 'PREPARED' then
      TMemDataSet(Instance).Prepared := Value; 
  end
  else
  if ClassType = TMacro then begin
    if PropName = 'ASDATETIME' then
      TMacro(Instance).AsDateTime := Value
    else
    if PropName = 'ASFLOAT' then
      TMacro(Instance).AsFloat := Value
    else
    if PropName = 'ASINTEGER' then
      TMacro(Instance).AsInteger := Value
    else
    if PropName = 'ASSTRING' then
      TMacro(Instance).AsString := Value;
  end;
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.

