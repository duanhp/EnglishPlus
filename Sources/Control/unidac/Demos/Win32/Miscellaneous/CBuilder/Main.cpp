//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MemDS"
#pragma link "DBAccess"
#pragma link "Uni"
#pragma link "UniDacVcl"
#pragma link "OracleUniProvider"
#pragma link "SQLServerUniProvider"
#pragma link "InterBaseUniProvider"
#pragma link "MySQLUniProvider"

#pragma resource "*.dfm"
TfmMain *fmMain;
//---------------------------------------------------------------------------
__fastcall TfmMain::TfmMain(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::btOpenClick(TObject *Sender)
{
  UniQuery->Open();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::btCloseClick(TObject *Sender)
{
  UniQuery->Close();
}
//---------------------------------------------------------------------------
void __fastcall TfmMain::btDisconnectClick(TObject *Sender)
{
  UniConnection->Disconnect();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::btExecuteClick(TObject *Sender)
{
  UniQuery->Execute();
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::FormShow(TObject *Sender)
{
  meSQL->Lines->Assign(UniQuery->SQL);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::meSQLExit(TObject *Sender)
{
  if (meSQL->Lines->Text != UniQuery->SQL->Text)
    UniQuery->SQL->Assign(meSQL->Lines);
}
//---------------------------------------------------------------------------

void __fastcall TfmMain::btConnectClick(TObject *Sender)
{
  UniConnection->Connect();        
}
//---------------------------------------------------------------------------

