//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "MemDS"
#pragma link "VirtualTable"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btOpenClick(TObject *Sender)
{
  VirtualTable->Open();
  edField->Text = VirtualTable->Fields->Fields[0]->FieldName;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btCloseClick(TObject *Sender)
{
  VirtualTable->Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btAutoFillClick(TObject *Sender)
{
  TField* Field = VirtualTable -> FindField("STRING1");
  for( int i = 1; i <= 100; i++ ){
      VirtualTable -> Append();
      VirtualTable -> FieldByName("NUMBER")->AsInteger = i;
      VirtualTable -> FieldByName("STRING")->AsString = "Use Oracle Data Access from Devart !!! (" + IntToStr(i) + ")";
      VirtualTable -> FieldByName("DATE")->AsDateTime = Date();
      VirtualTable -> FieldByName("MEMO")->AsString = "Memo value (" + IntToStr(i) + ")";

      if( Field != NULL )
        VirtualTable -> FieldByName("STRING1")->AsString = "TVirtualTable";

      VirtualTable -> Post();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btAddFieldClick(TObject *Sender)
{
  VirtualTable -> AddField("STRING1", ftString, 30);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btDelFieldClick(TObject *Sender)
{
  VirtualTable->DeleteField("STRING1");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btLoadClick(TObject *Sender)
{
  if( OpenDialog->Execute() )
    VirtualTable->LoadFromFile(OpenDialog->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btSaveClick(TObject *Sender)
{
  if( SaveDialog->Execute() )
    VirtualTable->SaveToFile(SaveDialog->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btClearClick(TObject *Sender)
{
  VirtualTable->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cbFilteredClick(TObject *Sender)
{
  VirtualTable->Filtered = cbFiltered->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btLocateClick(TObject *Sender)
{
  VirtualTable->Locate(edField->Text, Variant(edValue->Text), TLocateOptions());
}
//---------------------------------------------------------------------------

