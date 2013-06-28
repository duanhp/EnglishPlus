//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ToolWin.hpp>
#include "MemDS.hpp"
#include "VirtualTable.hpp"
#include <DB.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TToolBar *ToolBar;
        TButton *btOpen;
        TButton *btClose;
        TButton *btAutoFill;
        TButton *btAddField;
        TButton *btDelField;
        TDBNavigator *DBNavigator;
        TPanel *Panel1;
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label5;
        TEdit *edField;
        TEdit *edValue;
        TButton *btLocate;
        TEdit *edFilter;
        TCheckBox *cbFiltered;
        TButton *btLoad;
        TButton *btSave;
        TButton *btClear;
        TDBGrid *DBGrid;
        TDBMemo *DBMemo;
        TSplitter *Splitter;
        TStatusBar *StatusBar;
        TVirtualTable *VirtualTable;
        TDataSource *DataSource;
        TSaveDialog *SaveDialog;
        TOpenDialog *OpenDialog;
        void __fastcall btOpenClick(TObject *Sender);
        void __fastcall btCloseClick(TObject *Sender);
        void __fastcall btAutoFillClick(TObject *Sender);
        void __fastcall btAddFieldClick(TObject *Sender);
        void __fastcall btDelFieldClick(TObject *Sender);
        void __fastcall btLoadClick(TObject *Sender);
        void __fastcall btSaveClick(TObject *Sender);
        void __fastcall btClearClick(TObject *Sender);
        void __fastcall cbFilteredClick(TObject *Sender);
        void __fastcall btLocateClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
