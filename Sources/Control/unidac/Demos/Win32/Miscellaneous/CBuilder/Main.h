//---------------------------------------------------------------------------
#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "MemDS.hpp"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <DBCtrls.hpp>
#include <DBGrids.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include "DBAccess.hpp"
#include <DBTables.hpp>
#include <DB.hpp>
#include "Uni.hpp"
#include "UniDacVcl.hpp"
//---------------------------------------------------------------------------
class TfmMain : public TForm
{
__published:    // IDE-managed Components
    TPanel *ToolBar;
    TButton *btOpen;
    TButton *btClose;
    TButton *btExecute;
    TDBNavigator *DBNavigator;
    TMemo *meSQL;
    TDBGrid *DBGrid;
    TDataSource *DataSource;
    TButton *btDisconnect;
    TSplitter *Splitter1;
    TButton *btConnect;
    TUniConnection *UniConnection;
    TUniConnectDialog *UniConnectDialog;
    TUniQuery *UniQuery;
    void __fastcall btOpenClick(TObject *Sender);
    void __fastcall btCloseClick(TObject *Sender);
    void __fastcall btDisconnectClick(TObject *Sender);
    void __fastcall btExecuteClick(TObject *Sender);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall meSQLExit(TObject *Sender);
        void __fastcall btConnectClick(TObject *Sender);
private:        // User declarations
public:         // User declarations
    __fastcall TfmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmMain *fmMain;
//---------------------------------------------------------------------------
#endif
