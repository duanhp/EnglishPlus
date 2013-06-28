//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("CBuilder.res");
USEFORM("Main.cpp", fmMain);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
        Application->Initialize();
        Application->CreateForm(__classid(TfmMain), &fmMain);
        Application->Run();
    }
    catch (Exception &exception)
    {
        Application->ShowException(&exception);
    }
    return 0;
}
