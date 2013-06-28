// CodeGear C++Builder
// Copyright (c) 1995, 2008 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Oranetuni.pas' rev: 20.00

#ifndef OranetuniHPP
#define OranetuniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Memutils.hpp>	// Pascal unit
#include <Clrclasses.hpp>	// Pascal unit
#include <Scktcomp.hpp>	// Pascal unit
#include <Winsock.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Rtlconsts.hpp>	// Pascal unit
#include <Oracalluni.hpp>	// Pascal unit
#include <Oracryptuni.hpp>	// Pascal unit
#include <Oraparseruni.hpp>	// Pascal unit
#include <Crparser.hpp>	// Pascal unit
#include <Dateutils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Oranetuni
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS N_1;
class PASCALIMPLEMENTATION N_1 : public Clrclasses::Encoding
{
	typedef Clrclasses::Encoding inherited;
	
private:
	bool N_2;
	
public:
	__fastcall N_1(void);
	virtual Sysutils::TBytes __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int N_3, Sysutils::TBytes &bytes, int byteIndex)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int N_3, Sysutils::TBytes &bytes, int byteIndex)/* overload */;
	virtual Sysutils::TBytes __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual System::WideString __fastcall GetWideString(const Sysutils::TBytes bytes, int index, int count)/* overload */;
	__classmethod N_1* __fastcall N_4();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~N_1(void) { }
	
	
/* Hoisted overloads: */
	
public:
	inline System::WideString __fastcall  GetWideString(const Sysutils::TBytes bytes){ return Clrclasses::Encoding::GetWideString(bytes); }
	
};


class DELPHICLASS N_5;
class PASCALIMPLEMENTATION N_5 : public Clrclasses::UnicodeEncoding
{
	typedef Clrclasses::UnicodeEncoding inherited;
	
public:
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int N_3, Sysutils::TBytes &bytes, int byteIndex)/* overload */;
	virtual System::WideString __fastcall GetWideString(const Sysutils::TBytes bytes, int index, int count)/* overload */;
	virtual Sysutils::TBytes __fastcall GetBytes(const System::WideString chars)/* overload */;
	__classmethod N_5* __fastcall N_4();
public:
	/* TObject.Create */ inline __fastcall N_5(void) : Clrclasses::UnicodeEncoding() { }
	/* TObject.Destroy */ inline __fastcall virtual ~N_5(void) { }
	
	
/* Hoisted overloads: */
	
public:
	inline Sysutils::TBytes __fastcall  GetBytes(const System::AnsiString chars){ return Clrclasses::UnicodeEncoding::GetBytes(chars); }
	inline int __fastcall  GetBytes(const System::WideString chars, int charIndex, int charCount, Sysutils::TBytes &bytes, int byteIndex){ return Clrclasses::UnicodeEncoding::GetBytes(chars, charIndex, charCount, bytes, byteIndex); }
	inline System::WideString __fastcall  GetWideString(const Sysutils::TBytes bytes){ return Clrclasses::UnicodeEncoding::GetWideString(bytes); }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool UseDirectLobs;
extern PACKAGE int SDU;
extern PACKAGE int TDU;
extern PACKAGE void __fastcall InitNet(void);
extern PACKAGE void __fastcall LoadNet(void);
extern PACKAGE void __fastcall FreeNet(void);

}	/* namespace Oranetuni */
using namespace Oranetuni;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OranetuniHPP
