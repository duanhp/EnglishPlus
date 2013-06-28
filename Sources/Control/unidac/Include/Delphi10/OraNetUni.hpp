// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Oranetuni.pas' rev: 10.00

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
	virtual Clrclasses::TBytes __fastcall GetBytes(const AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const AnsiString chars, int charIndex, int N_3, Clrclasses::TBytes &bytes, int byteIndex)/* overload */;
	virtual int __fastcall GetBytes(const WideString chars, int charIndex, int N_3, Clrclasses::TBytes &bytes, int byteIndex)/* overload */;
	virtual Clrclasses::TBytes __fastcall GetBytes(const WideString chars)/* overload */;
	virtual WideString __fastcall GetWideString(const Clrclasses::TBytes bytes, int index, int count)/* overload */;
	/*         class method */ static N_1* __fastcall N_4(TMetaClass* vmt);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~N_1(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
public:
	inline WideString __fastcall  GetWideString(const Clrclasses::TBytes bytes){ return Encoding::GetWideString(bytes); }
	
};


class DELPHICLASS N_5;
class PASCALIMPLEMENTATION N_5 : public Clrclasses::UnicodeEncoding 
{
	typedef Clrclasses::UnicodeEncoding inherited;
	
public:
	virtual int __fastcall GetBytes(const AnsiString chars, int charIndex, int N_3, Clrclasses::TBytes &bytes, int byteIndex)/* overload */;
	virtual WideString __fastcall GetWideString(const Clrclasses::TBytes bytes, int index, int count)/* overload */;
	virtual Clrclasses::TBytes __fastcall GetBytes(const WideString chars)/* overload */;
	/*         class method */ static N_5* __fastcall N_4(TMetaClass* vmt);
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall N_5(void) : Clrclasses::UnicodeEncoding() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~N_5(void) { }
	#pragma option pop
	
	
/* Hoisted overloads: */
	
public:
	inline Clrclasses::TBytes __fastcall  GetBytes(const AnsiString chars){ return UnicodeEncoding::GetBytes(chars); }
	inline int __fastcall  GetBytes(const WideString chars, int charIndex, int charCount, Clrclasses::TBytes &bytes, int byteIndex){ return UnicodeEncoding::GetBytes(chars, charIndex, charCount, bytes, byteIndex); }
	inline WideString __fastcall  GetWideString(const Clrclasses::TBytes bytes){ return UnicodeEncoding::GetWideString(bytes); }
	
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
#endif	// Oranetuni
