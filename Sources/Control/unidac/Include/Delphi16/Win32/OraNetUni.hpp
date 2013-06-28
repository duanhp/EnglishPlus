// CodeGear C++Builder
// Copyright (c) 1995, 2011 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OraNetUni.pas' rev: 23.00 (Win32)

#ifndef OranetuniHPP
#define OranetuniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <MemUtils.hpp>	// Pascal unit
#include <CLRClasses.hpp>	// Pascal unit
#include <System.Win.ScktComp.hpp>	// Pascal unit
#include <Winapi.WinSock.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.RTLConsts.hpp>	// Pascal unit
#include <OraCallUni.hpp>	// Pascal unit
#include <OraCryptUni.hpp>	// Pascal unit
#include <OraParserUni.hpp>	// Pascal unit
#include <CRParser.hpp>	// Pascal unit
#include <System.DateUtils.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Oranetuni
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS N_1;
#pragma pack(push,4)
class PASCALIMPLEMENTATION N_1 : public Clrclasses::Encoding
{
	typedef Clrclasses::Encoding inherited;
	
private:
	bool N_2;
	
public:
	__fastcall N_1(void);
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::AnsiString chars)/* overload */;
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int N_3, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual int __fastcall GetBytes(const System::WideString chars, int charIndex, int N_3, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	__classmethod N_1* __fastcall N_4();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~N_1(void) { }
	
/* Hoisted overloads: */
	
public:
	inline System::WideString __fastcall  GetWideString(const System::DynamicArray<System::Byte> bytes){ return Clrclasses::Encoding::GetWideString(bytes); }
	
};

#pragma pack(pop)

class DELPHICLASS N_5;
#pragma pack(push,4)
class PASCALIMPLEMENTATION N_5 : public Clrclasses::UnicodeEncoding
{
	typedef Clrclasses::UnicodeEncoding inherited;
	
public:
	virtual int __fastcall GetBytes(const System::AnsiString chars, int charIndex, int N_3, System::DynamicArray<System::Byte> &bytes, int byteIndex)/* overload */;
	virtual System::WideString __fastcall GetWideString(const System::DynamicArray<System::Byte> bytes, int index, int count)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall GetBytes(const System::WideString chars)/* overload */;
	__classmethod N_5* __fastcall N_4();
public:
	/* TObject.Create */ inline __fastcall N_5(void) : Clrclasses::UnicodeEncoding() { }
	/* TObject.Destroy */ inline __fastcall virtual ~N_5(void) { }
	
/* Hoisted overloads: */
	
public:
	inline System::DynamicArray<System::Byte> __fastcall  GetBytes(const System::AnsiString chars){ return Clrclasses::UnicodeEncoding::GetBytes(chars); }
	inline int __fastcall  GetBytes(const System::WideString chars, int charIndex, int charCount, System::DynamicArray<System::Byte> &bytes, int byteIndex){ return Clrclasses::UnicodeEncoding::GetBytes(chars, charIndex, charCount, bytes, byteIndex); }
	inline System::WideString __fastcall  GetWideString(const System::DynamicArray<System::Byte> bytes){ return Clrclasses::UnicodeEncoding::GetWideString(bytes); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool UseDirectLobs;
extern PACKAGE int SDU;
extern PACKAGE int TDU;
extern PACKAGE void __fastcall InitNet(void);
extern PACKAGE void __fastcall LoadNet(void);
extern PACKAGE void __fastcall FreeNet(void);

}	/* namespace Oranetuni */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ORANETUNI)
using namespace Oranetuni;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OranetuniHPP
