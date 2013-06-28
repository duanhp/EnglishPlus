// CodeGear C++Builder
// Copyright (c) 1995, 2011 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'OraCryptUni.pas' rev: 23.00 (Win32)

#ifndef OracryptuniHPP
#define OracryptuniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <CLRClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Oracryptuni
{
//-- type declarations -------------------------------------------------------
typedef System::DynamicArray<System::Int8> TShortArr;

typedef System::DynamicArray<unsigned> TLongArr;

typedef System::DynamicArray<System::Byte> Oracryptuni__1;

typedef System::DynamicArray<System::DynamicArray<System::Byte> > TByteArrArr;

typedef System::DynamicArray<unsigned> Oracryptuni__2;

typedef System::DynamicArray<System::DynamicArray<unsigned> > TLongArrArr;

typedef System::DynamicArray<System::Byte> _dt_Oracryptuni_1;
typedef System::DynamicArray<System::Byte> _dt_Oracryptuni_2;
typedef _dt_Oracryptuni_1 __fastcall (__closure *TCryptFunc)(_dt_Oracryptuni_2 Data);

class DELPHICLASS TMD5;
class PASCALIMPLEMENTATION TMD5 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__int64 FDataSize;
	unsigned FBufferIndex;
	System::StaticArray<unsigned, 4> FDigest;
	System::StaticArray<unsigned, 16> FBuffer;
	void __fastcall ClearBuffer(void);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TMD5(void);
	void __fastcall Reset(void);
	void __fastcall Update(const System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall Final(void);
	System::DynamicArray<System::Byte> __fastcall ComputeHash(const System::DynamicArray<System::Byte> Data);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TMD5(void) { }
	
};


class DELPHICLASS TSHA1;
class PASCALIMPLEMENTATION TSHA1 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__int64 FDataSize;
	unsigned FBufferIndex;
	System::StaticArray<unsigned, 5> FDigest;
	System::StaticArray<unsigned, 80> FBuffer;
	void __fastcall ClearBuffer(void);
	unsigned __fastcall SwapDWord(const unsigned Value);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TSHA1(void);
	void __fastcall Update(const System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall Final(void);
	void __fastcall Reset(void);
	System::DynamicArray<System::Byte> __fastcall ComputeHash(const System::DynamicArray<System::Byte> Data);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSHA1(void) { }
	
};


class DELPHICLASS TOraDESBase;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOraDESBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DynamicArray<System::Byte> FIV;
	// System::TArray__1<System::Byte>  FIV;
	
protected:
	void __fastcall SetIV(System::DynamicArray<System::Byte> IV);
	System::DynamicArray<System::Byte> __fastcall XorBlock(System::DynamicArray<System::Byte> Vector, System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall InternalEncryptCBC(System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall InternalDecryptCBC(System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall InternalEncryptCBC8byte(System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall InternalDecryptCBC8byte(System::DynamicArray<System::Byte> Data);
	virtual System::DynamicArray<System::Byte> __fastcall InternalEncryptECB(System::DynamicArray<System::Byte> Data) = 0 ;
	virtual System::DynamicArray<System::Byte> __fastcall InternalDecryptECB(System::DynamicArray<System::Byte> Data) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TOraDESBase(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDESBase(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TOraDES;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOraDES : public TOraDESBase
{
	typedef TOraDESBase inherited;
	
private:
	TLongArr FEKey;
	TLongArr FDKey;
	
protected:
	TLongArr __fastcall deskey(System::DynamicArray<System::Byte> Key, bool edf);
	TLongArr __fastcall cookey(TLongArr raw1);
	System::DynamicArray<System::Byte> __fastcall des_enc(System::DynamicArray<System::Byte> Data, int Blocks);
	System::DynamicArray<System::Byte> __fastcall des_dec(System::DynamicArray<System::Byte> Data, int Blocks);
	void __fastcall scrunch(System::DynamicArray<System::Byte> outof, TLongArr into);
	void __fastcall unscrunch(TLongArr outof, System::DynamicArray<System::Byte> into);
	void __fastcall desfunc(TLongArr block, TLongArr keys);
	virtual System::DynamicArray<System::Byte> __fastcall InternalEncryptECB(System::DynamicArray<System::Byte> Data);
	virtual System::DynamicArray<System::Byte> __fastcall InternalDecryptECB(System::DynamicArray<System::Byte> Data);
	
public:
	__fastcall TOraDES(void);
	virtual void __fastcall SetKey(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> IV);
	System::DynamicArray<System::Byte> __fastcall EncryptDES(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall DecryptDES(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data);
	System::DynamicArray<System::Byte> __fastcall EncryptBlocks(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data, System::DynamicArray<System::Byte> IV);
	System::DynamicArray<System::Byte> __fastcall DecryptBlocks(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data, System::DynamicArray<System::Byte> IV);
	System::DynamicArray<System::Byte> __fastcall EncryptBlocks8byte(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data, System::DynamicArray<System::Byte> IV);
	System::DynamicArray<System::Byte> __fastcall DecryptBlocks8byte(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> Data, System::DynamicArray<System::Byte> IV);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDES(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TOra3DES;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOra3DES : public TOraDES
{
	typedef TOraDES inherited;
	
private:
	typedef System::DynamicArray<System::DynamicArray<System::Byte> > _TOra3DES__1;
	
	
private:
	_TOra3DES__1 FTripleKey;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall InternalEncryptECB(System::DynamicArray<System::Byte> Data);
	virtual System::DynamicArray<System::Byte> __fastcall InternalDecryptECB(System::DynamicArray<System::Byte> Data);
	
public:
	virtual void __fastcall SetKey(System::DynamicArray<System::Byte> Key, System::DynamicArray<System::Byte> IV);
public:
	/* TOraDES.Create */ inline __fastcall TOra3DES(void) : TOraDES() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOra3DES(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TOraAES;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TOraAES : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FParam1;
	int FParam2;
	int FParam3;
	int FParam4;
	System::Byte FRounds;
	TLongArrArr FDKey;
	TLongArrArr FEKey;
	unsigned __fastcall RowToInt(TByteArrArr InData, int RowIndex);
	void __fastcall IntToRow(TByteArrArr InData, unsigned InParam1, unsigned InParam2);
	unsigned __fastcall BytesToInt(System::DynamicArray<System::Byte> InData, int Offset);
	System::Byte __fastcall IntToByte(unsigned Value, int ByteIndex);
	
protected:
	void __fastcall InitKey(System::DynamicArray<System::Byte> Key);
	System::DynamicArray<System::Byte> __fastcall EncryptECB(System::DynamicArray<System::Byte> InData);
	System::DynamicArray<System::Byte> __fastcall DecryptECB(System::DynamicArray<System::Byte> InData);
	
public:
	__fastcall TOraAES(int Param1, int Param2, int Param3)/* overload */;
	__fastcall TOraAES(int Param1, int Param2, int Param3, int Param4)/* overload */;
	void __fastcall Init(System::DynamicArray<System::Byte> Key)/* overload */;
	System::DynamicArray<System::Byte> __fastcall Decrypt(System::DynamicArray<System::Byte> InData);
	System::DynamicArray<System::Byte> __fastcall Encrypt(System::DynamicArray<System::Byte> InData);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOraAES(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::Word __fastcall GetNormCharCode1(System::Word CharCode);
extern PACKAGE System::Word __fastcall GetNormCharCode2(System::Word CharCode);

}	/* namespace Oracryptuni */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ORACRYPTUNI)
using namespace Oracryptuni;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OracryptuniHPP
