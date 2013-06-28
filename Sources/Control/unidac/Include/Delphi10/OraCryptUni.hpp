// Borland C++ Builder
// Copyright (c) 1995, 2005 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Oracryptuni.pas' rev: 10.00

#ifndef OracryptuniHPP
#define OracryptuniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Sysutils.hpp>	// Pascal unit
#include <Clrclasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Oracryptuni
{
//-- type declarations -------------------------------------------------------
typedef DynamicArray<Shortint >  TShortArr;

typedef DynamicArray<unsigned >  TLongArr;

typedef DynamicArray<Byte >  OraCryptUni__1;

typedef DynamicArray<DynamicArray<Byte > >  TByteArrArr;

typedef DynamicArray<unsigned >  OraCryptUni__2;

typedef DynamicArray<DynamicArray<unsigned > >  TLongArrArr;

typedef Clrclasses::TBytes __fastcall (__closure *TCryptFunc)(Clrclasses::TBytes Data);

class DELPHICLASS TMD5;
class PASCALIMPLEMENTATION TMD5 : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	__int64 FDataSize;
	unsigned FBufferIndex;
	unsigned FDigest[4];
	unsigned FBuffer[16];
	void __fastcall ClearBuffer(void);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TMD5(void);
	void __fastcall Reset(void);
	void __fastcall Update(const Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall Final();
	Clrclasses::TBytes __fastcall ComputeHash(const Clrclasses::TBytes Data);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TMD5(void) { }
	#pragma option pop
	
};


class DELPHICLASS TSHA1;
class PASCALIMPLEMENTATION TSHA1 : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	__int64 FDataSize;
	unsigned FBufferIndex;
	unsigned FDigest[5];
	unsigned FBuffer[80];
	void __fastcall ClearBuffer(void);
	unsigned __fastcall SwapDWord(const unsigned Value);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TSHA1(void);
	void __fastcall Update(const Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall Final();
	void __fastcall Reset(void);
	Clrclasses::TBytes __fastcall ComputeHash(const Clrclasses::TBytes Data);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TSHA1(void) { }
	#pragma option pop
	
};


class DELPHICLASS TOraDESBase;
class PASCALIMPLEMENTATION TOraDESBase : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	DynamicArray<Byte >  FIV;
	
protected:
	void __fastcall SetIV(Clrclasses::TBytes IV);
	Clrclasses::TBytes __fastcall XorBlock(Clrclasses::TBytes Vector, Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall InternalEncryptCBC(Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall InternalDecryptCBC(Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall InternalEncryptCBC8byte(Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall InternalDecryptCBC8byte(Clrclasses::TBytes Data);
	virtual Clrclasses::TBytes __fastcall InternalEncryptECB(Clrclasses::TBytes Data) = 0 ;
	virtual Clrclasses::TBytes __fastcall InternalDecryptECB(Clrclasses::TBytes Data) = 0 ;
public:
	#pragma option push -w-inl
	/* TObject.Create */ inline __fastcall TOraDESBase(void) : System::TObject() { }
	#pragma option pop
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDESBase(void) { }
	#pragma option pop
	
};


class DELPHICLASS TOraDES;
class PASCALIMPLEMENTATION TOraDES : public TOraDESBase 
{
	typedef TOraDESBase inherited;
	
private:
	DynamicArray<unsigned >  FEKey;
	DynamicArray<unsigned >  FDKey;
	
protected:
	TLongArr __fastcall deskey(Clrclasses::TBytes Key, bool edf);
	TLongArr __fastcall cookey(TLongArr raw1);
	Clrclasses::TBytes __fastcall des_enc(Clrclasses::TBytes Data, int Blocks);
	Clrclasses::TBytes __fastcall des_dec(Clrclasses::TBytes Data, int Blocks);
	void __fastcall scrunch(Clrclasses::TBytes outof, TLongArr into);
	void __fastcall unscrunch(TLongArr outof, Clrclasses::TBytes into);
	void __fastcall desfunc(TLongArr block, TLongArr keys);
	virtual Clrclasses::TBytes __fastcall InternalEncryptECB(Clrclasses::TBytes Data);
	virtual Clrclasses::TBytes __fastcall InternalDecryptECB(Clrclasses::TBytes Data);
	
public:
	__fastcall TOraDES(void);
	virtual void __fastcall SetKey(Clrclasses::TBytes Key, Clrclasses::TBytes IV);
	Clrclasses::TBytes __fastcall EncryptDES(Clrclasses::TBytes Key, Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall DecryptDES(Clrclasses::TBytes Key, Clrclasses::TBytes Data);
	Clrclasses::TBytes __fastcall EncryptBlocks(Clrclasses::TBytes Key, Clrclasses::TBytes Data, Clrclasses::TBytes IV);
	Clrclasses::TBytes __fastcall DecryptBlocks(Clrclasses::TBytes Key, Clrclasses::TBytes Data, Clrclasses::TBytes IV);
	Clrclasses::TBytes __fastcall EncryptBlocks8byte(Clrclasses::TBytes Key, Clrclasses::TBytes Data, Clrclasses::TBytes IV);
	Clrclasses::TBytes __fastcall DecryptBlocks8byte(Clrclasses::TBytes Key, Clrclasses::TBytes Data, Clrclasses::TBytes IV);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDES(void) { }
	#pragma option pop
	
};


typedef DynamicArray<DynamicArray<Byte > >  OraCryptUni__8;

class DELPHICLASS TOra3DES;
class PASCALIMPLEMENTATION TOra3DES : public TOraDES 
{
	typedef TOraDES inherited;
	
private:
	DynamicArray<DynamicArray<Byte > >  FTripleKey;
	
protected:
	virtual Clrclasses::TBytes __fastcall InternalEncryptECB(Clrclasses::TBytes Data);
	virtual Clrclasses::TBytes __fastcall InternalDecryptECB(Clrclasses::TBytes Data);
	
public:
	virtual void __fastcall SetKey(Clrclasses::TBytes Key, Clrclasses::TBytes IV);
public:
	#pragma option push -w-inl
	/* TOraDES.Create */ inline __fastcall TOra3DES(void) : TOraDES() { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TOra3DES(void) { }
	#pragma option pop
	
};


class DELPHICLASS TOraAES;
class PASCALIMPLEMENTATION TOraAES : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	int FParam1;
	int FParam2;
	int FParam3;
	int FParam4;
	Byte FRounds;
	DynamicArray<DynamicArray<unsigned > >  FDKey;
	DynamicArray<DynamicArray<unsigned > >  FEKey;
	unsigned __fastcall RowToInt(TByteArrArr InData, int RowIndex);
	void __fastcall IntToRow(TByteArrArr InData, unsigned InParam1, unsigned InParam2);
	unsigned __fastcall BytesToInt(Clrclasses::TBytes InData, int Offset);
	Byte __fastcall IntToByte(unsigned Value, int ByteIndex);
	
protected:
	void __fastcall InitKey(Clrclasses::TBytes Key);
	Clrclasses::TBytes __fastcall EncryptECB(Clrclasses::TBytes InData);
	Clrclasses::TBytes __fastcall DecryptECB(Clrclasses::TBytes InData);
	
public:
	__fastcall TOraAES(int Param1, int Param2, int Param3)/* overload */;
	__fastcall TOraAES(int Param1, int Param2, int Param3, int Param4)/* overload */;
	void __fastcall Init(Clrclasses::TBytes Key)/* overload */;
	Clrclasses::TBytes __fastcall Decrypt(Clrclasses::TBytes InData);
	Clrclasses::TBytes __fastcall Encrypt(Clrclasses::TBytes InData);
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~TOraAES(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Word __fastcall GetNormCharCode1(Word CharCode);
extern PACKAGE Word __fastcall GetNormCharCode2(Word CharCode);

}	/* namespace Oracryptuni */
using namespace Oracryptuni;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Oracryptuni
