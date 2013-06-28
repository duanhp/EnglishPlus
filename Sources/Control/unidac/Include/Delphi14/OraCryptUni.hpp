// CodeGear C++Builder
// Copyright (c) 1995, 2009 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Oracryptuni.pas' rev: 21.00

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
typedef DynamicArray<System::ShortInt> TShortArr;

typedef DynamicArray<unsigned> TLongArr;

typedef DynamicArray<System::Byte> Oracryptuni__1;

typedef DynamicArray<DynamicArray<System::Byte> > TByteArrArr;

typedef DynamicArray<unsigned> Oracryptuni__2;

typedef DynamicArray<DynamicArray<unsigned> > TLongArrArr;

typedef Sysutils::TBytes __fastcall (__closure *TCryptFunc)(Sysutils::TBytes Data);

class DELPHICLASS TMD5;
class PASCALIMPLEMENTATION TMD5 : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__int64 FDataSize;
	unsigned FBufferIndex;
	StaticArray<unsigned, 4> FDigest;
	StaticArray<unsigned, 16> FBuffer;
	void __fastcall ClearBuffer(void);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TMD5(void);
	void __fastcall Reset(void);
	void __fastcall Update(const Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall Final(void);
	Sysutils::TBytes __fastcall ComputeHash(const Sysutils::TBytes Data);
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
	StaticArray<unsigned, 5> FDigest;
	StaticArray<unsigned, 80> FBuffer;
	void __fastcall ClearBuffer(void);
	unsigned __fastcall SwapDWord(const unsigned Value);
	
protected:
	void __fastcall Transform(void);
	
public:
	__fastcall TSHA1(void);
	void __fastcall Update(const Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall Final(void);
	void __fastcall Reset(void);
	Sysutils::TBytes __fastcall ComputeHash(const Sysutils::TBytes Data);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSHA1(void) { }
	
};


class DELPHICLASS TOraDESBase;
class PASCALIMPLEMENTATION TOraDESBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sysutils::TBytes FIV;
	
protected:
	void __fastcall SetIV(Sysutils::TBytes IV);
	Sysutils::TBytes __fastcall XorBlock(Sysutils::TBytes Vector, Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall InternalEncryptCBC(Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall InternalDecryptCBC(Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall InternalEncryptCBC8byte(Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall InternalDecryptCBC8byte(Sysutils::TBytes Data);
	virtual Sysutils::TBytes __fastcall InternalEncryptECB(Sysutils::TBytes Data) = 0 ;
	virtual Sysutils::TBytes __fastcall InternalDecryptECB(Sysutils::TBytes Data) = 0 ;
public:
	/* TObject.Create */ inline __fastcall TOraDESBase(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDESBase(void) { }
	
};


class DELPHICLASS TOraDES;
class PASCALIMPLEMENTATION TOraDES : public TOraDESBase
{
	typedef TOraDESBase inherited;
	
private:
	TLongArr FEKey;
	TLongArr FDKey;
	
protected:
	TLongArr __fastcall deskey(Sysutils::TBytes Key, bool edf);
	TLongArr __fastcall cookey(TLongArr raw1);
	Sysutils::TBytes __fastcall des_enc(Sysutils::TBytes Data, int Blocks);
	Sysutils::TBytes __fastcall des_dec(Sysutils::TBytes Data, int Blocks);
	void __fastcall scrunch(Sysutils::TBytes outof, TLongArr into);
	void __fastcall unscrunch(TLongArr outof, Sysutils::TBytes into);
	void __fastcall desfunc(TLongArr block, TLongArr keys);
	virtual Sysutils::TBytes __fastcall InternalEncryptECB(Sysutils::TBytes Data);
	virtual Sysutils::TBytes __fastcall InternalDecryptECB(Sysutils::TBytes Data);
	
public:
	__fastcall TOraDES(void);
	virtual void __fastcall SetKey(Sysutils::TBytes Key, Sysutils::TBytes IV);
	Sysutils::TBytes __fastcall EncryptDES(Sysutils::TBytes Key, Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall DecryptDES(Sysutils::TBytes Key, Sysutils::TBytes Data);
	Sysutils::TBytes __fastcall EncryptBlocks(Sysutils::TBytes Key, Sysutils::TBytes Data, Sysutils::TBytes IV);
	Sysutils::TBytes __fastcall DecryptBlocks(Sysutils::TBytes Key, Sysutils::TBytes Data, Sysutils::TBytes IV);
	Sysutils::TBytes __fastcall EncryptBlocks8byte(Sysutils::TBytes Key, Sysutils::TBytes Data, Sysutils::TBytes IV);
	Sysutils::TBytes __fastcall DecryptBlocks8byte(Sysutils::TBytes Key, Sysutils::TBytes Data, Sysutils::TBytes IV);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOraDES(void) { }
	
};


class DELPHICLASS TOra3DES;
class PASCALIMPLEMENTATION TOra3DES : public TOraDES
{
	typedef TOraDES inherited;
	
private:
	typedef DynamicArray<DynamicArray<System::Byte> > _TOra3DES__1;
	
	
private:
	_TOra3DES__1 FTripleKey;
	
protected:
	virtual Sysutils::TBytes __fastcall InternalEncryptECB(Sysutils::TBytes Data);
	virtual Sysutils::TBytes __fastcall InternalDecryptECB(Sysutils::TBytes Data);
	
public:
	virtual void __fastcall SetKey(Sysutils::TBytes Key, Sysutils::TBytes IV);
public:
	/* TOraDES.Create */ inline __fastcall TOra3DES(void) : TOraDES() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOra3DES(void) { }
	
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
	System::Byte FRounds;
	TLongArrArr FDKey;
	TLongArrArr FEKey;
	unsigned __fastcall RowToInt(TByteArrArr InData, int RowIndex);
	void __fastcall IntToRow(TByteArrArr InData, unsigned InParam1, unsigned InParam2);
	unsigned __fastcall BytesToInt(Sysutils::TBytes InData, int Offset);
	System::Byte __fastcall IntToByte(unsigned Value, int ByteIndex);
	
protected:
	void __fastcall InitKey(Sysutils::TBytes Key);
	Sysutils::TBytes __fastcall EncryptECB(Sysutils::TBytes InData);
	Sysutils::TBytes __fastcall DecryptECB(Sysutils::TBytes InData);
	
public:
	__fastcall TOraAES(int Param1, int Param2, int Param3)/* overload */;
	__fastcall TOraAES(int Param1, int Param2, int Param3, int Param4)/* overload */;
	void __fastcall Init(Sysutils::TBytes Key)/* overload */;
	Sysutils::TBytes __fastcall Decrypt(Sysutils::TBytes InData);
	Sysutils::TBytes __fastcall Encrypt(Sysutils::TBytes InData);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOraAES(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE System::Word __fastcall GetNormCharCode1(System::Word CharCode);
extern PACKAGE System::Word __fastcall GetNormCharCode2(System::Word CharCode);

}	/* namespace Oracryptuni */
using namespace Oracryptuni;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// OracryptuniHPP
