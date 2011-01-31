/** \file
 *  This C header file was generated by $ANTLR version 3.1.3 Mar 17, 2009 19:23:44
 *
 *     -  From the grammar source file : PBJ.g
 *     -                            On : 2011-01-31 14:03:47
 *     -                 for the lexer : PBJLexerLexer *
 * Editing it, at least manually, is not wise. 
 *
 * C language generator and runtime by Jim Idle, jimi|hereisanat|idle|dotgoeshere|ws.
 *
 *
 * The lexer PBJLexer has the callable functions (rules) shown below,
 * which will invoke the code for the associated rule in the source grammar
 * assuming that the input stream is pointing to a token/text stream that could begin
 * this rule.
 * 
 * For instance if you call the first (topmost) rule in a parser grammar, you will
 * get the results of a full parse, but calling a rule half way through the grammar will
 * allow you to pass part of a full token stream to the parser, such as for syntax checking
 * in editors and so on.
 *
 * The parser entry points are called indirectly (by function pointer to function) via
 * a parser context typedef pPBJLexer, which is returned from a call to PBJLexerNew().
 *
 * As this is a generated lexer, it is unlikely you will call it 'manually'. However
 * the methods are provided anyway.
 * * The methods in pPBJLexer are  as follows:
 *
 *  -  void      pPBJLexer->PACKAGELITERAL(pPBJLexer)
 *  -  void      pPBJLexer->IMPORTLITERAL(pPBJLexer)
 *  -  void      pPBJLexer->DOT(pPBJLexer)
 *  -  void      pPBJLexer->MESSAGE(pPBJLexer)
 *  -  void      pPBJLexer->EXTEND(pPBJLexer)
 *  -  void      pPBJLexer->EXTENSIONS(pPBJLexer)
 *  -  void      pPBJLexer->RESERVE(pPBJLexer)
 *  -  void      pPBJLexer->TO(pPBJLexer)
 *  -  void      pPBJLexer->ENUM(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS8(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS16(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS32(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS64(pPBJLexer)
 *  -  void      pPBJLexer->REQUIRED(pPBJLexer)
 *  -  void      pPBJLexer->PBJOPTIONAL(pPBJLexer)
 *  -  void      pPBJLexer->REPEATED(pPBJLexer)
 *  -  void      pPBJLexer->DEFAULT(pPBJLexer)
 *  -  void      pPBJLexer->EQUALS(pPBJLexer)
 *  -  void      pPBJLexer->BLOCK_OPEN(pPBJLexer)
 *  -  void      pPBJLexer->BLOCK_CLOSE(pPBJLexer)
 *  -  void      pPBJLexer->ITEM_TERMINATOR(pPBJLexer)
 *  -  void      pPBJLexer->UINT8(pPBJLexer)
 *  -  void      pPBJLexer->INT8(pPBJLexer)
 *  -  void      pPBJLexer->SINT8(pPBJLexer)
 *  -  void      pPBJLexer->FIXED8(pPBJLexer)
 *  -  void      pPBJLexer->SFIXED8(pPBJLexer)
 *  -  void      pPBJLexer->UINT16(pPBJLexer)
 *  -  void      pPBJLexer->INT16(pPBJLexer)
 *  -  void      pPBJLexer->SINT16(pPBJLexer)
 *  -  void      pPBJLexer->FIXED16(pPBJLexer)
 *  -  void      pPBJLexer->SFIXED16(pPBJLexer)
 *  -  void      pPBJLexer->UINT32(pPBJLexer)
 *  -  void      pPBJLexer->INT32(pPBJLexer)
 *  -  void      pPBJLexer->SINT32(pPBJLexer)
 *  -  void      pPBJLexer->FIXED32(pPBJLexer)
 *  -  void      pPBJLexer->SFIXED32(pPBJLexer)
 *  -  void      pPBJLexer->UINT64(pPBJLexer)
 *  -  void      pPBJLexer->INT64(pPBJLexer)
 *  -  void      pPBJLexer->SINT64(pPBJLexer)
 *  -  void      pPBJLexer->FIXED64(pPBJLexer)
 *  -  void      pPBJLexer->SFIXED64(pPBJLexer)
 *  -  void      pPBJLexer->FLOAT(pPBJLexer)
 *  -  void      pPBJLexer->DOUBLE(pPBJLexer)
 *  -  void      pPBJLexer->BOOL(pPBJLexer)
 *  -  void      pPBJLexer->BYTES(pPBJLexer)
 *  -  void      pPBJLexer->STRING(pPBJLexer)
 *  -  void      pPBJLexer->UUID(pPBJLexer)
 *  -  void      pPBJLexer->SHA256(pPBJLexer)
 *  -  void      pPBJLexer->ANGLE(pPBJLexer)
 *  -  void      pPBJLexer->SOLIDANGLE(pPBJLexer)
 *  -  void      pPBJLexer->TIME(pPBJLexer)
 *  -  void      pPBJLexer->DURATION(pPBJLexer)
 *  -  void      pPBJLexer->NORMAL(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR2F(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR2D(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR3F(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR3D(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR4F(pPBJLexer)
 *  -  void      pPBJLexer->VECTOR4D(pPBJLexer)
 *  -  void      pPBJLexer->QUATERNION(pPBJLexer)
 *  -  void      pPBJLexer->BOUNDINGSPHERE3F(pPBJLexer)
 *  -  void      pPBJLexer->BOUNDINGSPHERE3D(pPBJLexer)
 *  -  void      pPBJLexer->BOUNDINGBOX3F3F(pPBJLexer)
 *  -  void      pPBJLexer->BOUNDINGBOX3D3F(pPBJLexer)
 *  -  void      pPBJLexer->SQBRACKET_OPEN(pPBJLexer)
 *  -  void      pPBJLexer->SQBRACKET_CLOSE(pPBJLexer)
 *  -  void      pPBJLexer->STRING_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->STRING_GUTS(pPBJLexer)
 *  -  void      pPBJLexer->BOOL_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->HEX_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->DECIMAL_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->OCTAL_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->HexDigit(pPBJLexer)
 *  -  void      pPBJLexer->FLOATING_POINT_LITERAL(pPBJLexer)
 *  -  void      pPBJLexer->Exponent(pPBJLexer)
 *  -  void      pPBJLexer->EscapeSequence(pPBJLexer)
 *  -  void      pPBJLexer->OctalEscape(pPBJLexer)
 *  -  void      pPBJLexer->UnicodeEscape(pPBJLexer)
 *  -  void      pPBJLexer->IDENTIFIER(pPBJLexer)
 *  -  void      pPBJLexer->QUALIFIEDIDENTIFIER(pPBJLexer)
 *  -  void      pPBJLexer->COMMENT(pPBJLexer)
 *  -  void      pPBJLexer->WS(pPBJLexer)
 *  -  void      pPBJLexer->Tokens(pPBJLexer)
 *
 * The return type for any particular rule is of course determined by the source
 * grammar file.
 */
// [The "BSD licence"]
// Copyright (c) 2005-2009 Jim Idle, Temporal Wave LLC
// http://www.temporal-wave.com
// http://www.linkedin.com/in/jimidle
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef	_PBJLexer_H
#define _PBJLexer_H
/* =============================================================================
 * Standard antlr3 C runtime definitions
 */
#include    <antlr3.h>

/* End of standard antlr 3 runtime definitions
 * =============================================================================
 */
 
#ifdef __cplusplus
extern "C" {
#endif

// Forward declare the context typedef so that we can use it before it is
// properly defined. Delegators and delegates (from import statements) are
// interdependent and their context structures contain pointers to each other
// C only allows such things to be declared if you pre-declare the typedef.
//
typedef struct PBJLexer_Ctx_struct PBJLexer, * pPBJLexer;



#ifdef	ANTLR3_WINDOWS
// Disable: Unreferenced parameter,							- Rules with parameters that are not used
//          constant conditional,							- ANTLR realizes that a prediction is always true (synpred usually)
//          initialized but unused variable					- tree rewrite variables declared but not needed
//          Unreferenced local variable						- lexer rule declares but does not always use _type
//          potentially unitialized variable used			- retval always returned from a rule 
//			unreferenced local function has been removed	- susually getTokenNames or freeScope, they can go without warnigns
//
// These are only really displayed at warning level /W4 but that is the code ideal I am aiming at
// and the codegen must generate some of these warnings by necessity, apart from 4100, which is
// usually generated when a parser rule is given a parameter that it does not use. Mostly though
// this is a matter of orthogonality hence I disable that one.
//
#pragma warning( disable : 4100 )
#pragma warning( disable : 4101 )
#pragma warning( disable : 4127 )
#pragma warning( disable : 4189 )
#pragma warning( disable : 4505 )
#pragma warning( disable : 4701 )
#endif

/** Context tracking structure for PBJLexer
 */
struct PBJLexer_Ctx_struct
{
    /** Built in ANTLR3 context tracker contains all the generic elements
     *  required for context tracking.
     */
    pANTLR3_LEXER    pLexer;


     void (*mPACKAGELITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mIMPORTLITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mDOT)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mMESSAGE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mEXTEND)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mEXTENSIONS)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mRESERVE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mTO)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mENUM)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mREQUIRED)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mPBJOPTIONAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mREPEATED)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mDEFAULT)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mEQUALS)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBLOCK_OPEN)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBLOCK_CLOSE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mITEM_TERMINATOR)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUINT8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mINT8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSINT8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFIXED8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSFIXED8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUINT16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mINT16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSINT16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFIXED16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSFIXED16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUINT32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mINT32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSINT32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFIXED32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSFIXED32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUINT64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mINT64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSINT64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFIXED64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSFIXED64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLOAT)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mDOUBLE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOOL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBYTES)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSTRING)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUUID)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSHA256)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mANGLE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSOLIDANGLE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mTIME)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mDURATION)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mNORMAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR2F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR2D)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR3F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR3D)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR4F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mVECTOR4D)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mQUATERNION)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOUNDINGSPHERE3F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOUNDINGSPHERE3D)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOUNDINGBOX3F3F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOUNDINGBOX3D3F)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSQBRACKET_OPEN)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSQBRACKET_CLOSE)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSTRING_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mSTRING_GUTS)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mBOOL_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mHEX_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mDECIMAL_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mOCTAL_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mHexDigit)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLOATING_POINT_LITERAL)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mExponent)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mEscapeSequence)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mOctalEscape)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mUnicodeEscape)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mIDENTIFIER)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mQUALIFIEDIDENTIFIER)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mCOMMENT)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mWS)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mTokens)	(struct PBJLexer_Ctx_struct * ctx);    const char * (*getGrammarFileName)();
    void	    (*free)   (struct PBJLexer_Ctx_struct * ctx);
        
};

// Function protoypes for the constructor functions that external translation units
// such as delegators and delegates may wish to call.
//
ANTLR3_API pPBJLexer PBJLexerNew         (pANTLR3_INPUT_STREAM instream);
ANTLR3_API pPBJLexer PBJLexerNewSSD      (pANTLR3_INPUT_STREAM instream, pANTLR3_RECOGNIZER_SHARED_STATE state);

/** Symbolic definitions of all the tokens that the lexer will work with.
 * \{
 *
 * Antlr will define EOF, but we can't use that as it it is too common in
 * in C header files and that would be confusing. There is no way to filter this out at the moment
 * so we just undef it here for now. That isn't the value we get back from C recognizers
 * anyway. We are looking for ANTLR3_TOKEN_EOF.
 */
#ifdef	EOF
#undef	EOF
#endif
#ifdef	Tokens
#undef	Tokens
#endif 
#define SINT16      60
#define BOUNDINGBOX3F3F      52
#define SHA256      69
#define UINT8      54
#define FLAGS64      78
#define UUID      68
#define VECTOR3F      45
#define FLAGS8      75
#define VECTOR3D      46
#define STRING_GUTS      79
#define INT8      55
#define Exponent      82
#define FLOAT      37
#define EQUALS      20
#define FIXED64      35
#define EOF      -1
#define HexDigit      81
#define BOUNDINGBOX3D3F      53
#define TIME      66
#define EXTEND      15
#define EXTENSIONS      16
#define STRING_LITERAL      6
#define SINT64      34
#define FLOATING_POINT_LITERAL      73
#define VECTOR2D      44
#define VECTOR2F      43
#define IDENTIFIER      10
#define SOLIDANGLE      65
#define UINT16      63
#define SQBRACKET_OPEN      24
#define FIXED32      30
#define SFIXED32      31
#define FIXED8      57
#define DOUBLE      38
#define MESSAGE      14
#define HEX_LITERAL      70
#define FLAGS16      76
#define COMMENT      85
#define DOT      5
#define SINT32      29
#define NORMAL      42
#define INT16      59
#define BLOCK_OPEN      12
#define QUATERNION      49
#define ANGLE      64
#define QUALIFIEDIDENTIFIER      9
#define INT64      33
#define TO      17
#define ITEM_TERMINATOR      8
#define DEFAULT      26
#define BOOL      39
#define REPEATED      23
#define BOUNDINGSPHERE3D      51
#define OCTAL_LITERAL      72
#define RESERVE      18
#define BOOL_LITERAL      74
#define BOUNDINGSPHERE3F      50
#define REQUIRED      22
#define PBJOPTIONAL      21
#define UINT64      32
#define DURATION      67
#define INT32      28
#define SINT8      56
#define FLAGS32      77
#define BLOCK_CLOSE      13
#define WS      86
#define ENUM      19
#define FIXED16      61
#define PACKAGELITERAL      7
#define SFIXED16      62
#define SFIXED64      36
#define UnicodeEscape      84
#define BYTES      41
#define VECTOR4D      48
#define IMPORTLITERAL      11
#define UINT32      27
#define VECTOR4F      47
#define PROTO      4
#define DECIMAL_LITERAL      71
#define EscapeSequence      80
#define OctalEscape      83
#define SFIXED8      58
#define STRING      40
#define SQBRACKET_CLOSE      25
#ifdef	EOF
#undef	EOF
#define	EOF	ANTLR3_TOKEN_EOF
#endif

#ifndef TOKENSOURCE
#define TOKENSOURCE(lxr) lxr->pLexer->rec->state->tokSource
#endif

/* End of token definitions for PBJLexer
 * =============================================================================
 */
/** \} */

#ifdef __cplusplus
}
#endif

#endif

/* END - Note:Keep extra line feed to satisfy UNIX systems */
