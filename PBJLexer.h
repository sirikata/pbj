/** \file
 *  This C header file was generated by $ANTLR version 3.1.3 Mar 18, 2009 10:09:25
 *
 *     -  From the grammar source file : PBJ.g
 *     -                            On : 2009-05-23 21:52:13
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
 *  -  void      pPBJLexer->TO(pPBJLexer)
 *  -  void      pPBJLexer->ENUM(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS8(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS16(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS32(pPBJLexer)
 *  -  void      pPBJLexer->FLAGS64(pPBJLexer)
 *  -  void      pPBJLexer->REQUIRED(pPBJLexer)
 *  -  void      pPBJLexer->OPTIONAL(pPBJLexer)
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
     void (*mTO)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mENUM)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS8)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS16)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS32)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mFLAGS64)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mREQUIRED)	(struct PBJLexer_Ctx_struct * ctx);
     void (*mOPTIONAL)	(struct PBJLexer_Ctx_struct * ctx);
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
#define SINT16      58
#define BOUNDINGBOX3F3F      50
#define SHA256      63
#define UINT8      52
#define FLAGS64      76
#define UUID      62
#define VECTOR3F      43
#define FLAGS8      73
#define VECTOR3D      44
#define STRING_GUTS      77
#define INT8      53
#define Exponent      80
#define FLOAT      35
#define EQUALS      18
#define FIXED64      33
#define EOF      -1
#define HexDigit      79
#define BOUNDINGBOX3D3F      51
#define TIME      65
#define EXTEND      13
#define EXTENSIONS      15
#define STRING_LITERAL      9
#define SINT64      32
#define FLOATING_POINT_LITERAL      70
#define VECTOR2D      42
#define VECTOR2F      41
#define IDENTIFIER      14
#define UINT16      61
#define SQBRACKET_OPEN      22
#define FIXED32      28
#define SFIXED32      29
#define FIXED8      55
#define DOUBLE      36
#define MESSAGE      12
#define HEX_LITERAL      67
#define FLAGS16      74
#define COMMENT      83
#define DOT      72
#define SINT32      27
#define NORMAL      40
#define INT16      57
#define BLOCK_OPEN      10
#define QUATERNION      47
#define ANGLE      64
#define QUALIFIEDIDENTIFIER      6
#define INT64      31
#define TO      16
#define ITEM_TERMINATOR      7
#define DEFAULT      24
#define BOOL      39
#define REPEATED      21
#define BOUNDINGSPHERE3D      49
#define OCTAL_LITERAL      69
#define BOOL_LITERAL      71
#define BOUNDINGSPHERE3F      48
#define REQUIRED      20
#define UINT64      30
#define DURATION      66
#define OPTIONAL      19
#define INT32      26
#define SINT8      54
#define FLAGS32      75
#define BLOCK_CLOSE      11
#define WS      84
#define ENUM      17
#define FIXED16      59
#define PACKAGELITERAL      5
#define SFIXED16      60
#define SFIXED64      34
#define UnicodeEscape      82
#define BYTES      38
#define VECTOR4D      46
#define IMPORTLITERAL      8
#define UINT32      25
#define VECTOR4F      45
#define PROTO      4
#define DECIMAL_LITERAL      68
#define EscapeSequence      78
#define OctalEscape      81
#define SFIXED8      56
#define STRING      37
#define SQBRACKET_CLOSE      23
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
