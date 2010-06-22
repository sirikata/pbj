/*  PBJ Parsing Utility API for streaming C# and C++ pbj classes
 *  PBJParseUtil.h
 *
 *  Copyright (c) 2009, Daniel Reiter Horn
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *  * Neither the name of Sirikata nor the names of its contributors may
 *    be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _PBJ_PARSE_UTIL_H_
#define _PBJ_PARSE_UTIL_H_
#ifdef __cplusplus
extern "C" {
#endif
#undef	SCOPE_TYPE
#undef	SCOPE_STACK
#undef	SCOPE_TOP
#define SCOPE_PUSH(scope) ctx->pPBJParser_##scope##Top = pPBJParser_##scope##Push(ctx)
#define SCOPE_POP(scope) pPBJParser_##scope##Pop(ctx)
#define	SCOPE_TYPE(scope)   pPBJParser_##scope##_SCOPE
#define SCOPE_STACK(scope)  pPBJParser_##scope##Stack
#define	SCOPE_TOP(scope)    ctx->pPBJParser_##scope##Top
//#define	SCOPE_SIZE(scope)			(ctx->SCOPE_STACK(scope)->size(ctx->SCOPE_STACK(scope)))
#ifndef SCOPE_SIZE
#define	SCOPE_SIZE(scope) ctx->pPBJParser_##scope##Stack_limit
#endif
#define SCOPE_INSTANCE(scope, i)	(ctx->SCOPE_STACK(scope)->get(ctx->SCOPE_STACK(scope),i))
typedef struct LanguageOutputStruct LanguageOutput;
typedef struct CsStreams CsStreamOutput;


    pANTLR3_UINT8 protoImportFromPBJToken(pPBJParser ctx, pANTLR3_COMMON_TOKEN name);
    pANTLR3_STRING stripPBJExtension(pANTLR3_STRING name);

    pANTLR3_UINT8 replaceImportedMessageType(pPBJParser ctx, pANTLR3_COMMON_TOKEN name);
    pANTLR3_STRING filterImportedMessageType(pANTLR3_STRING name);

void initSymbolTable(SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING msgId, int isExtension);
void initNameSpace(pPBJParser ctx, SCOPE_TYPE(NameSpace) symtab);

void definePackage(pPBJParser ctx, pANTLR3_STRING id);
void defineImport(pPBJParser ctx, pANTLR3_STRING filename);
void defineType(pPBJParser ctx, pANTLR3_STRING id);
void defineMessage(pPBJParser ctx, pANTLR3_STRING id);
void defineMessageEnd(pPBJParser ctx, pANTLR3_STRING id);
void defineExtensionRange(pPBJParser ctx, pANTLR3_STRING extension_start, pANTLR3_STRING extension_end);
void defineReservedRange(pPBJParser ctx, pANTLR3_STRING extension_start, pANTLR3_STRING extension_end);
void defineExtension(pPBJParser ctx, pANTLR3_STRING id);
void defineExtensionEnd(pPBJParser ctx, pANTLR3_STRING id);
void defineEnumValue(pPBJParser ctx, pANTLR3_STRING messageName, pANTLR3_STRING enumName, pANTLR3_LIST enumList, pANTLR3_STRING id,  pANTLR3_STRING value);
void defineFlagValue(pPBJParser ctx, pANTLR3_STRING messageName, pANTLR3_STRING flagName, pANTLR3_LIST flagList, pANTLR3_STRING id,  pANTLR3_STRING value);
void defineEnum(pPBJParser ctx, pANTLR3_STRING messageName, pANTLR3_STRING id, pANTLR3_LIST enumList);
void defineFlag(pPBJParser ctx, pANTLR3_STRING messageName, pANTLR3_STRING id, pANTLR3_LIST flagLis, unsigned int bits);
void defineField(pPBJParser ctx, pANTLR3_STRING type, pANTLR3_STRING name, pANTLR3_STRING value, unsigned int field_offset, int notRepeated, int isRequired, int multiplicitive_advanced_type);

SCOPE_TYPE(NameSpace) NameSpacePush(pPBJParser ctx);
void NameSpacePop(pPBJParser ctx);


void ANTLR3_CDECL freeSymbolTable(SCOPE_TYPE(Symbols) symtab);
void ANTLR3_CDECL stringFree(void *s);
void grammarToString(pANTLR3_TREE_NODE_STREAM node_stream, pANTLR3_BASE_TREE base, pANTLR3_BASE_TREE stop, pANTLR3_STRING bufOutput);
pANTLR3_STRING ANTLR3_CDECL stringDup(pANTLR3_STRING s);
pANTLR3_STRING defaultValuePreprocess(pPBJParser ctx, pANTLR3_STRING type, pANTLR3_STRING value);
ANTLR3_BOOLEAN isTypeName(pPBJParser ctx, pANTLR3_UINT8 name);
#ifdef __cplusplus
}
#endif
#endif //_PBJ_PARSE_UTIL_H_
