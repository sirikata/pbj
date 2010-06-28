/** \file
 *  This C header file was generated by $ANTLR version 3.1.3 Mar 18, 2009 10:09:25
 *
 *     -  From the grammar source file : PBJ.g
 *     -                            On : 2010-06-28 11:26:04
 *     -                for the parser : PBJParserParser *
 * Editing it, at least manually, is not wise. 
 *
 * C language generator and runtime by Jim Idle, jimi|hereisanat|idle|dotgoeshere|ws.
 *
 *
 * The parser PBJParser has the callable functions (rules) shown below,
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
 * a parser context typedef pPBJParser, which is returned from a call to PBJParserNew().
 *
 * The methods in pPBJParser are  as follows:
 *
 *  - PBJParser_protocol_return      pPBJParser->protocol(pPBJParser)
 *  - PBJParser_protoroot_return      pPBJParser->protoroot(pPBJParser)
 *  - PBJParser_error_header_return      pPBJParser->error_header(pPBJParser)
 *  - PBJParser_pbj_header_return      pPBJParser->pbj_header(pPBJParser)
 *  - PBJParser_package_return      pPBJParser->package(pPBJParser)
 *  - PBJParser_packageident_return      pPBJParser->packageident(pPBJParser)
 *  - PBJParser_importrule_return      pPBJParser->importrule(pPBJParser)
 *  - PBJParser_message_return      pPBJParser->message(pPBJParser)
 *  - PBJParser_message_or_extend_return      pPBJParser->message_or_extend(pPBJParser)
 *  - PBJParser_message_identifier_return      pPBJParser->message_identifier(pPBJParser)
 *  - PBJParser_message_elements_return      pPBJParser->message_elements(pPBJParser)
 *  - PBJParser_message_element_return      pPBJParser->message_element(pPBJParser)
 *  - PBJParser_extensions_return      pPBJParser->extensions(pPBJParser)
 *  - PBJParser_reservations_return      pPBJParser->reservations(pPBJParser)
 *  - PBJParser_integer_inclusive_return      pPBJParser->integer_inclusive(pPBJParser)
 *  - PBJParser_enum_def_return      pPBJParser->enum_def(pPBJParser)
 *  - PBJParser_enum_element_return      pPBJParser->enum_element(pPBJParser)
 *  - PBJParser_enum_identifier_return      pPBJParser->enum_identifier(pPBJParser)
 *  - PBJParser_flags_def_return      pPBJParser->flags_def(pPBJParser)
 *  - PBJParser_flag_identifier_return      pPBJParser->flag_identifier(pPBJParser)
 *  - PBJParser_flag_element_return      pPBJParser->flag_element(pPBJParser)
 *  - PBJParser_field_return      pPBJParser->field(pPBJParser)
 *  - PBJParser_field_offset_return      pPBJParser->field_offset(pPBJParser)
 *  - PBJParser_field_name_return      pPBJParser->field_name(pPBJParser)
 *  - PBJParser_field_type_return      pPBJParser->field_type(pPBJParser)
 *  - PBJParser_multiplicitive_type_return      pPBJParser->multiplicitive_type(pPBJParser)
 *  - PBJParser_array_spec_return      pPBJParser->array_spec(pPBJParser)
 *  - PBJParser_default_value_return      pPBJParser->default_value(pPBJParser)
 *  - PBJParser_numeric_type_return      pPBJParser->numeric_type(pPBJParser)
 *  - PBJParser_array_type_return      pPBJParser->array_type(pPBJParser)
 *  - PBJParser_multiplicitive_advanced_type_return      pPBJParser->multiplicitive_advanced_type(pPBJParser)
 *  - PBJParser_advanced_numeric_type_return      pPBJParser->advanced_numeric_type(pPBJParser)
 *  - PBJParser_advanced_array_type_return      pPBJParser->advanced_array_type(pPBJParser)
 *  - PBJParser_literal_value_return      pPBJParser->literal_value(pPBJParser)
 *  - PBJParser_flags_return      pPBJParser->flags(pPBJParser)
 *  - PBJParser_integer_return      pPBJParser->integer(pPBJParser)
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

#ifndef	_PBJParser_H
#define _PBJParser_H
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
typedef struct PBJParser_Ctx_struct PBJParser, * pPBJParser;



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
typedef struct PBJParser_protocol_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_protocol_return;

typedef struct PBJParser_protoroot_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_protoroot_return;

typedef struct PBJParser_error_header_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_error_header_return;

typedef struct PBJParser_pbj_header_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_pbj_header_return;

typedef struct PBJParser_package_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_package_return;

typedef struct PBJParser_packageident_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_packageident_return;

typedef struct PBJParser_importrule_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_importrule_return;

typedef struct PBJParser_message_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_message_return;

typedef struct PBJParser_message_or_extend_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_message_or_extend_return;

typedef struct PBJParser_message_identifier_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_message_identifier_return;

typedef struct PBJParser_message_elements_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_message_elements_return;

typedef struct PBJParser_message_element_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_message_element_return;

typedef struct PBJParser_extensions_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_extensions_return;

typedef struct PBJParser_reservations_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_reservations_return;

typedef struct PBJParser_integer_inclusive_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_integer_inclusive_return;

typedef struct PBJParser_enum_def_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_enum_def_return;

typedef struct PBJParser_enum_element_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_enum_element_return;

typedef struct PBJParser_enum_identifier_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_enum_identifier_return;

typedef struct PBJParser_flags_def_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_flags_def_return;

typedef struct PBJParser_flag_identifier_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_flag_identifier_return;

typedef struct PBJParser_flag_element_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_flag_element_return;

typedef struct PBJParser_field_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_field_return;

typedef struct PBJParser_field_offset_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_field_offset_return;

typedef struct PBJParser_field_name_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_field_name_return;

typedef struct PBJParser_field_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_field_type_return;

typedef struct PBJParser_multiplicitive_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_multiplicitive_type_return;

typedef struct PBJParser_array_spec_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_array_spec_return;

typedef struct PBJParser_default_value_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_default_value_return;

typedef struct PBJParser_numeric_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_numeric_type_return;

typedef struct PBJParser_array_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_array_type_return;

typedef struct PBJParser_multiplicitive_advanced_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_multiplicitive_advanced_type_return;

typedef struct PBJParser_advanced_numeric_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_advanced_numeric_type_return;

typedef struct PBJParser_advanced_array_type_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_advanced_array_type_return;

typedef struct PBJParser_literal_value_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_literal_value_return;

typedef struct PBJParser_flags_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_flags_return;

typedef struct PBJParser_integer_return_struct
{
    /** Generic return elements for ANTLR3 rules that are not in tree parsers or returning trees
     */
    pANTLR3_COMMON_TOKEN    start;
    pANTLR3_COMMON_TOKEN    stop;
    pANTLR3_BASE_TREE	tree;
   
}
    PBJParser_integer_return;



/* globalAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the Symbols scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_SymbolsPush().
 */
typedef struct  PBJParser_Symbols_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_Symbols_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    pANTLR3_STRING message;
    pANTLR3_LIST required_advanced_fields;
    pANTLR3_HASH_TABLE types;
    pANTLR3_HASH_TABLE flag_sizes;
    pANTLR3_HASH_TABLE enum_sizes;
    pANTLR3_HASH_TABLE flag_values;
    pANTLR3_HASH_TABLE flag_all_on;
    pANTLR3_HASH_TABLE enum_values;
    int *reserved_range_start;
    int *reserved_range_end;
    int num_reserved_ranges;
    int *extension_range_start;
    int *extension_range_end;
    int num_extension_ranges;
    struct CsStreams *cs_streams;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_Symbols_SCOPE, * pPBJParser_Symbols_SCOPE;
/* globalAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the NameSpace scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_NameSpacePush().
 */
typedef struct  PBJParser_NameSpace_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_NameSpace_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    struct LanguageOutputStruct* output;
    pANTLR3_STRING filename;
    pANTLR3_STRING externalNamespace;
    pANTLR3_STRING internalNamespace;
    pANTLR3_STRING package;
    pANTLR3_LIST imports;
    pANTLR3_STRING prefix;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_NameSpace_SCOPE, * pPBJParser_NameSpace_SCOPE;

/* ruleAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the message scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_messagePush().
 */
typedef struct  PBJParser_message_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_message_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    int isExtension;
    pANTLR3_STRING messageName;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_message_SCOPE, * pPBJParser_message_SCOPE;
/* ruleAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the enum_def scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_enum_defPush().
 */
typedef struct  PBJParser_enum_def_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_enum_def_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    pANTLR3_STRING enumName;
    pANTLR3_LIST enumList;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_enum_def_SCOPE, * pPBJParser_enum_def_SCOPE;
/* ruleAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the flags_def scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_flags_defPush().
 */
typedef struct  PBJParser_flags_def_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_flags_def_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    pANTLR3_STRING flagName;
    pANTLR3_LIST flagList;
    int flagBits;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_flags_def_SCOPE, * pPBJParser_flags_def_SCOPE;
/* ruleAttributeScopeDecl(scope)
 */
/* makeScopeSet() 
 */
 /** Definition of the field scope variable tracking
 *  structure. An instance of this structure is created by calling
 *  PBJParser_fieldPush().
 */
typedef struct  PBJParser_field_SCOPE_struct
{
    /** Function that the user may provide to be called when the
     *  scope is destroyed (so you can free pANTLR3_HASH_TABLES and so on)
     *
     * \param POinter to an instance of this typedef/struct
     */
    void    (ANTLR3_CDECL *free)	(struct PBJParser_field_SCOPE_struct * frame);
    
    /* =============================================================================
     * Programmer defined variables...
     */
    pANTLR3_STRING fieldType;
    pANTLR3_STRING fieldName;
    pANTLR3_STRING defaultValue;
    int fieldOffset;
    int isNumericType;

    /* End of programmer defined variables
     * =============================================================================
     */
} 
    PBJParser_field_SCOPE, * pPBJParser_field_SCOPE;

/** Context tracking structure for PBJParser
 */
struct PBJParser_Ctx_struct
{
    /** Built in ANTLR3 context tracker contains all the generic elements
     *  required for context tracking.
     */
    pANTLR3_PARSER   pParser;
    /* globalAttributeScopeDef(scope)
     */
    /** Pointer to the  Symbols stack for use by pPBJParser_SymbolsPush()
     *  and pPBJParser_SymbolsPop()
     */
    pANTLR3_STACK pPBJParser_SymbolsStack;
    ANTLR3_UINT32 pPBJParser_SymbolsStack_limit;
    /** Pointer to the top of the stack for the global scope pPBJParser_SymbolsStack
     */
    pPBJParser_Symbols_SCOPE    (*pPBJParser_SymbolsPush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_Symbols_SCOPE    pPBJParser_SymbolsTop;


    /* globalAttributeScopeDef(scope)
     */
    /** Pointer to the  NameSpace stack for use by pPBJParser_NameSpacePush()
     *  and pPBJParser_NameSpacePop()
     */
    pANTLR3_STACK pPBJParser_NameSpaceStack;
    ANTLR3_UINT32 pPBJParser_NameSpaceStack_limit;
    /** Pointer to the top of the stack for the global scope pPBJParser_NameSpaceStack
     */
    pPBJParser_NameSpace_SCOPE    (*pPBJParser_NameSpacePush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_NameSpace_SCOPE    pPBJParser_NameSpaceTop;

    /* ruleAttributeScopeDef(scope)
     */
    /** Pointer to the  message stack for use by pPBJParser_messagePush()
     *  and pPBJParser_messagePop()
     */
    pANTLR3_STACK pPBJParser_messageStack;
    ANTLR3_UINT32 pPBJParser_messageStack_limit;
    pPBJParser_message_SCOPE   (*pPBJParser_messagePush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_message_SCOPE   pPBJParser_messageTop;
    /* ruleAttributeScopeDef(scope)
     */
    /** Pointer to the  enum_def stack for use by pPBJParser_enum_defPush()
     *  and pPBJParser_enum_defPop()
     */
    pANTLR3_STACK pPBJParser_enum_defStack;
    ANTLR3_UINT32 pPBJParser_enum_defStack_limit;
    pPBJParser_enum_def_SCOPE   (*pPBJParser_enum_defPush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_enum_def_SCOPE   pPBJParser_enum_defTop;
    /* ruleAttributeScopeDef(scope)
     */
    /** Pointer to the  flags_def stack for use by pPBJParser_flags_defPush()
     *  and pPBJParser_flags_defPop()
     */
    pANTLR3_STACK pPBJParser_flags_defStack;
    ANTLR3_UINT32 pPBJParser_flags_defStack_limit;
    pPBJParser_flags_def_SCOPE   (*pPBJParser_flags_defPush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_flags_def_SCOPE   pPBJParser_flags_defTop;
    /* ruleAttributeScopeDef(scope)
     */
    /** Pointer to the  field stack for use by pPBJParser_fieldPush()
     *  and pPBJParser_fieldPop()
     */
    pANTLR3_STACK pPBJParser_fieldStack;
    ANTLR3_UINT32 pPBJParser_fieldStack_limit;
    pPBJParser_field_SCOPE   (*pPBJParser_fieldPush)(struct PBJParser_Ctx_struct * ctx);
    pPBJParser_field_SCOPE   pPBJParser_fieldTop;


     PBJParser_protocol_return (*protocol)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_protoroot_return (*protoroot)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_error_header_return (*error_header)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_pbj_header_return (*pbj_header)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_package_return (*package)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_packageident_return (*packageident)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_importrule_return (*importrule)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_message_return (*message)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_message_or_extend_return (*message_or_extend)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_message_identifier_return (*message_identifier)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_message_elements_return (*message_elements)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_message_element_return (*message_element)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_extensions_return (*extensions)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_reservations_return (*reservations)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_integer_inclusive_return (*integer_inclusive)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_enum_def_return (*enum_def)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_enum_element_return (*enum_element)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_enum_identifier_return (*enum_identifier)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_flags_def_return (*flags_def)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_flag_identifier_return (*flag_identifier)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_flag_element_return (*flag_element)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_field_return (*field)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_field_offset_return (*field_offset)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_field_name_return (*field_name)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_field_type_return (*field_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_multiplicitive_type_return (*multiplicitive_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_array_spec_return (*array_spec)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_default_value_return (*default_value)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_numeric_type_return (*numeric_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_array_type_return (*array_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_multiplicitive_advanced_type_return (*multiplicitive_advanced_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_advanced_numeric_type_return (*advanced_numeric_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_advanced_array_type_return (*advanced_array_type)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_literal_value_return (*literal_value)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_flags_return (*flags)	(struct PBJParser_Ctx_struct * ctx);
     PBJParser_integer_return (*integer)	(struct PBJParser_Ctx_struct * ctx);
    // Delegated rules
    const char * (*getGrammarFileName)();
    void	    (*free)   (struct PBJParser_Ctx_struct * ctx);
    /* @headerFile.members() */
    pANTLR3_BASE_TREE_ADAPTOR	adaptor;
    pANTLR3_VECTOR_FACTORY		vectors;
    /* End @headerFile.members() */
};

// Function protoypes for the constructor functions that external translation units
// such as delegators and delegates may wish to call.
//
ANTLR3_API pPBJParser PBJParserNew         (pANTLR3_COMMON_TOKEN_STREAM instream);
ANTLR3_API pPBJParser PBJParserNewSSD      (pANTLR3_COMMON_TOKEN_STREAM instream, pANTLR3_RECOGNIZER_SHARED_STATE state);

/** Symbolic definitions of all the tokens that the parser will work with.
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
#define FLAGS64      78
#define UINT8      54
#define UUID      68
#define VECTOR3F      45
#define FLAGS8      75
#define STRING_GUTS      79
#define VECTOR3D      46
#define INT8      55
#define Exponent      82
#define EQUALS      20
#define FLOAT      37
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
#define TO      17
#define INT64      33
#define ITEM_TERMINATOR      8
#define DEFAULT      26
#define BOOL      39
#define BOUNDINGSPHERE3D      51
#define REPEATED      23
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
#define WS      86
#define BLOCK_CLOSE      13
#define ENUM      19
#define FIXED16      61
#define SFIXED16      62
#define PACKAGELITERAL      7
#define SFIXED64      36
#define UnicodeEscape      84
#define BYTES      41
#define VECTOR4D      48
#define IMPORTLITERAL      11
#define UINT32      27
#define VECTOR4F      47
#define PROTO      4
#define OctalEscape      83
#define EscapeSequence      80
#define DECIMAL_LITERAL      71
#define SFIXED8      58
#define SQBRACKET_CLOSE      25
#define STRING      40
#ifdef	EOF
#undef	EOF
#define	EOF	ANTLR3_TOKEN_EOF
#endif

#ifndef TOKENSOURCE
#define TOKENSOURCE(lxr) lxr->pLexer->rec->state->tokSource
#endif

/* End of token definitions for PBJParser
 * =============================================================================
 */
/** \} */

#ifdef __cplusplus
}
#endif

#endif

/* END - Note:Keep extra line feed to satisfy UNIX systems */
