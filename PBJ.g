/*  PBJ Parsing Grammar
 *  PBJ.g
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

grammar PBJ;

options
{
    output = AST;
    language = C;
    ASTLabelType = pANTLR3_BASE_TREE;
}

tokens
{
    PROTO;
}

scope NameSpace {
    struct LanguageOutputStruct* output;
    pANTLR3_STRING filename;
    pANTLR3_STRING externalNamespace;
    pANTLR3_STRING internalNamespace;
    pANTLR3_STRING package;
    pANTLR3_LIST imports;
    pANTLR3_STRING prefix;
}

scope Symbols {
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
}

@members {
    #include "PBJParseUtil.h"
}
protocol
    scope Symbols;
    @init {
        initSymbolTable(SCOPE_TOP(Symbols),NULL,0);
    }
    : protoroot ->protoroot
    ;

protoroot
    scope NameSpace;
    @init {
        initNameSpace(ctx,SCOPE_TOP(NameSpace));
    }
	:	(pbj_header|error_header) importrule* package importrule* message*
    {
    }
	|	(importrule* message* -> PACKAGELITERAL["package"] WS[" "] QUALIFIEDIDENTIFIER[SCOPE_TOP(NameSpace)->externalNamespace->chars] STRING_LITERAL[SCOPE_TOP(NameSpace)->internalNamespace->chars] ITEM_TERMINATOR[";"] WS["\n"] importrule* message*)
    {
        definePackage( ctx, NULL );
    }
	;

error_header : ((DOT?)->WS["\n"])
        {
                fprintf(stderr,"error: line 0: please put \"pbj-0.0.3\" at the top of your PBJ file\n");
                exit(1);
        };
pbj_header : (STRING_LITERAL -> WS["\n"])
    {
            if (strncmp((char*)$STRING_LITERAL.text->chars,"\"pbj-0.0.3\"",9)==0&&$STRING_LITERAL.text->chars[9]<='9'&&$STRING_LITERAL.text->chars[9]>='3') {
                
            }else {

                fprintf(stderr,"error: line \%d: pbj version \%s not understood--this compiler understands \"pbj-0.0.3\"\n",$STRING_LITERAL->line,$STRING_LITERAL.text->chars);  
                exit(1);
            }
    }
    ;


package
   :   ( PACKAGELITERAL packageident ITEM_TERMINATOR -> PACKAGELITERAL WS[" "] packageident QUALIFIEDIDENTIFIER["."] QUALIFIEDIDENTIFIER[SCOPE_TOP(NameSpace)->externalNamespace->chars] QUALIFIEDIDENTIFIER[SCOPE_TOP(NameSpace)->internalNamespace->chars] ITEM_TERMINATOR WS["\n"])
        {
            definePackage( ctx, $packageident.text );
        }
	;
packageident: (QUALIFIEDIDENTIFIER|IDENTIFIER);

// The following is a bit confusing: the transformation converts into a .proto for writing the .proto file.  However, this (apparently) doesn't change the
// AST that's generated, which still has the .pbj version.  Therefore, we strip the .pbj again when we call defineImport.
importrule
   :   (IMPORTLITERAL STRING_LITERAL ITEM_TERMINATOR -> IMPORTLITERAL WS[" "] STRING_LITERAL[{protoImportFromPBJToken(ctx, $STRING_LITERAL)}] ITEM_TERMINATOR WS["\n"] )
        {
            defineImport( ctx, stripPBJExtension($STRING_LITERAL.text) );
        }
	;

message
    scope {
        int isExtension;
        pANTLR3_STRING messageName;
    }
    :   ( message_or_extend message_identifier BLOCK_OPEN message_elements BLOCK_CLOSE -> message_or_extend WS[" "] message_identifier WS[" "] BLOCK_OPEN WS["\n"] message_elements BLOCK_CLOSE WS["\n"] )
        {
            if(!$message::isExtension) {
                defineType( ctx, $message::messageName );
            }
            stringFree($message::messageName);
        }
	;

message_or_extend : 
        MESSAGE {$message::isExtension=0;}
        |
        EXTEND {$message::isExtension=1;}
        ;

message_identifier
    : IDENTIFIER
    {
        $message::messageName=stringDup($IDENTIFIER.text);
        if ($message::isExtension) {
            defineExtension(ctx, $message::messageName);
        }else {
            defineMessage(ctx, $message::messageName);
        }
    }
    ;

message_elements
    scope Symbols;
    @init
    {
        initSymbolTable(SCOPE_TOP(Symbols), $message::messageName, $message::isExtension);  
    }
	:	message_element*
    {
        if($message::isExtension) {
            defineExtensionEnd(ctx, $message::messageName);
        }else {
            defineMessageEnd(ctx, $message::messageName);
        }
    }
    ;

message_element
	:	field
	|	message
	|	enum_def
	|	flags_def
    |   extensions
    |   reservations
	;

extensions
        : 
        ( EXTENSIONS integer TO integer_inclusive ITEM_TERMINATOR -> WS["\t"] EXTENSIONS WS[" "] integer WS[" "] TO WS[" "] integer_inclusive ITEM_TERMINATOR WS["\n"] )
        {
            defineExtensionRange(ctx, $integer.text, $integer_inclusive.text);
        }
        ;

reservations : (RESERVE integer TO integer_inclusive ITEM_TERMINATOR -> )
        {
            defineReservedRange(ctx, $integer.text, $integer_inclusive.text);
        }
        ;

integer_inclusive : integer 
        {
            
        }
        ;

enum_def
    scope {
        pANTLR3_STRING enumName;
        pANTLR3_LIST enumList;
    }
    @init {
        $enum_def::enumList=antlr3ListNew(1);
    }
	:	( ENUM enum_identifier BLOCK_OPEN enum_element+ BLOCK_CLOSE -> WS["\t"] ENUM WS[" "] enum_identifier WS[" "] BLOCK_OPEN WS["\n"] (WS["\t"] enum_element)+ WS["\t"] BLOCK_CLOSE WS["\n"] )
        {
            defineEnum( ctx, $message::messageName, $enum_def::enumName, $enum_def::enumList);
            $enum_def::enumList->free($enum_def::enumList);
            stringFree($enum_def::enumName);
        }
	;

enum_element
	:	(IDENTIFIER EQUALS integer ITEM_TERMINATOR -> WS["\t"] IDENTIFIER WS[" "] EQUALS WS[" "] integer ITEM_TERMINATOR WS["\n"] )
        {
            defineEnumValue( ctx, $message::messageName, $enum_def::enumName, $enum_def::enumList, $IDENTIFIER.text, $integer.text );
        }
	;
enum_identifier
    : IDENTIFIER
      {
            $enum_def::enumName=stringDup($IDENTIFIER.text);
      }
      ;

flags_def
    scope
    {
        pANTLR3_STRING flagName;
        pANTLR3_LIST flagList;
        int flagBits;
    }
    @init {
        $flags_def::flagList=antlr3ListNew(1);
        
    }
	:	( flags flag_identifier BLOCK_OPEN flag_element+ BLOCK_CLOSE -> WS["\t"] ENUM["enum"] WS[" "] flag_identifier WS[" "] BLOCK_OPEN WS["\n"] (WS["\t"] flag_element)+ WS["\t"] BLOCK_CLOSE WS["\n"] )
        {
            defineFlag( ctx, $message::messageName, $flags_def::flagName, $flags_def::flagList, $flags_def::flagBits);
            $flags_def::flagList->free($flags_def::flagList);
            stringFree($flags_def::flagName);
        }
	;

flag_identifier 
	:	IDENTIFIER
        {
            $flags_def::flagName=stringDup($IDENTIFIER.text);
        }
	;

flag_element
	:	( IDENTIFIER EQUALS integer ITEM_TERMINATOR -> WS["\t"] IDENTIFIER WS[" "] EQUALS WS[" "] integer ITEM_TERMINATOR WS["\n"])
        {
            defineFlagValue( ctx, $message::messageName, $flags_def::flagName, $flags_def::flagList, $IDENTIFIER.text , $integer.text);
        }
	;

field
    scope{
        pANTLR3_STRING fieldType;
        pANTLR3_STRING fieldName;
        ///protobuf value if it is an advanced_type or default kind of type...  C++ value if it's a multiplicitve type
        pANTLR3_STRING defaultValue;
        int fieldOffset;
        int isNumericType;
    }
    @init {$field::defaultValue=NULL; $field::isNumericType=0;}
    :  ( ( (PBJOPTIONAL multiplicitive_type field_name EQUALS field_offset default_value? ITEM_TERMINATOR ) | ( (REQUIRED|REPEATED) multiplicitive_type field_name EQUALS field_offset ITEM_TERMINATOR ) ) -> WS["\t"] REPEATED["repeated"] WS[" "] multiplicitive_type WS[" "] field_name WS[" "] EQUALS WS[" "] field_offset WS[" "] SQBRACKET_OPEN["["] IDENTIFIER["packed"] EQUALS["="] BOOL_LITERAL["true"] SQBRACKET_CLOSE["]"] ITEM_TERMINATOR WS["\n"] )
    {
        defineField(ctx, $field::fieldType,$field::fieldName,$field::defaultValue,$field::fieldOffset,$REPEATED==NULL,$REQUIRED!=NULL,1);
        stringFree($field::fieldName);
        stringFree($field::fieldType);
        stringFree($field::defaultValue);
    }
     |
     (( (PBJOPTIONAL field_type field_name EQUALS field_offset default_value? ITEM_TERMINATOR )  -> WS["\t"] PBJOPTIONAL WS[" "] field_type WS[" "] field_name WS[" "] EQUALS WS[" "] field_offset WS[" "] default_value ITEM_TERMINATOR WS["\n"] )
      | 
      ( ( (REQUIRED|REPEATED) field_type field_name EQUALS field_offset ITEM_TERMINATOR ) 
          -> {$field::isNumericType && $REQUIRED==NULL}?WS["\t"] REPEATED WS[" "] field_type WS[" "] field_name WS[" "] EQUALS WS[" "] field_offset  WS[" "] SQBRACKET_OPEN["["] IDENTIFIER["packed"] EQUALS["="] BOOL_LITERAL["true"] SQBRACKET_CLOSE["]"] ITEM_TERMINATOR WS["\n"]  
          -> WS["\t"] REQUIRED REPEATED WS[" "] field_type WS[" "] field_name WS[" "] EQUALS WS[" "] field_offset ITEM_TERMINATOR WS["\n"] ) )
    {
        defineField(ctx, $field::fieldType,$field::fieldName,$field::defaultValue,$field::fieldOffset,$REPEATED==NULL,$REQUIRED!=NULL,0);
        stringFree($field::fieldName);
        stringFree($field::fieldType);
        stringFree($field::defaultValue);
    }
	;

field_offset
    : integer
    {
        
        $field::fieldOffset=atoi((char*)($integer.text->chars));
    }
    ;

field_name
    : IDENTIFIER
    {
        $field::fieldName=stringDup($IDENTIFIER.text);
    }
    ;

field_type
    : numeric_type
    {
        $field::isNumericType=1;
        $field::fieldType=stringDup($numeric_type.text);
    }
    | array_type
    {
        $field::isNumericType=0;
        $field::fieldType=stringDup($array_type.text);
    }
    | advanced_numeric_type
    {
       $field::isNumericType=1;
       $field::fieldType=stringDup($advanced_numeric_type.text);
    }
    | advanced_array_type
    {
       $field::isNumericType=0;
       $field::fieldType=stringDup($advanced_array_type.text);
    }
    | ( IDENTIFIER
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)<28}?
              UINT32["uint32"] 
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)<=32}?
              UINT32["uint32"] 
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)==64}?
             UINT64["uint64"] 
        -> IDENTIFIER )
    {
       $field::isNumericType=(SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$IDENTIFIER.text->chars)!=NULL||
                                SCOPE_TOP(Symbols)->enum_sizes->get(SCOPE_TOP(Symbols)->enum_sizes,$IDENTIFIER.text->chars)!=NULL);
       $field::fieldType=stringDup($IDENTIFIER.text);
    }
    | ( QUALIFIEDIDENTIFIER
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)<28}?
              UINT32["uint32"] 
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)<=32}?
              UINT32["uint32"] 
        -> {SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)!=NULL
            && *(unsigned int*)SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)==64}?
             UINT64["uint64"] 
        -> QUALIFIEDIDENTIFIER[{replaceImportedMessageType(ctx, $QUALIFIEDIDENTIFIER)}] )
    {
       $field::isNumericType=(SCOPE_TOP(Symbols)->flag_sizes->get(SCOPE_TOP(Symbols)->flag_sizes,$QUALIFIEDIDENTIFIER.text->chars)!=NULL||
                                SCOPE_TOP(Symbols)->enum_sizes->get(SCOPE_TOP(Symbols)->enum_sizes,$QUALIFIEDIDENTIFIER.text->chars)!=NULL);
       $field::fieldType=filterImportedMessageType($QUALIFIEDIDENTIFIER.text);
    }
    ;
multiplicitive_type
    : 
    multiplicitive_advanced_type 
    {
       $field::fieldType=stringDup($multiplicitive_advanced_type.text);        
    }
    ;

array_spec
	:	SQBRACKET_OPEN integer? SQBRACKET_CLOSE
	;

default_value
	:	SQBRACKET_OPEN DEFAULT EQUALS literal_value SQBRACKET_CLOSE
    {
        $field::defaultValue=defaultValuePreprocess(ctx, $field::fieldType, $literal_value.text);
    }
	;

numeric_type:		UINT32
	|	INT32
	|	SINT32
	|	FIXED32
	|	SFIXED32
	|	UINT64
	|	INT64
	|	SINT64
	|	FIXED64
	|	SFIXED64
	|	FLOAT
	|	DOUBLE
	|	BOOL
	;
array_type:	STRING
	|	BYTES
	;

multiplicitive_advanced_type:
    |   NORMAL -> FLOAT["float"]
    |   VECTOR2F -> FLOAT["float"]
    |   VECTOR2D -> DOUBLE["double"]
    |   VECTOR3F -> FLOAT["float"]
    |   VECTOR3D -> DOUBLE["double"]
    |   VECTOR4F -> FLOAT["float"]
    |   VECTOR4D -> DOUBLE["double"]
    |   QUATERNION -> FLOAT["float"]
    |   BOUNDINGSPHERE3F -> FLOAT["float"]
    |   BOUNDINGSPHERE3D -> DOUBLE["double"]
    |   BOUNDINGBOX3F3F -> FLOAT["float"]
    |   BOUNDINGBOX3D3F -> DOUBLE["double"]
    ;

advanced_numeric_type:	UINT8 -> UINT32["uint32"]
	|	INT8 -> INT32["int32"]
	|	SINT8 -> SINT32["sint32"]
	|	FIXED8 -> INT32["uint32"]
	|	SFIXED8 -> INT32["sint32"]
	|	INT16 -> INT32["int32"]
	|	SINT16 -> SINT32["sint32"]
	|	FIXED16 -> INT32["uint32"]
	|	SFIXED16 -> INT32["sint32"]
    |   UINT16 -> UINT32["uint32"]
    |   ANGLE -> FLOAT["float"]
    |   SOLIDANGLE -> FLOAT["float"]
    |   TIME -> FIXED64["fixed64"]
    |   DURATION -> SFIXED64["sfixed64"]
    ; 

advanced_array_type:	   UUID -> BYTES["bytes"]
    |   SHA256 -> BYTES["bytes"]
    ; 

literal_value
	:	HEX_LITERAL
    |   DECIMAL_LITERAL
    |   OCTAL_LITERAL
    |   FLOATING_POINT_LITERAL
    |   BOOL_LITERAL
    |   STRING_LITERAL
    ;

PACKAGELITERAL :    'package';
IMPORTLITERAL :     'import';

DOT :  '.';

// Message elements
MESSAGE	:	'message';
EXTEND	:	'extend';
EXTENSIONS : 'extensions';
RESERVE : 'reserve';
TO : 'to';
// Enum elements
ENUM	:	'enum';

flags : 
     FLAGS8
     {
        $flags_def::flagBits=8;
     }
     |
     FLAGS16
     {
        $flags_def::flagBits=16;
     }
     |
     FLAGS32
     {
        $flags_def::flagBits=32;
     }
     |
     FLAGS64
     {
        $flags_def::flagBits=64;
     }

     ;
// Flags elements
FLAGS8	:	'flags8';
FLAGS16	:	'flags16';
FLAGS32	:	'flags32';
FLAGS64	:	'flags64';

// Field elements
REQUIRED:	'required';
PBJOPTIONAL:	'optional';
REPEATED:	'repeated';

DEFAULT	:	'default';


EQUALS	:	'=';

// Common block elements
BLOCK_OPEN	:	'{';
BLOCK_CLOSE	:	'}';

ITEM_TERMINATOR
	:	';';

// Type elements
UINT8	:	'uint8';
INT8	:	'int8';
SINT8	:	'sint8';
FIXED8	:	'fixed8';
SFIXED8	:	'sfixed8';
UINT16	:	'uint16';
INT16	:	'int16';
SINT16	:	'sint16';
FIXED16	:	'fixed16';
SFIXED16:	'sfixed16';
UINT32	:	'uint32';
INT32	:	'int32';
SINT32	:	'sint32';
FIXED32	:	'fixed32';
SFIXED32:	'sfixed32';
UINT64	:	'uint64';
INT64	:	'int64';
SINT64	:	'sint64';
FIXED64	:	'fixed64';
SFIXED64:	'sfixed64';
FLOAT	:	'float';
DOUBLE	:	'double';
BOOL	:	'bool';
BYTES   :   'bytes';
STRING   :   'string';

UUID : 'uuid';
SHA256 : 'sha256';
ANGLE : 'angle';
SOLIDANGLE : 'solidangle';
TIME : 'time';
DURATION : 'duration';
NORMAL : 'normal';
VECTOR2F : 'vector2f';
VECTOR2D : 'vector2d';
VECTOR3F : 'vector3f';
VECTOR3D : 'vector3d';
VECTOR4F : 'vector4f';
VECTOR4D : 'vector4d';
QUATERNION : 'quaternion';
BOUNDINGSPHERE3F : 'boundingsphere3f';
BOUNDINGSPHERE3D : 'boundingsphere3d';
BOUNDINGBOX3F3F : 'boundingbox3f3f';
BOUNDINGBOX3D3F : 'boundingbox3d3f';


SQBRACKET_OPEN	:	'[';
SQBRACKET_CLOSE	:	']';

integer
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | OCTAL_LITERAL
    ;

STRING_LITERAL
    :  '"' STRING_GUTS '"'
    ;

fragment
STRING_GUTS :	( EscapeSequence | ~('\\'|'"') )* ;

BOOL_LITERAL
    : 'true'
    | 'false'
    ;

HEX_LITERAL : '0' ('x'|'X') HexDigit+ ;

DECIMAL_LITERAL : ('0' | '1'..'9' '0'..'9'*) ;

OCTAL_LITERAL : '0' ('0'..'7')+ ;

fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;


FLOATING_POINT_LITERAL
    :   ('0'..'9')+ '.' ('0'..'9')* Exponent?
    |   '.' ('0'..'9')+ Exponent?
    |   ('0'..'9')+ Exponent
    ;

fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;


fragment
EscapeSequence
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   OctalEscape
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;


IDENTIFIER : ('a'..'z' |'A'..'Z' |'_' ) ('a'..'z' |'A'..'Z' |'_' |'0'..'9' )* ;


QUALIFIEDIDENTIFIER : ('a'..'z' |'A'..'Z' |'_' ) ('a'..'z' |'A'..'Z' |'_' | '.' |'0'..'9' )* ;

COMMENT	: '//' .* '\n' {$channel=HIDDEN;}
        | '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
        ;

WS       : (' '|'\t'|'\n'|'\r')+ {$channel=HIDDEN;} ;
