#include <antlr3.h>
#include "PBJ.h"
#include "PBJParseUtil.h"

void ANTLR3_CDECL freeSymbolTable(SCOPE_TYPE(Symbols) symtab) {
    symtab->types->free(symtab->types);
}
void initNameSpace(SCOPE_TYPE(NameSpace) symtab) {
    symtab->package=NULL;
    symtab->imports = antlr3ListNew(1);
}
void initSymbolTable(SCOPE_TYPE(Symbols) symtab) {
    symtab->types = antlr3HashTableNew(11);
    symtab->enum_values = antlr3HashTableNew(11);
    symtab->free = freeSymbolTable;
}

void initPackage(SCOPE_TYPE(NameSpace) symtab, pANTLR3_STRING id) {
    symtab->package=id->factory->newRaw(id->factory);
    symtab->package->appendS(symtab->package,id);
}

void stringFree(void* s) {
    pANTLR3_STRING id=(pANTLR3_STRING)s;
    id->factory->destroy(id->factory,id);
}
pANTLR3_STRING stringDup(pANTLR3_STRING s) {
    pANTLR3_STRING retval=s->factory->newRaw(s->factory);
    retval->appendS(retval,s);
    return retval;
}

void initImport(SCOPE_TYPE(NameSpace) symtab, pANTLR3_STRING filename) {
    pANTLR3_STRING s=filename->factory->newRaw(filename->factory);
    s->appendS(s,filename);
    symtab->imports->add(symtab->imports,s,&stringFree);
    
}

void defineType(SCOPE_TYPE(NameSpace) ns, SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING id) {
    if (symtab == NULL) return;
    symtab->types->put(symtab->types, id->chars, id, NULL);
    fprintf(stderr,"define type \%s\n", id->chars);
}

void defineEnum(SCOPE_TYPE(NameSpace) ns, SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING id, pANTLR3_LIST enumValues) {
    if (symtab == NULL) return;
    defineType(ns, symtab,id);
    fprintf(stderr,"define enum value \%s\n", id->chars);
}
void defineEnumValue(SCOPE_TYPE(NameSpace) ns, SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING enumName, pANTLR3_LIST enumValues, pANTLR3_STRING id, pANTLR3_STRING value) {
    if (symtab == NULL) return;
    symtab->enum_values->put(symtab->enum_values, id->chars, id, NULL);
    fprintf(stderr,"define enum value \%s::\%s=\%s\n", enumName->chars,id->chars,value->chars);
}
void defineFlag(SCOPE_TYPE(NameSpace) ns, SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING id, pANTLR3_LIST flagValues) {
    if (symtab == NULL) return;
    defineType(ns, symtab, id);
    fprintf(stderr,"define flag value \%s\n", id->chars);
}

void defineFlagValue(SCOPE_TYPE(NameSpace) ns, SCOPE_TYPE(Symbols) symtab, pANTLR3_STRING flagName, pANTLR3_LIST flagValues, pANTLR3_STRING id, pANTLR3_STRING value) {
    if (symtab == NULL) return;//FIXME
    symtab->enum_values->put(symtab->enum_values, id->chars, id, NULL);
    fprintf(stderr,"define flag value \%s::\%s::\%s=\%s\n", ns->package->chars, flagName->chars,id->chars,value->chars);
}

ANTLR3_BOOLEAN isTypeName(pPBJParser ctx, pANTLR3_UINT8 name) {
    int i;
    for (i = (int)SCOPE_SIZE(Symbols)-1 ; i >= 0; i--) {
        pANTLR3_HASH_TABLE symtab;
        pANTLR3_STRING symbol;
        SCOPE_TYPE(Symbols) symScope;

        symScope = (SCOPE_TYPE(Symbols))SCOPE_INSTANCE(Symbols, i);
        symtab = (pANTLR3_HASH_TABLE) symScope->types;
        symbol = (pANTLR3_STRING) (symtab->get(symtab, (void *)name));

        if (symbol != NULL)
            return ANTLR3_TRUE;
    }
    return ANTLR3_FALSE;
}

void grammarToString	(pANTLR3_TREE_NODE_STREAM tns, pANTLR3_BASE_TREE p, pANTLR3_BASE_TREE stop, pANTLR3_STRING buf)
{

	ANTLR3_UINT32   n;
	ANTLR3_UINT32   c;

	if	(!p->isNilNode(p) )
	{
		pANTLR3_STRING	text;

		text	= p->toString(p);
        if (text == NULL) {
            pANTLR3_COMMON_TOKEN tok=((pANTLR3_COMMON_TREE)(p->super))->token;
            if (tok->strFactory==NULL) {
                tok->strFactory=buf->factory;
                text = tok->getText(((pANTLR3_COMMON_TREE)(p->super))->token);
            }
        }

		if  (text == NULL)
		{
			text = tns->ctns->stringFactory->newRaw(tns->ctns->stringFactory);

			text->addc	(text, ' ');
			text->addi	(text, p->getType(p));
		}

		buf->appendS(buf, text);
	}

	if	(p == stop)
	{
		return;		/* Finished */
	}

	n = p->getChildCount(p);

	if	(n > 0 && ! p->isNilNode(p) )
	{
		buf->addc   (buf, ' ');
		buf->addi   (buf, ANTLR3_TOKEN_DOWN);
	}

	for	(c = 0; c<n ; c++)
	{
		pANTLR3_BASE_TREE   child;

		child = (pANTLR3_BASE_TREE)p->getChild(p, c);
		grammarToString(tns, child, stop, buf);
	}

	if	(n > 0 && ! p->isNilNode(p) )
	{
		buf->addc   (buf, ' ');
		buf->addi   (buf, ANTLR3_TOKEN_UP);
	}
}
