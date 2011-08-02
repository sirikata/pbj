#include "PBJ.h"
extern "C" {
#include "PBJParseUtil.h"
}
#include <sstream>
#include <iostream>
#include <fstream>
#include "PBJLanguageOutput.hpp"

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

int main(int argc, char *argv[])
{

    pANTLR3_UINT8 filename;
    pANTLR3_INPUT_STREAM input;
    pPBJLexer lxr;
    pANTLR3_COMMON_TOKEN_STREAM tstream;
    pPBJParser psr,ctx;
    PBJParser_protocol_return     pbjAST;
    if (argc < 2 || argv[1] == NULL)
        filename = (pANTLR3_UINT8)"./input";
    else
        filename = (pANTLR3_UINT8)argv[1];
    const char * outputFilename="output";
    if (argc>=3) {
        outputFilename=argv[2];
    }
    char * csOut=NULL;
    char * cppOut=NULL;
    char* hppOut = NULL;
    char* fppOut = NULL;
    char * cppInclude=NULL;
    int argindex;
    const char *outputInternalNamespace="_PBJ_Internal";
    const char *outputExternalNamespace="";
    const char *prefix = NULL;
    const char* export_macro = NULL;
    bool delay = false;
    for (argindex=3;argindex<argc;++argindex) {
        if (strncmp(argv[argindex],"--delay",7)==0) {
            delay = true;
        }
        if (strncmp(argv[argindex],"--cpp=",6)==0) {
            cppOut=argv[argindex]+6;
        }
        if (strncmp(argv[argindex],"--hpp=",6)==0) {
            hppOut=argv[argindex]+6;
        }
        if (strncmp(argv[argindex],"--fpp=",6)==0) {
            fppOut=argv[argindex]+6;
        }


        if (strncmp(argv[argindex],"--cs=",5)==0) {
            csOut=argv[argindex]+5;
        }
        if (strncmp(argv[argindex],"--include=",10)==0) {
            cppInclude=argv[argindex]+10;
        }
        if (strncmp(argv[argindex],"--inamespace=",13)==0) {
            outputInternalNamespace=argv[argindex]+13;
            std::string *s = new std::string("_" + std::string(outputInternalNamespace) + "_");
            outputInternalNamespace = s->c_str();
        }
        if (strncmp(argv[argindex],"--namespace=",12)==0) {
            outputExternalNamespace=argv[argindex]+12;
        }
        if (strncmp(argv[argindex],"--prefix=",9)==0) {
            prefix=argv[argindex]+9;
        }
        if (strncmp(argv[argindex],"--export_macro=",15)==0) {
            export_macro=argv[argindex]+15;
        }

    }

    // Kind of a weird option, but necessary due to timestamping issues. This
    // delays creation by 1 second so that the pbj source and the output files
    // (cs, cpp, etc) will have different timestamps so make won't try to
    // re-make them.
    if (delay) {
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
    }

    input = antlr3AsciiFileStreamNew(filename);
    if ( input == NULL ) {
        fprintf(stderr, "Failed to open file %s\n", (char *)filename);
        exit(1);
    }

    lxr = PBJLexerNew(input);
    if ( lxr == NULL ) {
        fprintf(stderr, "Unable to create the lexer due to malloc() failure1\n");
        exit(1);
    }

    tstream = antlr3CommonTokenStreamSourceNew(ANTLR3_SIZE_HINT, TOKENSOURCE(lxr));
    if (tstream == NULL) {
	fprintf(stderr, "Out of memory trying to allocate token stream\n");
	exit(1);
    }

    psr = ctx = PBJParserNew(tstream);
    if (psr == NULL) {
        fprintf(stderr, "Out of memory trying to allocate parser\n");
        exit(ANTLR3_ERR_NOMEM);
    }
    ctx->pPBJParser_NameSpaceTop=NameSpacePush(ctx);
    SCOPE_TOP(NameSpace)->filename=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
    SCOPE_TOP(NameSpace)->filename->append8(SCOPE_TOP(NameSpace)->filename,(const char*)outputFilename);
    SCOPE_TOP(NameSpace)->internalNamespace=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
    SCOPE_TOP(NameSpace)->internalNamespace->append8(SCOPE_TOP(NameSpace)->internalNamespace,(const char*)outputInternalNamespace);
    SCOPE_TOP(NameSpace)->externalNamespace=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
    SCOPE_TOP(NameSpace)->externalNamespace->append8(SCOPE_TOP(NameSpace)->externalNamespace,(const char*)outputExternalNamespace);
    if (prefix == NULL) {
        SCOPE_TOP(NameSpace)->prefix = NULL;
    }
    else {
        SCOPE_TOP(NameSpace)->prefix=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
        //SCOPE_TOP(NameSpace)->prefix->append8(SCOPE_TOP(NameSpace)->prefix, "_");
        SCOPE_TOP(NameSpace)->prefix->append8(SCOPE_TOP(NameSpace)->prefix,(const char*)prefix);
        //SCOPE_TOP(NameSpace)->prefix->append8(SCOPE_TOP(NameSpace)->prefix, "_");
    }
    if (strlen(outputExternalNamespace)) {
        SCOPE_TOP(NameSpace)->externalNamespace->append8(SCOPE_TOP(NameSpace)->externalNamespace,".");
    }
    if(export_macro == NULL)
    {
        std::cout << "herer\n";
        SCOPE_TOP(NameSpace)->export_macro=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
        SCOPE_TOP(NameSpace)->export_macro->append8(SCOPE_TOP(NameSpace)->export_macro,(const char*)" ");


    }
    else
    {
        std::cout << "\nelse\n";
        SCOPE_TOP(NameSpace)->export_macro=tstream->tstream->tokenSource->strFactory->newRaw(tstream->tstream->tokenSource->strFactory);
        SCOPE_TOP(NameSpace)->export_macro->append8(SCOPE_TOP(NameSpace)->export_macro,(const char*)export_macro);

    }

    SCOPE_TOP(NameSpace)->output=(struct LanguageOutputStruct*)malloc(sizeof(struct LanguageOutputStruct));
    std::fstream cppOutStream,csOutStream,hppOutStream,fppOutStream;
    SCOPE_TOP(NameSpace)->output->cs=&std::cerr;
    SCOPE_TOP(NameSpace)->output->cpp=&std::cout;//could open something dependent on filename
    SCOPE_TOP(NameSpace)->output->hpp=&std::cout;
    SCOPE_TOP(NameSpace)->output->fpp=&std::cout;

    if (cppOut) {
        cppOutStream.open(cppOut,std::ios_base::out);
        SCOPE_TOP(NameSpace)->output->cpp=&cppOutStream;
    }

    if(hppOut)
    {
      hppOutStream.open(hppOut, std::ios_base::out);
      SCOPE_TOP(NameSpace)->output->hpp=&hppOutStream;
    }

    if(fppOut)
    {
      fppOutStream.open(fppOut, std::ios_base::out);
      SCOPE_TOP(NameSpace)->output->fpp=&fppOutStream;
    }


    if (csOut) {
        csOutStream.open(csOut,std::ios_base::out);
        SCOPE_TOP(NameSpace)->output->cs=&csOutStream;
    }
    if (cppInclude) {
        (*SCOPE_TOP(NameSpace)->output->cpp)<<"#include \""<<cppInclude<<"\"\n";
    }
    pbjAST=psr->protocol(psr);
    if (psr->pParser->rec->getNumberOfSyntaxErrors(psr->pParser->rec) > 0)
    {
        ANTLR3_FPRINTF(stderr, "The parser returned %d errors, tree walking aborted.\n", psr->pParser->rec->getNumberOfSyntaxErrors(psr->pParser->rec));

    }
    else
    {
        pANTLR3_COMMON_TREE_NODE_STREAM    nodes;
        nodes   = antlr3CommonTreeNodeStreamNewTree(pbjAST.tree, ANTLR3_SIZE_HINT); // sIZE HINT WILL SOON BE DEPRECATED!!
        pANTLR3_STRING s = nodes->stringFactory->newRaw(nodes->stringFactory);
        grammarToString(nodes->tnstream,nodes->root,NULL,s);
        FILE*fp=fopen(outputFilename,"w");
        if (s->size>1)
            fwrite(s->chars,s->size-1,1,fp);
        fclose(fp);
        stringFree(s);
        nodes   ->free  (nodes);        nodes   = NULL;
    }
/*
    if (SCOPE_TOP(NameSpace)->output->cpp)
        fclose(SCOPE_TOP(NameSpace)->output->cpp);
    if (SCOPE_TOP(NameSpace)->output->cs)
        fclose(SCOPE_TOP(NameSpace)->output->cs);
*/
    NameSpacePop(ctx);
    psr->free(psr);
    psr = NULL;

    tstream->free(tstream);
    tstream = NULL;


    lxr->free(lxr);
    lxr = NULL;

    input->close(input);
    input = NULL;

    return 0;
}
