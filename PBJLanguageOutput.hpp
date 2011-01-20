extern "C" struct LanguageOutputStruct {
    std::ostream* cpp;
    std::ostream* hpp;
    std::ostream* npp;
    std::ostream* cs;
};

extern "C" struct CsStreams {
    std::stringstream* csType;
    std::stringstream* csBuild;
    std::stringstream* csMembers;
};

