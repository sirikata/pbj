SIRIKATA = "../../"
ANTLR_INCLUDE="${SIRIKATA}/dependencies/installed-antlr/include"
ANTLR_LIB="${SIRIKATA}/dependencies/installed-antlr"
tpbj: output.pbj.hpp output.pb.h test.cpp pbj.hpp cspbj.exe
	g++ -m32 -std=c++98 -Wall -g -o tpbj test.cpp output.pb.cc ${SIRIKATA}/dependencies/installed-protobufs/lib/libprotobuf.a -Wall -I${SIRIKATA}/dependencies/include	-I${SIRIKATA}/libcore/src -I${SIRIKATA}//dependencies/installed-protobufs/include -I${SIRIKATA}/dependencies/include/boost-1_35 -I${SIRIKATA}/dependencies/installed-boost/include/boost-1_37 -lpthread

cspbj.exe: Output.pbj.cs Output.cs Test.cs pbj.hpp
	${SIRIKATA}/dependencies/installed-mono/bin/gmcs -r:${SIRIKATA}/dependencies/installed-protobufs/bin/Google.ProtocolBuffers.dll Output.pbj.cs Output.cs Test.cs PBJ.cs -out:cspbj.exe
output.pb.h: output.pbj.hpp
	${SIRIKATA}/dependencies/installed-protobufs/bin/protoc --cpp_out=. output.proto
Output.cs:output.proto
	${SIRIKATA}/dependencies/installed-protobufs/bin/protoc --descriptor_set_out=output.protobin -I. output.proto
	${SIRIKATA}/dependencies/installed-mono/bin/mono ${SIRIKATA}/dependencies/installed-protobufs/bin/ProtoGen.exe output.protobin
output.pbj.cs: pbj output.proto protocol/Test.pbj
	./pbj protocol/Test.pbj output.proto 1> output.pbj.hpp 2> Output.pbj.cs
output.proto: pbj protocol/Test.pbj
	./pbj protocol/Test.pbj output.proto 1> output.pbj.hpp 2> Output.pbj.cs
output.pbj.hpp: pbj output.proto protocol/Test.pbj
	./pbj protocol/Test.pbj output.proto 1> output.pbj.hpp 2> Output.pbj.cs
pbj : main.cpp PBJ.h PBJLexer.o PBJParser.o PBJParseUtil.o
	g++ -std=c++98 -Wall -static -g2 -o pbj -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -L${ANTLR_LIB} -I/usr/local/include -L/usr/local/lib main.cpp PBJLexer.o PBJParser.o PBJParseUtil.o ${ANTLR_LIB}/.libs/libantlr3c.a || g++ -o pbj -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -L${ANTLR_LIB} -I/usr/local/include -L/usr/local/lib -g2 main.cpp PBJLexer.o PBJParser.o PBJParseUtil.o ${ANTLR_LIB}/.libs/libantlr3c.a || g++ -o pbj -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -L${ANTLR_LIB} -I/usr/local/include -L/usr/local/lib -g2 main.cpp PBJLexer.o PBJParser.o PBJParseUtil.o ${ANTLR_LIB}/.libs/libantlr3c.a || g++ -std=c++98 -Wall -static -g2 -o pbj -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -L${ANTLR_LIB}/libantlr3c.la -I/usr/local/include -L/usr/local/lib main.cpp PBJLexer.o PBJParser.o PBJParseUtil.o  

PBJLexer.c : PBJ.g
	java -cp "antlr-3.1.3.jar" org.antlr.Tool PBJ.g

PBJParser.c : PBJ.g
	java -cp antlr-3.1.3.jar org.antlr.Tool PBJ.g

PBJLexer.h : PBJ.g
	java -cp antlr-3.1.3.jar org.antlr.Tool PBJ.g

PBJParser.h : PBJ.g
	java -cp antlr-3.1.3.jar org.antlr.Tool PBJ.g

PBJLexer.o : PBJLexer.h PBJLexer.c
	gcc -c -g2 -Wall -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -I/usr/local/include -o PBJLexer.o PBJLexer.c

PBJParser.o : PBJParser.h PBJParser.c
	gcc -c -g2 -Wall -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -I/usr/local/include -o PBJParser.o PBJParser.c

PBJParseUtil.o : PBJParseUtil.h PBJParseUtil.cpp
	g++ -c -g2 -Wall -I${ANTLR_INCLUDE} -I${ANTLR_LIB} -I/usr/local/include -o PBJParseUtil.o PBJParseUtil.cpp

clean:
	rm -f PBJParseUtil.o PBJLexer.o PBJParser.o PBJLexer.c PBJParser.c main.o
