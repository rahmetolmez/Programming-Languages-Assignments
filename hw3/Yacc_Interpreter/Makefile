all:
	flex gpp_interpreter.l
	yacc -d gpp_interpreter.y
	gcc lex.yy.c y.tab.c -o parsed -s

run:
	./parsed

runfile:
	./parsed "helloworld.g++"