all:
	./lein uberjar && gcc -c -m32 src/utils.c -o target/utils.o

clean:
	rm target/utils.o && ./lein clean
