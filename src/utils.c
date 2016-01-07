#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int readInt() {
	int a;
	scanf("%d", &a);
	return a;
}

char* readString() {
	char* string;
	size_t bytes_read;
	int BUFF_SIZE = 200;
	
	//getline(&string, &bytes_read, stdin);
	string = (char *) malloc(BUFF_SIZE + 1);
	bytes_read = getline(&string, &BUFF_SIZE, stdin);
	//scanf("%s\n", string);
	if (bytes_read == -1) {

	}

	return string;
}

void printInt(int a) {
	printf("%d\n", a);
}

void printString(char* c) {
	puts(c);
}

void error() {
	printf("runtime error\n");
	exit(EXIT_FAILURE);
}

char* _concatStrings(char* str1, char* str2) {
	char* nstring = (char*) malloc(strlen(str1) + strlen(str2) + 1);
	strcat(nstring, str1);
	strcat(nstring, str2);
	return nstring;
}
