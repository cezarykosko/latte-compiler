#include<stdio.h>
#include<stdlib.h>
#include<string.h>

int readInt() {
	int a;
	scanf("%d\n", &a);
	return a;
}

void error() {
	printf("runtime error\n");
	exit(EXIT_FAILURE);
}

char* readString() {
	char* string =(char*) malloc (100 * (sizeof(char)));
        scanf("%s\n", string);
        return string;
}

void printInt(int a) {
	printf("%d\n", a);
}

void printString(char* c) {
	printf("%s\n", c);
}

char* _concatStrings(char* str1, char* str2) {
	char* nstring = (char*) malloc(strlen(str1) + strlen(str2) + 1);
	strcat(nstring, str1);
	strcat(nstring, str2);
	return nstring;
}
