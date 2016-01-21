#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void error() {
	printf("runtime error\n");
	exit(EXIT_FAILURE);
}

int readInt() {
	int a;
	int res = scanf("%d\n", &a);
	if (res == 0 || res == EOF) {
		error();
	}
	return a;
}

char* readString() {
	char* string = NULL;
	size_t size;
	getline(&string, &size, stdin);
	string[strlen(string) - 1] = 0;
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

int _eqStrings(char* str1, char* str2) {
	return (strcmp(str1, str2) == 0);
}
