

#undef assert
void __assert(char* file, char* function, int line, char* test);

#define assert(e)	(__assert(__FILE__,__func__,___LINE___,#e));


