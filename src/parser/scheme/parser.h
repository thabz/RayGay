
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

class Parser 
{
    public:
	Parser();
	ParseTree parse(string code);
	ParseTree parse(istream code);

    private:
	// Store a parsetree here

};

#endif

