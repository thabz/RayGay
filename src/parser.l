
%{



%}
alpha	[a-zA-Z]
digit	[0-9]
special	[\.\_-]
string	{alpha}({alpha}|{digit}|{special})*
%%
[ \t\n]		;
material	return tMATERIAL;
solidbox	return tSOLIDBOX;
sphere		return tSPHERE;
{digit}+ |
{digit}+"."{digit}+ { yyval.d = atof(yytext); return tFLOAT;}
{string}	{ return tSTRING; }

%%


