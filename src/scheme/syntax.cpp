

/*
  Definitions may occur at the beginning of a <body> (that is, the body of a lambda, 
  let, let*, letrec, let-syntax, or letrec-syntax expression or that of a definition 
  of an appropriate form). Such definitions are known as internal definitions as 
  opposed to the top level definitions described above. The variable defined by an 
  internal definition is local to the <body>. That is, <variable> is bound rather 
  than assigned, and the region of the binding is the entire <body>.
*/

/*
 Syntax definitions are valid only at the top level of a <program>.
 There is no define-syntax analogue of internal definitions.
*/ 



/**
 * Says whether an input-form F matches a pattern P.
 * 
 * @param literals list of literals
 * @param P pattern
 * @param F input form
 */
bool Syntax::matches(SchemeObject* literals, SchemeObject* P, SchemeObject* F) {
    if (i_pair_p(pattern) == S_TRUE) {
        if (isImproperList(pattern)) {
            // P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or 
            // improper list of n or more forms that match P1 through Pn, 
            // respectively, and whose nth ``cdr'' matches Pn+1.
        } else if (containsEllipsis(pattern)) {
            // P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is 
            // the identifier ... and F is a proper list of at least n forms, 
            // the first n of which match P1 through Pn, respectively, and each 
            // remaining element of F matches Pn+1.
        } else {
            // P is a list (P1 ... Pn) and F is a list of n 
            // forms that match P1 through Pn, respectively.
        }
    } else (i_vector_p(pattern) == S_TRUE) {
        // P is a vector of the form #(P1 ... Pn) and F is a vector of n forms that 
        // match P1 through Pn; or
        // --- or ---
        // P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the 
        // identifier ... and F is a vector of n or more forms the first n of which 
        // match P1 through Pn, respectively, and each remaining element of F 
        // matches Pn+1.
    } else if (i_symbol_p(pattern) == S_TRUE) {
        if (s_member(literals, pattern)) {
            // P is a literal identifier and F is an identifier with the same binding.
        } else {
            // P is a non-literal identifier.
        }
    } else {
        // P is a datum and F is equal to P in the sense of the equal? procedure.
        return s_equal_p(pattern, input) == S_TRUE;
    }
}

bool isImproperList(SchemeObject* list) {
    while (i_pair_p(list) == S_TRUE) {
        list = i_cdr(list);
    }
    return list != S_EMPTY;
}

bool containsEllipsis(SchemeObject* list) {
    return s_member(list, ellipsis_symbol);
}