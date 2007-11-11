

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

SchemeObject* Syntax::transcribe(SchemeObject* transformer, SchemeObject* expr) {
    SchemeObject* templat = find_template(transformer, expr);
    if (templat == NULL) {
        throw scheme_exception(L"Invalid construct: " + expr.toString());
    }
    
} 

/**
 * Find the template that correspond to the pattern that matches an expression.
 *
 * @param transformer the body of a syntax-rules
 * @param expr the expression to transform 
 * @return the corresponding template of the pattern that matches the expression
 *         or NULL if none found.
 */
SchemeObject* Syntax::find_template(SchemeObject* transformer, SchemeObject* expr) {
    SchemeObject* literals = i_car(transformer);
    SchemeObject* syntax_rules = i_cdr(transformer);
    // Iterate through all syntax-rules and find the pattern the matches
    // the expr. Then return the corresponding template.
    while (syntax_rules != S_EMPTY_LIST) {
        SchemeObject* syntax_rule = i_car(syntax_rules);
        SchemeObject* pattern = i_car(syntax_rule);
        if (matches(literals, pattern, expr)) {
            // Return the template
            return i_cadr(syntax_rule);
        }
        syntax_rules = i_cdr(syntax_rules);
    }
    return NULL;
}


/**
 * Says whether an input-form F matches a pattern P.
 * 
 * @param literals list of literals
 * @param P pattern
 * @param F input form
 */
bool Syntax::matches(SchemeObject* literals, SchemeObject* P, SchemeObject* F) {
    if (i_pair_p(P) == S_TRUE) {
        if (s_member(P, ellipsis_symbol) == S_TRUE) {
            // P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is 
            // the identifier ... and F is a proper list of at least n forms, 
            // the first n of which match P1 through Pn, respectively, and each 
            // remaining element of F matches Pn+1.
            if (i_pair_p(i_cdr(P)) == S_TRUE && i_cadr(P) == ellipsis_symbol) {
                SchemeObject* Pn1 = i_car(P);
                while (F != S_EMPTY_LIST) {
                    if (!matches(literals, Pn1, i_car(F))) return false;
                    F = i_cdr(F);
                }
                return true;
            } else {
                return i_pair_p(F) == S_TRUE &&
                       matches(literals, i_car(P), i_car(F)) &&
                       matches(literals, i_cdr(P), i_cdr(F));    
            }
        } else {
            // P is a list (P1 ... Pn) and F is a list of n 
            // forms that match P1 through Pn, respectively.
            // --- or ---    
            // P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or 
            // improper list of n or more forms that match P1 through Pn, 
            // respectively, and whose nth ``cdr'' matches Pn+1.
            return i_pair_p(F) == S_TRUE &&
                   matches(literals, i_car(P), i_car(F)) &&
                   matches(literals, i_cdr(P), i_cdr(F));    
        }
    } else (i_vector_p(P) == S_TRUE) {
        // P is a vector of the form #(P1 ... Pn) and F is a vector of n forms that 
        // match P1 through Pn; or
        // --- or ---
        // P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the 
        // identifier ... and F is a vector of n or more forms the first n of which 
        // match P1 through Pn, respectively, and each remaining element of F 
        // matches Pn+1.
    } else if (i_symbol_p(P) == S_TRUE) {
        if (s_member(literals, P)) {
            // P is a literal identifier and F is an identifier with the same binding.
        } else {
            // P is a non-literal identifier.
        }
    } else {
        // P is a datum and F is equal to P in the sense of the equal? procedure.
        return s_equal_p(P, F) == S_TRUE;
    }
}
