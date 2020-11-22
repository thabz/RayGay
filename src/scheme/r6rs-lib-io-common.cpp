
#include "r6rs-lib-io-common.h"
#include "scheme.h"

SchemeObject *s_eof_object(Scheme *scheme) { return S_EOF; }

SchemeObject *s_eof_object_p(Scheme *scheme, SchemeObject *o) {
  return bool2scm(o->type() == SchemeObject::EOFTYPE);
}

SchemeObject *s_current_output_port(Scheme *scheme) {
  return scheme->current_output_port;
}

SchemeObject *s_current_input_port(Scheme *scheme) {
  return scheme->current_input_port;
}

SchemeObject *i_input_port_p(SchemeObject *o) {
  return o->type() == SchemeObject::INPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeObject *i_output_port_p(SchemeObject *o) {
  return o->type() == SchemeObject::OUTPUT_PORT ? S_TRUE : S_FALSE;
}

SchemeObject *s_input_port_p(Scheme *scheme, SchemeObject *o) {
  return i_input_port_p(o);
}

SchemeObject *s_output_port_p(Scheme *scheme, SchemeObject *o) {
  return i_output_port_p(o);
}

SchemeObject *i_put_datum(Scheme *scheme, SchemeObject *port, SchemeObject *o) {
  wostream *wos = port->wos;
  if (i_string_p(o) == S_TRUE) {
    (*wos) << o->str;
  } else if (i_char_p(o) == S_TRUE) {
    (*wos) << o->c;
  } else {
    (*wos) << o->toString();
  }
  return S_UNSPECIFIED;
  return S_UNSPECIFIED;
}
