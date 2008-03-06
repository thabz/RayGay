
#include "r6rs-lib-io-ports.h"
#include "r6rs-lib-io-common.h"
#include "scheme.h"


void R6RSLibIOPorts::bind(Scheme* scheme, SchemeObject* envt) {
    // Defined in io-common
	scheme->assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, envt);
	scheme->assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, envt);
	scheme->assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, envt);
	scheme->assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, envt);
	scheme->assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, envt);
}
