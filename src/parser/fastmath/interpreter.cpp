
#include "interpreter.h"
#include "bytecode.h"
#include <cmath>

Interpreter::Interpreter(const Program& program)
{
    this->program = program;
    pc = 0;
}

void Interpreter::reset();
{
    program->reset();
}

double Interpreter::run()
{
    ByteCode bytecode;
    double tmp;
    double stack[1000];
    double* sp = &stack;
    program->reset();

    while(opcode = program->nextOpcode()) {
	switch(opcode) {
	    case NOP:
		break;
	    case PUSH:
		sp++;
		*sp = program->nextOperand();
		break;
	    case ADD:
		tmp = *sp;
		sp--;
		*sp += tmp;
		break;
	    case SUB:
		tmp = *sp;
		sp--;
		*sp -= tmp;
		break;
	    case MULT:
		tmp = *sp;
		sp--;
		sp *= tmp;
		break;
	    case DIV:
		tmp = *sp;
		sp--;
		*sp /= tmp;
		break;
	    case SIN:
		*sp = sin(*sp);
		break;
	    case COS:
		*sp = cos(*sp);
		break;
	    case SQRT:
		*sp = sqrt(*sp);
		break;
	}
    }
    return *sp;
}

