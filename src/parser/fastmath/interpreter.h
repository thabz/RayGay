
#ifndef INTERPRETER_H
#define INTERPRETER_H

class Interpreter 
{
    public:
	Interpreter(const Program& program);
	void reset();
	double run();

    private:
	const Program& program;
	unsigned long pc;
	vector<double> stack;
};

#endif
