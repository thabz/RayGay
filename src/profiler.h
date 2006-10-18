
#ifndef RAYGAY_PROFILER
#define RAYGAY_PROFILER

#include <time.h>
#include <vector>
#include <string>

using namespace std;

class Profiler {
    public:
	static Profiler* create(string name, string parent = "") {
	    Profiler* p = new Profiler(name,parent);
	    profilers.push_back(p);
	    return p;
	}
	
	static void dump();

	void begin();
	void end();
	double getSeconds();
	
    private:
	Profiler(string n, string p) : name(n), parent(p)
	{ accumulated_time = 0; };
	clock_t last_begin;
	long accumulated_time;
	string name;
	string parent;
	static vector<Profiler*> profilers;

};

inline
void Profiler::begin() {
    last_begin = clock();
}

inline
void Profiler::end() {
    accumulated_time += (clock() - last_begin);
}

inline
double Profiler::getSeconds() {
    return double(accumulated_time) / double(CLOCKS_PER_SEC);
}

#endif
