
#ifndef RAYGAY_PROFILER
#define RAYGAY_PROFILER

#include <time.h>
#include <vector>
#include <string>

using namespace std;

class Profiler {
    public:
	static Profiler* create(string name, string parent = "");
	
	static void dump();

	void start();
	void stop();
	
    private:
	Profiler(string n, string p);
	static vector<Profiler*> findByParent(string n);
	static uint32_t longestname();
        static void dump(Profiler* p, double percentage, uint32_t indent);
	double secs();

	clock_t last_begin;
	long accumulated_time;
	string name;
	string parent;
	static vector<Profiler*> profilers;
};

inline
void Profiler::start() {
    last_begin = clock();
}

inline
void Profiler::stop() {
    accumulated_time += (clock() - last_begin);
}

#endif
