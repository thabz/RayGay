
#ifndef STATS_H
#define STATS_H

#include <map>
#include <string>
#include <iostream>
#include <vector>
#include <iosfwd>

using namespace std;

class Stats {

    public:
	Stats();
	void put(string key, long value);
	long get(string key) const;
	void inc(string key);
	void dump() const;

    private:
	map<string,long> stats;
};

#endif /* STATS_H */
