
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
	static Stats* getUniqueInstance();
	void put(string key, long value);
	long get(string key) const;
	void inc(string key);
	void dump() const;
	void clear();

    private:
	static Stats* Stats::uniqueInstance;
	Stats() {};
	map<string,long> stats;
};


#endif /* STATS_H */
