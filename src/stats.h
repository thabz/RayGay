
#ifndef STATS_H
#define STATS_H

#include <map>
#include <string>
#include <time.h>
#include <iostream>
#include <vector>
#include <iosfwd>

using namespace std;

/// A helper class for counting statistics
/**
 * The stats class is basically a hashmap of string and incremental
 * counts of the different strings.
 */ 
class Stats {

    public:
	/// Returns the singleton
	static Stats* getUniqueInstance();
	/// Inserts a value
	void put(string key, long value);
	/// Gets a value
	long get(string key) const;
	/// Increases a value by one
	void inc(string key);
	/// Increases a value by an amount
	void inc(string key,long amount);
	/// Prints out the stats
	void dump() const;
	/// Remove all stats
	void clear();


	/// Begin a time measure
	void beginTimer(string key);
	/// End a time measure
	void endTimer(string key);

	/// Switch stats off
	void disable() { disabled = true; };

    private:
	static Stats* Stats::uniqueInstance;
	Stats() {disabled = false; };
	map<string,long> stats;
	map<string,time_t> beginTimes;
	map<string,time_t> endTimes;
        bool disabled;
};


#endif /* STATS_H */
