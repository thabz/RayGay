
#include "stats.h"

using namespace std;

Stats* Stats::uniqueInstance = NULL;

Stats* Stats::getUniqueInstance() {
    if (uniqueInstance == NULL) {
	uniqueInstance = new Stats();
    }
    return uniqueInstance;
}

void Stats::put(string key, long value) {
    if (disabled) 
	return;
    stats[key] = value;
}

long Stats::get(string key) const {
    map<string,long>::const_iterator p = stats.find(key);
    if (p != stats.end()) {
	return p->second;
    } else {
	return 0;
    }
}

void Stats::inc(string key) {
    if (disabled) 
	return;
    this->inc(key,1);
}

void Stats::inc(string key, long amount) {
    if (disabled) 
	return;
    map<string,long>::const_iterator p = stats.find(key);
    if (p != stats.end()) {
	(stats.find(key)->second) += amount;
    } else {
	put(key,amount);
    }
}

void Stats::clear() {
    stats.clear();
}

void Stats::dump() const {
    map<string,long>::const_iterator cur_entry;
    for (cur_entry = stats.begin();
	    cur_entry != stats.end();
	    cur_entry++) {
	cout << cur_entry->first << ": " << cur_entry->second << endl;
    }
}
