
#include "stats.h"
#include <iomanip>

using namespace std;


string StatsStrings[] = {
    "Primary rays cast",
    "Secondary rays cast",
    "Shadow rays cast",
    "Shadow cache hint hit",
    "Shadow cache voxel hit",
    "Photon rays cast",
    "Photons lost in void",
    "Global photons stored",
    "Caustic photons stored",
    "Kd-tree objects added",
    "Scene objects added",
    "Irradiance cache size",
    "Irradiance cache hits",
    "Irradiance cache misses",
    "Total camera rays cast"
};

/// The unique singleton instance
Stats* Stats::uniqueInstance = NULL;

Stats* Stats::getUniqueInstance() {
    if (uniqueInstance == NULL) {
	uniqueInstance = new Stats();
    }
    return uniqueInstance;
}

Stats::Stats() {
    disabled = false;
    stats = new long[STATS_LAST+1];
    clear();
}

void Stats::put(StatsKey key, long value) {
    if (!disabled) {
	stats[key] = value;
    }
}

long Stats::get(StatsKey key) const {
    return stats[key];
}

void Stats::clear() {
    for(int i = 0; i < STATS_LAST; i++) {
	stats[i] = 0;
    }
}

void Stats::dump() const {
    // Print key/value pairs
    map<string,long>::const_iterator cur_entry;
    for(int i = 0; i < STATS_LAST; i++) {
	if (stats[i] > 0) {
	    cout << StatsStrings[i] << ": " << stats[i] << endl;
	}
    }

    // Print time measures
    map<string,time_t>::const_iterator cur_time;
    for(cur_time = beginTimes.begin();
	    cur_time != beginTimes.end();
	    cur_time++) {
	cout << cur_time->first;
	if (endTimes.find(cur_time->first) != endTimes.end()) {
	    long secs = endTimes.find(cur_time->first)->second - cur_time->second;
	    cout << ": ";
	    cout << setfill('0') << setw(2) << secs / 60;
	    cout << ":";
	    cout << setfill('0') << setw(2) << secs % 60;
	    cout << endl;
	}
    }
}

void Stats::inc(StatsKey key) {
    stats[key]++;

}

void Stats::beginTimer(string key) {
    beginTimes[key] = time(NULL);
}

void Stats::endTimer(string key) {
    endTimes[key] = time(NULL);
}

