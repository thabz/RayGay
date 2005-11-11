
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
    "Total camera rays cast",
    "Threads",
    "Kd-tree depth",
    "Kd-tree nodes",
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
    map<string,clock_t>::const_iterator cur_time;
    for(cur_time = beginTimes.begin(); cur_time != beginTimes.end(); cur_time++) {
	cout << cur_time->first;
	if (endTimes.find(cur_time->first) != endTimes.end()) {
	    long secs = double(endTimes.find(cur_time->first)->second - cur_time->second) / CLOCKS_PER_SEC;
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
    beginTimes[key] = clock();
}

void Stats::endTimer(string key) {
    endTimes[key] = clock();
}

///////////////////////////////////////////////////
// Statistics
///////////////////////////////////////////////////

vector<Statistics*> Statistics::stats;

Statistics::Statistics(string g, string n) 
{
    group = g;
    name = n;
    stats.push_back(this);
}

void Statistics::put(string group, string name, double value)
{
    CounterStats* c = new CounterStats(group,name);
    c->put(value);
}

Statistics::~Statistics() 
{

}

void Statistics::dumpAll()
{
    for(uint32_t i = 0; i < stats.size(); i++) {
	cout << stats[i]->group << "/" << stats[i]->name << ": ";
	stats[i]->out();
	cout << endl;
    }
}

///////////////////////////////////////////////////
// TimerStats 
///////////////////////////////////////////////////

TimerStats::TimerStats(string group, string name) : Statistics(group,name)
{
}

void TimerStats::startTimer()
{
    begin_time = clock();
}

void TimerStats::stopTimer()
{
    end_time = clock();
}

void TimerStats::out() const 
{
    long secs = double(end_time - begin_time) / CLOCKS_PER_SEC;
    cout << setfill('0') << setw(2) << secs / 60;
    cout << ":";
    cout << setfill('0') << setw(2) << secs % 60;
}

///////////////////////////////////////////////////
// CounterStats 
///////////////////////////////////////////////////

CounterStats::CounterStats(string group, string name) : Statistics(group,name)
{
    value = 0.0;
}

void CounterStats::inc() 
{
    value++;
}

void CounterStats::inc(double amount)
{
    value += amount;
}

void CounterStats::put(double v)
{
    value = v;
}

void CounterStats::out() const {
    cout << long(value);
}

///////////////////////////////////////////////////
// PercentageStats
///////////////////////////////////////////////////

PercentageStats::PercentageStats(string g, string n) : CounterStats(g,n)
{
    total = 1.0;
}

void PercentageStats::setTotal(double value)
{
    total = value;
}

void PercentageStats::out() const 
{
    if (total != 0.0) {
	double percentage = 100.0 * value / total;
	cout << percentage << "%";
    } else {
	cout << "N/A";
    }
}

