
#ifndef STATS_H
#define STATS_H

#include <map>
#include <string>
#include <time.h>
#include <iostream>
#include <vector>
#include <iosfwd>

using namespace std;

enum StatsKey {
    STATS_PRIMARY_RAYS_CAST,
    STATS_SECONDARY_RAYS_CAST,
    STATS_SHADOW_RAYS_CAST,
    STATS_SHADOW_HINT_HIT,
    STATS_SHADOW_VOXEL_HIT,
    STATS_PHOTON_RAYS_TRACED,
    STATS_PHOTONS_LOST_IN_VOID,
    STATS_GLOBAL_PHOTONS_STORED,
    STATS_CAUSTIC_PHOTONS_STORED,
    STATS_KDTREE_OBJECTS_ADDED,
    STATS_SCENE_OBJECTS_ADDED,
    STATS_IRRADIANCE_CACHE_SIZE,
    STATS_IRRADIANCE_CACHE_HITS,
    STATS_IRRADIANCE_CACHE_MISSES,
    STATS_TOTAL_CAMERA_RAYS_CAST,
    STATS_THREADS,
    STATS_KDTREE_DEPTH,
    STATS_KDTREE_NODES,
    STATS_LAST
};

/**
 * A helper class for counting statistics
 *
 * \todo inline constructor and getUniqueInstance as done in environment.h
 */ 
class Stats {

    public:
	/// Returns the singleton
	static Stats* getUniqueInstance();
	/// Inserts a value
	void put(StatsKey, long value);
	/// Gets a value
	long get(StatsKey key) const;
	/// Increases a value by one
	void inc(StatsKey key);
	/// Increases a value by an amount
	void inc(StatsKey key, long amount);
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
	Stats();
	static Stats* Stats::uniqueInstance;
	long* stats;
	map<string,time_t> beginTimes;
	map<string,time_t> endTimes;
	bool disabled;
};

#endif /* STATS_H */
