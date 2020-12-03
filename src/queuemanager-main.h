
#ifndef RAYGAY_QUEMANAGER_H
#define RAYGAY_QUEMANAGER_H

#include "http/httpclient.h"
#include "image/image.h"
#include <set>
#include <vector>

extern "C" {
#include <sys/types.h>
}

using namespace std;

class QueueJob {
  // time_t begin;
};

class QueueFrameJob : public QueueJob {
public:
  uint32_t frame;
  uint32_t frames;
  Image *result;
};

class QueueTileJob : public QueueJob {
public:
  uint32_t x, y;
  uint32_t w, h;
  Image *result;
};

class QueueMaster;

class QueueSlave {
public:
  QueueSlave(string host_and_port, QueueMaster *master);
  ~QueueSlave();
  string getProgress();
  void run();

private:
  QueueMaster *master;
  string progress;
  HTTPClient *client;
};

class QueueMaster {
public:
  QueueMaster(set<string> hosts, vector<QueueJob> jobs);
  void run();
  QueueJob get();
  void done(QueueJob job);

private:
  set<string> hosts;
  vector<QueueJob> jobs_to_do;
  vector<QueueJob> jobs_being_done;
  vector<QueueSlave *> slaves;
};

#endif /* RAYGAY_QUEMANAGER_H */
