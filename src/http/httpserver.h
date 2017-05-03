
#ifndef RENDERER_HTTPSERVER_H
#define RENDERER_HTTPSERVER_H

#include <string>
#include <map>
#include <vector>

#include "http/httpcommon.h"

using namespace std;

class Action {
   public:
       virtual HTTPResponse execute(const HTTPRequest& request) = 0;
       virtual ~Action() {};
};

class FileAction : public Action {
   public:
       FileAction(string document_root);           
       HTTPResponse execute(const HTTPRequest& request);
   private:
       string document_root;               
};

class Webserver 
{
    public:
        Webserver(int port, string document_root);
        ~Webserver();
        void addAction(string url, Action* action);
        void run();
        
    private:
        int process(FILE* f);
        void readHeaders(FILE* f, HTTPRequest& request);
        
        map<string,Action*> actions;
        int sock;
        int port;
        FileAction* file_action;
};

#endif
