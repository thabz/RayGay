
#ifndef RENDERER_WEBSERVER_H
#define RENDERER_WEBSERVER_H

#include <string>
#include <map>

using namespace std;

class HTTPResponse {
    public:        
        HTTPResponse();
        string statusString(int status);
        void sendHeaders(int status, char* contenttype);
        void sendText(const string& text);
        
        long length;
        FILE* output;
};

class HTTPRequest {
      
};


class Action {
   public:
       virtual void execute(const HTTPRequest& request, HTTPResponse& response);
       virtual ~Action() {};
};

class Webserver 
{
    public:
        Webserver(int port);
        ~Webserver();
        void addAction(string url, Action* action);
        void run();
        
    private:
        void send(FILE* f, HTTPResponse& response);
        int process(FILE* f);
        
        map<string,Action*> actions;
        int sock;
        int port;
        Action* default_action;
};

#endif