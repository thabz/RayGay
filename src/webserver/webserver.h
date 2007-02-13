
#ifndef RENDERER_WEBSERVER_H
#define RENDERER_WEBSERVER_H

#include <string>
#include <map>
#include <vector>

using namespace std;

class HTTPResponse {
    public:        
        HTTPResponse();
        string statusString(int status);
        void addHeader(string name, string value);
        void sendHeaders(int status, const char* contenttype);
        void sendText(const string& text);
        void sendFile(FILE* file);
        
        long length;
        FILE* output;
        vector<pair<string,string> > extra_headers;
};

class HTTPRequest {
    public:        
       string method; // "GET", "POST", etc.
       string path;
};

class Action {
   public:
       virtual void execute(const HTTPRequest& request, HTTPResponse& response) = 0;
       virtual ~Action() {};
};

class FileAction : public Action {
   public:
       FileAction(string document_root);           
       void execute(const HTTPRequest& request, HTTPResponse& response);
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
        void send(FILE* f, HTTPResponse& response);
        int process(FILE* f);
        void readHeaders(FILE* f, HTTPRequest& request);
        
        map<string,Action*> actions;
        int sock;
        int port;
        FileAction* file_action;
};

class WebUtil
{
    public:
        static string pathToMimetype(string path);            
};

#endif