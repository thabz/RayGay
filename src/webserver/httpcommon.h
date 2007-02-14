
#ifndef RAYGAY_HTTP_COMMON_H
#define RAYGAY_HTTP_COMMON_H

#include <string>
#include <vector>

using namespace std;

class HTTPMessage {
            
};


class HTTPResponse {
    public:        
        HTTPResponse();
        HTTPResponse(int status, string contenttype);
        string statusString();
        void addHeader(string name, string value);
       
        void setBody(const string& text);
        void setBody(FILE* data);
        
        int status;
        long length;
        string contenttype;
        FILE* bodyFILE;
        string bodyString;
        vector<pair<string,string> > headers;
};

class HTTPRequest {
    public:        
       string method; // "GET", "POST", etc.
       string path;
       
       void setBody(const string& text);
       void setBody(FILE* data);
};

class WebUtil
{
    public:
        static string pathToMimetype(string path); 
        static void copy(FILE* from, FILE* to);           
};

#endif