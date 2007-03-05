
#ifndef RAYGAY_HTTP_COMMON_H
#define RAYGAY_HTTP_COMMON_H

#include <string>
#include <map>

using namespace std;

/// Base class for HTTPRequest and HTTPResponse. A HTTPMessage is a collection
/// of headers and a body.
class HTTPMessage {
   public: 
       HTTPMessage();                   
       void addHeader(string name, string value);
       void addHeader(string name, long value);
       string getHeader(const string& name) const;
       
       void setBody(const string& text);
       void addBody(const string& text);
       void setBody(FILE* data);
       void writeBody(FILE* output) const;
       
       void readHeaders(FILE* input);
       void writeHeaders(FILE* output);
       
       void readParams(FILE* input);
       string getParamAsString(string key);
       long getParamAsLong(string key);

       string contenttype;
       long length;
       FILE* bodyFILE;
       string bodyString;
       map<string,string> headers;
       map<string,string> params;
};

class HTTPResponse : public HTTPMessage {
    public:        
        HTTPResponse();
        HTTPResponse(int status, string contenttype);
        string statusString();
        
        int status;
};

class HTTPRequest : public HTTPMessage {
    public:
       string host;   // Eg. localhost:20000
       string method; // "GET", "POST", etc.
       string path;   // "/index.html", "/", etc.
};

class WebUtil
{
    public:
        static string pathToMimetype(string path);
        static void copy(FILE* from, FILE* to);
        static void copy(FILE* from, FILE* to, unsigned long size);
        static long filesize(string filename);
        static string MD5(string filename);
        static string formatDate(time_t date);
};

#endif

