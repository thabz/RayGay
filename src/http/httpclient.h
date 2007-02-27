
#ifndef RAYGAY_HTTPCLIENT_H
#define RAYGAY_HTTPCLIENT_H

#include "http/httpcommon.h"

class HTTPFormPost : public HTTPRequest {
    public:
        HTTPFormPost();
        void addParameter(string name, string value);
};

class HTTPClient {
    public:
        HTTPClient(string host_and_port);
        HTTPResponse send(HTTPRequest& request);
    private:
        string host_and_port;
        int port;            
        struct hostent* server;

};

#endif

