
#ifndef RAYGAY_HTTPCLIENT_H
#define RAYGAY_HTTPCLIENT_H

#include "webserver/httpcommon.h"

class HTTPFormPost : public HTTPRequest {
    public:
        HTTPFormPost();    
        void addParameter(string name, string value);                    
};



class HTTPClient {
    public:        
        HTTPClient();
        HTTPResponse send(HTTPRequest& request);    
};

#endif

