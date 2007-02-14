
#ifndef RAYGAY_HTTPCLIENT_H
#define RAYGAY_HTTPCLIENT_H

#include "webserver/httpcommon.h"

class HTTPClient {
    HTTPClient();
    HTTPResponse send(const HTTPRequest& request);
        
};

#endif

