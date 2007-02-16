
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpclient.h"
#include "exception.h"
#include <iostream>
#include <csignal>

int main(int argc, char *argv[]) {
    try {
        HTTPClient client = HTTPClient();
        HTTPRequest request;
        HTTPResponse response;
        
        request.host = "localhost:20000";
        request.host = "127.0.0.1:20000";
        request.method = "GET";
        request.path = "/";
        request.setBody("a=b&c=10&e=ddddd\r\n");
        request.addHeader("Content-type","application/x-www-form-urlencoded");
        response = client.send(request);
        
    } catch (Exception e) {
        std::cout << "Exception: " << e.getMessage() 
                  << " at " << e.getSourceFile() << ":" << e.getSourceLine() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
};
