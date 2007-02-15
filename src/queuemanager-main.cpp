
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
        
        request.host = "localhost";
        request.port = 20000;
        request.method = "GET";
        request.path = "/";
        response = client.send(request);
        
    } catch (Exception e) {
        std::cout << "Exception: " << e.getMessage() 
                  << " at " << e.getSourceFile() << ":" << e.getSourceLine() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
};
