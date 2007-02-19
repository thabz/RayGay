
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpclient.h"
#include "exception.h"
#include <iostream>
#include <csignal>

void sendFile(string filename) {
    HTTPClient client = HTTPClient();
    HTTPRequest request;
    HTTPResponse response;
        
    request.host = "localhost:20000";
    request.method = "PUT";
    request.path = "/" + filename;
    FILE* f = fopen(filename.c_str(),"r");
    if (f == NULL) {
        throw_exception("Couldn't read " + filename);        
    }
    request.setBody(f);

    request.addHeader("Content-type", WebUtil::pathToMimetype(filename));
    request.addHeader("Content-length", WebUtil::filesize(filename));
    response = client.send(request);
    fclose(f);
}

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
        
        sendFile("out.png");
        
        
    } catch (Exception e) {
        std::cout << "Exception: " << e.getMessage() 
                  << " at " << e.getSourceFile() << ":" << e.getSourceLine() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
};
