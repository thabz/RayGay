
#include "webserver/webserver.h"
#include "exception.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <sstream>
#include <iostream>

extern "C" {
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <arpa/inet.h>        /*  inet (3) funtions         */
}

////////////////////////////////////////////////////////////////////////
// WebServer
////////////////////////////////////////////////////////////////////////

Webserver::Webserver(int port, string document_root) 
{
    this->port = port;
    sock = socket(AF_INET, SOCK_STREAM, 0);
            
    struct sockaddr_in sin;
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(port);
    if (bind(sock, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
        throw_exception("Can't bind to port");            
    };
    file_action = new FileAction(document_root);
}

Webserver::~Webserver() {
    cout << "\rWebserver shutting down." << endl;        
    close(sock);            
}

void Webserver::run() {
    listen(sock, 5);
    cout << "HTTP server listening on port " << port << endl;

    while(1) { 
        int s;
        FILE* f;
        s = accept(sock,NULL,NULL);
        if (s < 0) {
            break;        
        }
        if (fork() == 0) {
            f = fdopen(s, "r+");
            process(f);
            fclose(f);   
            close(s);
            exit(EXIT_SUCCESS);
        }
        close(s);
    }
    close(sock);
}

int Webserver::process(FILE* f) {
    char buf[4096];
    char *method;
    char *path;
    char *protocol;
              
    if (!fgets(buf, sizeof(buf), f)) 
       return -1;
    printf("%s", buf);

    method = strtok(buf, " ");
    path = strtok(NULL, " ");
    protocol = strtok(NULL, "\r");
    if (!method || !path || !protocol) 
        return -1;
    
    // Extract request
    HTTPRequest request;
    request.method = string(method);
    request.path = string(path);
    request.readHeaders(f);
    request.readParams(f);
    request.setBody(f);

    // Find and execute action
    Action* action = actions[path];
    if (action == NULL) {
        action = file_action;
    }
    HTTPResponse response = action->execute(request);   
    response.addHeader("Server", string("RayGay Renderslave ") + string(VERSION));
    
    // Send response
    fseek(f, 0, SEEK_CUR); // Force change of stream direction    
    fprintf(f, "HTTP/1.0 %d %s\r\n", response.status, response.statusString().c_str());
    response.writeHeaders(f);
    
    if (response.bodyFILE != NULL) {
        WebUtil::copy(response.bodyFILE, f);
        fclose(response.bodyFILE);        
    } else {
        fprintf(f, "%s", response.bodyString.c_str());
    }
    return 0;
}

void Webserver::addAction(string url, Action* action) {
    actions[url] = action;        
}

////////////////////////////////////////////////////////////////////////
// FileAction
////////////////////////////////////////////////////////////////////////

FileAction::FileAction(string document_root)
{
    this->document_root = document_root;        
}

HTTPResponse FileAction::execute(const HTTPRequest& request)
{ 
    HTTPResponse response;        
    struct stat statbuf;
    string path = document_root + request.path;
    // TODO: Sanitize path, ie. remove ".." and leading "/"
    if (request.method == "GET" || request.method == "HEAD") {
        if (stat(path.c_str(), &statbuf) >= 0) {
            ostringstream os;
            os << statbuf.st_size;         
            // TODO: Send a "Content-MD5" header also
            response = HTTPResponse(200, WebUtil::pathToMimetype(path));
            response.addHeader("Content-Length",os.str());
            if (request.method == "GET") {
               FILE* f = fopen(path.c_str(), "r");
               response.setBody(f);
            }
        } else {
            response = HTTPResponse(404, "text/plain");
        }
    } else if (request.method == "DELETE") {
        // This method isn't strictly HTTP1.1, but we need it
        // and I don't want to implement a complete WebDAV stack.    
        unlink(path.c_str());
        response = HTTPResponse(200, "text/plain");
    } else if (request.method == "PUT") {
            
    } else {
        response = HTTPResponse(405, "text/plain");
        response.addHeader("Allow", "PUT, GET, HEAD, DELETE");    
    }
    return response;
}

