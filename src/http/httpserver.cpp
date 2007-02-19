
#include "http/httpserver.h"
#include "exception.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <sstream>
#include <iostream>
#include <csignal>
#include <cerrno>

extern "C" {
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <arpa/inet.h>        /*  inet (3) funtions         */
#include "http/md5.h"
}

// Allow forked-off processes to die when they're done.
void sigchild(int n) {
    wait3(NULL,WNOHANG,NULL);        
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
        if (errno == EADDRINUSE) {
            throw_exception("Can't bind to port");
        } else if (errno == EINVAL) {
            throw_exception("Something is already bound to port.");
         } else if (errno == EACCES) {
            throw_exception("Not allowed to bind to port.");
        }
    };
    file_action = new FileAction(document_root);
    signal(SIGCHLD,sigchild);
}

Webserver::~Webserver() {
    cout << "\rWebserver shutting down." << endl;        
    if (close(sock) == -1) {
        cout << "\rCouldn't close socket." << endl;        
    };            
}

void Webserver::run() {
    listen(sock, 5);
    cout << "HTTP server listening on port " << port << endl;

    while(1) { 
        int s;
        FILE* f;
        s = accept(sock,NULL,NULL);
        if (s < 0) {
            close(sock);
            return;        
        }
        if (fork() == 0) {
            f = fdopen(s, "r+");
            process(f);
            fclose(f);   
            //close(s);
            exit(EXIT_SUCCESS);
        }
        if (close(s) == -1) {
            printf("close(s) failed\n");        
        };
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

    char* bufp = buf;
    method = strsep(&bufp, " ");
    path = strsep(&bufp, " ");
    protocol = strsep(&bufp, "\r");
    if (!method || !path || !protocol) 
        return -1;
    
    // Extract request
    HTTPRequest request;
    request.method = string(method);
    request.path = string(path);
    request.readHeaders(f);
    request.writeHeaders(stdout);
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
    response.writeBody(f);
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
    char md5_hex[33];
    string path = document_root + request.path;
    // TODO: Sanitize path, ie. remove ".." and leading "/"
    if (request.method == "GET" || request.method == "HEAD") {
        if (stat(path.c_str(), &statbuf) >= 0) {
            ostringstream os;
            os << statbuf.st_size;         
            response = HTTPResponse(200, WebUtil::pathToMimetype(path));
            response.addHeader("Content-Length",os.str());
            FILE* f = fopen(path.c_str(), "r");
            md5_stream_hex(f,md5_hex);
            md5_hex[32] = '\0';
            response.addHeader("Content-MD5",string(md5_hex));
            if (request.method == "GET") {
               rewind(f);
               response.setBody(f);
            } else {
               fclose(f);            
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
        long size = atoi(request.getHeader("Content-length").c_str());
        // TODO: If no content-header is sent, return "411 Length required" as per the RFC. 
        bool exists = stat(path.c_str(), &statbuf) != -1;
        FILE* f = fopen(path.c_str(), "w");
        WebUtil::copy(request.bodyFILE, f, size);
        fclose(f);
        if (exists) {
            response = HTTPResponse(200, "text/plain");
            response.setBody("File modified");
        } else {
            response = HTTPResponse(201, "text/plain");
            response.setBody("File created");
        }
    } else if (request.method == "MKCOL") {    
    } else {
        response = HTTPResponse(405, "text/plain");
        response.addHeader("Allow", "PUT, GET, HEAD, DELETE, MKCOL");    
    }
    return response;
}

