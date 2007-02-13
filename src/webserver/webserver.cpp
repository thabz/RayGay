
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
    
    Action* action = actions[path];
    HTTPRequest request;
    request.method = string(method);
    request.path = string(path);
    readHeaders(f, request);
    HTTPResponse response;
    response.output = f;
    if (action == NULL) {
        action = file_action;
    }
    action->execute(request, response);   
    return 0;
}

void Webserver::addAction(string url, Action* action) {
    actions[url] = action;        
}

void Webserver::readHeaders(FILE* f, HTTPRequest& request) {
    char buf[4096];
    while(1) {
        if (!fgets(buf, sizeof(buf), f)) {
            return;        
        }
        if (buf[0] == '\0' || buf[1] == '\0' || buf[2] == '\0' ) {
            return;
        }
        cout << "Header: " << string(buf) << endl;
    }        
}

////////////////////////////////////////////////////////////////////////
// HTTPResponse
////////////////////////////////////////////////////////////////////////

void HTTPResponse::sendHeaders(int status, const char* contenttype)
{
    FILE* f = output;
    fseek(f, 0, SEEK_CUR); // Force change of stream direction    
    fprintf(f, "HTTP/1.0 %d %s\r\n", status, statusString(status).c_str());
    fprintf(f, "Server: RayGay Renderslave %s\r\n",VERSION);
    if (contenttype != NULL) {
        fprintf(f, "Content-type: %s\r\n", contenttype);
    }
    for(uint32_t i = 0; i < extra_headers.size(); i++) {
        pair<string,string> p = extra_headers[i];
        fprintf(f, "%s: %s\r\n", p.first.c_str(), p.second.c_str());    
    }
    fprintf(f,"\r\n");
}

void HTTPResponse::sendText(const string& s)
{
    fprintf(output, "%s", s.c_str());
}

void HTTPResponse::sendFile(FILE* file)
{
    uint8_t data[4096];
    size_t n;
    while ((n = fread(data, 1, sizeof(data), file)) > 0) {
       fwrite(data, 1, n, output);
    }
}


HTTPResponse::HTTPResponse() {
    length = -1;
}

string HTTPResponse::statusString(int status) {
    switch(status) {
        case 200: return "OK";
        case 404: return "Not found";
        case 405: return "Method not allowed";
        case 403: return "Forbidden";
        case 501: return "Not supported";
        default:  return "Unknown";
    }
}

void HTTPResponse::addHeader(string name, string value)
{
    extra_headers.push_back(pair<string,string>(name,value));        
}

////////////////////////////////////////////////////////////////////////
// WebUtil
////////////////////////////////////////////////////////////////////////
string WebUtil::pathToMimetype(string path) {
    if (path.rfind(".png") != path.npos) {
        return "image/png";
    } else if (path.rfind(".jpeg") != path.npos) {
        return "image/jpeg";
    } else if (path.rfind(".jpg") != path.npos) {
        return "image/jpeg";
    } else if (path.rfind(".txt") != path.npos) {
        return "text/plain";
    } else {
        return "application/unknown";
    }        
}

////////////////////////////////////////////////////////////////////////
// FileAction
////////////////////////////////////////////////////////////////////////

FileAction::FileAction(string document_root)
{
    this->document_root = document_root;        
}

void FileAction::execute(const HTTPRequest& request, HTTPResponse& response)
{
    struct stat statbuf;
    string path = document_root + request.path;
    // TODO: Sanitize path, ie. remove ".." and leading "/"
    if (request.method == "GET" || request.method == "HEAD") {
        if (stat(path.c_str(), &statbuf) >= 0) {
            ostringstream os;
            os << statbuf.st_size;         
            response.addHeader("Content-Length",os.str());
            // TODO: Send a "Content-MD5" header also
            response.sendHeaders(200,WebUtil::pathToMimetype(path).c_str());        
            if (request.method == "GET") {
               FILE* f = fopen(path.c_str(), "r");
               response.sendFile(f);
               fclose(f);
            }
        } else {
            response.sendHeaders(404, "text/plain");
        }
    } else if (request.method == "PUT") {
            
    } else {
        response.addHeader("Allow", "PUT, GET, HEAD");    
        response.sendHeaders(405, "text/plain");
    }
}

