
#include "webserver/webserver.h"
#include "exception.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <ctime>
#include <cstdio>
#include <iostream>

extern "C" {
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>        /*  inet (3) funtions         */
}

Webserver::Webserver(int port) 
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
    default_action = new Action();
}

Webserver::~Webserver() {
    cout << "~Webserver called." << endl;        
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
        f = fdopen(s, "r+");
        process(f);
        fclose(f);   
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
    HTTPResponse response;
    HTTPRequest request;
    response.output = f;
    if (action == NULL) {
        action = default_action;
    }
    action->execute(request, response);   
    return 0;
}

void HTTPResponse::sendHeaders(int status, char* contenttype)
{
    FILE* f = output;
    fseek(f, 0, SEEK_CUR); // Force change of stream direction    
    fprintf(f, "HTTP/1.0 %d %s\r\n", status, statusString(status).c_str());
    fprintf(f, "Server: RayGay %s Renderserver\r\n",VERSION);
    if (contenttype != NULL) {
        fprintf(f, "Content-type: %s\r\n", contenttype);
    }
    fprintf(f,"\r\n");
}

void HTTPResponse::sendText(const string& s)
{
    fprintf(output, "%s", s.c_str());
}


void Webserver::addAction(string url, Action* action) {
    actions[url] = action;        
}


HTTPResponse::HTTPResponse() {
    length = -1;
}

string HTTPResponse::statusString(int status) {
    switch(status) {
        case 200: return "OK";
        case 404: return "Not found";
        case 403: return "Forbidden";
        case 501: return "Not supported";
        default:  return "Unknown";
    }
}

void Action::execute(const HTTPRequest& request, HTTPResponse& response)
{
    response.sendHeaders(404, "text/plain");
}

