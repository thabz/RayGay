
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpclient.h"
#include "exception.h"

#include <iostream>
#include <cstdio>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

HTTPFormPost::HTTPFormPost() {
    method = "POST";
    addHeader("Content-type","application/x-www-form-urlencoded");
}

void HTTPFormPost::addParameter(string name, string value) {
    string p = name + "=" + value;
    if (bodyString != "") {
        p = "&" + p;
    }
    addBody(p);
}


HTTPClient::HTTPClient() {

}

HTTPResponse HTTPClient::send(HTTPRequest& request)
{
    HTTPResponse response;
    struct sockaddr_in serv_addr;
    int sock;
    char buf[4096];

    request.addHeader("User-agent", string("RayGay Queuemanager ")+string(VERSION));
    request.addHeader("Host", request.host);

    // Split request.host (format "host:port") into char* host and int port
    strncpy(buf, request.host.c_str(), sizeof(buf));
    char* bufp = buf;
    char* hostname = strsep(&bufp, ":");
    int port = (int) strtol(bufp, NULL, 10);


    bzero((uint8_t*) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);

    struct hostent* server;
    if (inet_addr(hostname) == INADDR_NONE) {
        // hostname needs DNS lookup    
        server = gethostbyname(hostname);
        if (server == NULL) {
            throw_exception("No such host");
        }
    } else {
        // hostname is an IP-address
        in_addr_t addr = inet_addr(hostname);
        server = gethostbyaddr((char*)&addr, sizeof(addr), AF_INET);            
    }
    bcopy((uint8_t*)server->h_addr, (uint8_t*)&serv_addr.sin_addr.s_addr, server->h_length);

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(sock, (struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) {
        close(sock);
        throw_exception("Error connecting");
    }

    // Send request
    FILE* f = fdopen(sock, "r+");
    fprintf(f, "%s %s HTTP/1.1\r\n", request.method.c_str(), request.path.c_str());
    request.writeHeaders(f);
    request.writeBody(f);
    cout << "Request sent" << endl;
    
    // Read response
    fseek(f, 0, SEEK_CUR); // Force change of stream direction
    cout << "Request sent 2" << endl;
    if (!fgets(buf, sizeof(buf), f)) {
        fclose(f);
        close(sock);
        throw_exception("Error reading response");
    }
    cout << "Request sent 3" << endl;
    printf("%s", buf);

    char* protocol = strtok(buf, " ");
    char* status = strtok(NULL, " ");
    strtok(NULL, "\r");
    response.status = atoi(status);
    response.readHeaders(f);
    response.readParams(f);
    response.setBody(f);
    fclose(f);
    close(sock);

    return response;
}