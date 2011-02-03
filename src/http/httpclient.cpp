
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpclient.h"
#include "exception.h"

#include <iostream>
#include <cstdio>
#include <stdlib.h>
#include <cerrno>
#include <cstring>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define MAX_CONNECT_RETRIES 1000

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


HTTPClient::HTTPClient(string host_and_port) {
    this->host_and_port = host_and_port;

    char buf[4096];

    // Split host_and_port (format "host:port") into char* host and int port
    ::strncpy(buf, host_and_port.c_str(), sizeof(buf));
    char* bufp = buf;
    char* hostname = ::strsep(&bufp, ":");
    port = (int) ::strtol(bufp, NULL, 10);

    if (::inet_addr(hostname) == INADDR_NONE) {
        // hostname needs DNS lookup    
        server = ::gethostbyname(hostname);
        if (server == NULL) {
            throw_exception("No such host");
        }
    } else {
        // hostname is an IP-address
        in_addr_t addr = ::inet_addr(hostname);
        server = ::gethostbyaddr((char*)&addr, sizeof(addr), AF_INET);            
    }

}

HTTPResponse HTTPClient::send(HTTPRequest& request)
{
    HTTPResponse response;
    struct sockaddr_in serv_addr;
    int sock;
    char buf[4096];
    
    request.addHeader("User-agent", string("RayGay Queuemanager ")+string(VERSION));
    request.addHeader("Date", WebUtil::formatDate(time(NULL)));
    request.addHeader("Host", host_and_port);

    ::bzero((uint8_t*) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(port);
    ::bcopy((uint8_t*)server->h_addr, (uint8_t*)&serv_addr.sin_addr.s_addr, server->h_length);

    if ((sock = ::socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        ::printf("errno %d", errno);
        throw_exception("Failure creating socket");    
    }
    
    int retries = 0;
    while (::connect(sock, (struct sockaddr *)&serv_addr,sizeof(serv_addr)) == -1 && retries < MAX_CONNECT_RETRIES) {
        ::usleep(1000);
        retries++;
    }
    if (retries == MAX_CONNECT_RETRIES) {
        printf("errno %d", errno);
        close(sock);
        throw_exception("Connection failure.");
    }
    

    // Send request
    FILE* f = ::fdopen(sock, "r+");
    ::fprintf(f, "%s %s HTTP/1.0\r\n", request.method.c_str(), request.path.c_str());
    request.writeHeaders(f);
    request.writeBody(f);
    
    // Read response
    ::fseek(f, 0, SEEK_CUR); // Force change of stream direction
    if (!::fgets(buf, sizeof(buf), f)) {
        ::fclose(f);
        ::close(sock);
        throw_exception("Error reading response");
    }
    //printf("%s", buf);

    char* protocol = ::strtok(buf, " ");
    char* status = ::strtok(NULL, " ");
    ::strtok(NULL, "\r");
    response.status = ::atoi(status);
    response.readHeaders(f);
    response.readParams(f);
    response.setBody(f);
    ::fclose(f);
    ::close(sock);

    return response;
}
