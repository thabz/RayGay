
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpclient.h"
#include "exception.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
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
    struct hostent* server;
    int sock;
    char buf[4096];

    request.addHeader("User-Agent",string("RayGay Queuemanager ")+string(VERSION));

    sock = socket(AF_INET, SOCK_STREAM, 0);
    server = gethostbyname(request.host.c_str());
    if (server == NULL) {
        throw_exception("No such host");
    }
    printf("%s resolved to %d.%d.%d.%d\n", request.host.c_str(), (uint8_t)server->h_addr[0], (uint8_t)server->h_addr[1], (uint8_t)server->h_addr[2], (uint8_t)server->h_addr[3]);

    bzero((uint8_t*) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    bcopy((uint8_t*)server->h_addr, (uint8_t*)&serv_addr.sin_addr.s_addr, server->h_length);
    serv_addr.sin_port = htons(request.port);

    if (connect(sock, (struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) {
        close(sock);
        throw_exception("Error connecting");
    }

    // Send request
    FILE* f = fdopen(sock, "r+");
    fprintf(f, "%s %s HTTP/1.1\r\n", request.method.c_str(), request.path.c_str());
    request.writeHeaders(f);
    request.writeBody(f);

    // Read response
    fseek(f, 0, SEEK_CUR); // Force change of stream direction
    if (!fgets(buf, sizeof(buf), f)) {
        fclose(f);
        close(sock);
        throw_exception("Error reading response");
    }
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