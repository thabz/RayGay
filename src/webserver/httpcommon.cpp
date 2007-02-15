
#include "webserver/httpcommon.h"

////////////////////////////////////////////////////////////////////////
// HTTPMessage
////////////////////////////////////////////////////////////////////////
HTTPMessage::HTTPMessage() {
    length = -1;
    bodyFILE = NULL;
    bodyString = "";
}

void HTTPMessage::addHeader(string name, string value)
{
    headers[name] = value;
}

string HTTPMessage::getHeader(string name) {
    return headers[name];        
}

void HTTPMessage::setBody(FILE* data) {
    this->bodyFILE = data;        
}

void HTTPMessage::setBody(const string& data) {
    this->bodyString = data;        
}

void HTTPMessage::addBody(const string& data) {
    this->bodyString += data;
}

void HTTPMessage::writeHeaders(FILE* output) {
    for(map<string,string>::iterator ite = headers.begin(); ite != headers.end(); ite++) {
        fprintf(output, "%s: %s\r\n", ite->first.c_str(), ite->second.c_str());    
    }
    fprintf(output,"\r\n");
}

void HTTPMessage::readHeaders(FILE* input) {
    char buf[4096];
    while(1) {
        if (!fgets(buf, sizeof(buf), input)) {
            return;        
        }
        if (buf[0] == '\0' || buf[1] == '\0' || buf[2] == '\0' ) {
            // TODO: Make a better check for empty line        
            return;
        }
        printf("%s",buf);
    }        
}

void HTTPMessage::readParams(FILE* input) {
    if (getHeader("Content-type") == "application/x-www-form-urlencoded") {
        // TODO: Read params from body 
    }
    // TODO: Read additional params from request path
}

////////////////////////////////////////////////////////////////////////
// HTTPResponse
////////////////////////////////////////////////////////////////////////

HTTPResponse::HTTPResponse(int status, string contenttype) {
    this->status = status;
    this->contenttype = contenttype;
    addHeader("Content-type",contenttype);
}

HTTPResponse::HTTPResponse() {
}

string HTTPResponse::statusString() {
    switch(status) {
        case 200: return "OK";
        case 404: return "Not found";
        case 405: return "Method not allowed";
        case 403: return "Forbidden";
        case 501: return "Not supported";
        default:  return "Unknown";
    }
}

////////////////////////////////////////////////////////////////////////
// WebUtil
////////////////////////////////////////////////////////////////////////
string WebUtil::pathToMimetype(string path) {
    const char* ext = strrchr(path.c_str(),'.');
    if (strcmp(ext,".png") == 0) return "image/png";
    if (strcmp(ext,".jpeg") == 0) return "image/jpeg";
    if (strcmp(ext,".jpg") == 0) return "image/jpeg";
    if (strcmp(ext,".txt") == 0) return "text/plain";
    if (strcmp(ext,".html") == 0) return "text/html";
    if (strcmp(ext,".css") == 0) return "text/css";
    return "application/unknown";
}

void WebUtil::copy(FILE* from, FILE* to)
{
    uint8_t buf[4096];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), from)) > 0) {
       fwrite(buf, 1, n, to);
    }
}
