
#include "http/httpcommon.h"

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

void HTTPMessage::writeBody(FILE* output) {
    if (bodyFILE != NULL) {
        WebUtil::copy(bodyFILE, output);
        fclose(bodyFILE);        
    } else {
        fprintf(output, "%s", bodyString.c_str());
    }
        
}

void HTTPMessage::readHeaders(FILE* input) {
    char buf[4096];
    char* bufp;
    while(1) {
        if (!fgets(buf, sizeof(buf), input)) {
            return;        
        }
        if (buf[0] == '\0' || buf[1] == '\0' || buf[2] == '\0' ) {
            // TODO: Make a better check for empty line        
            return;
        }
        bufp = buf;
        char* key = strsep(&bufp, ":");
        while(*key == ' ') key++;
        char* value = strsep(&bufp, "\r");
        while(*value == ' ') value++;
        addHeader(string(key),string(value));
    }        
}

void HTTPMessage::readParams(FILE* input) {
    char buf[16*1024];
    char *bufp, *key, *value;
    if (getHeader("Content-type") == "application/x-www-form-urlencoded") {
        if (!fgets(buf, sizeof(buf), input)) {
            printf("No body in form");         
            return;        
        }
        bufp = buf;
        while (1) {
            key = strsep(&bufp, "=");
            if (bufp == NULL) {
                break;    
            }        
            value = strsep(&bufp, "&");
            params[key] = value;
        }
    }
    // TODO: Read additional params from request path
}

string HTTPMessage::getParamAsString(string key) {
    return params[key];        
}

long HTTPMessage::getParamAsLong(string key) {
    return strtol(params[key].c_str(), NULL, 10);
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


