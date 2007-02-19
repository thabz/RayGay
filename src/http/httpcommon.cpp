
#include "http/httpcommon.h"
#include "exception.h"
#include "math/constants.h" // For MIN()
#include <iostream>
#include <sstream>
#include <sys/stat.h>


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

void HTTPMessage::addHeader(string name, long value)
{
    ostringstream os;
    os << value;
    headers[name] = os.str();
}


string HTTPMessage::getHeader(const string& name) const {
    return headers.find(name)->second;
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

void HTTPMessage::writeBody(FILE* output) const {
    if (bodyFILE != NULL) {
        WebUtil::copy(bodyFILE, output);
        fclose(bodyFILE);        
    } else {
        fprintf(output, "%s\r\n", bodyString.c_str());
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
        case 201: return "Created";
        case 400: return "Bad request";
        case 403: return "Forbidden";
        case 404: return "Not found";
        case 405: return "Method not allowed";
        case 411: return "Length required";
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
    while ((n = fread(buf, sizeof(uint8_t), sizeof(buf), from)) > 0) {
       fwrite(buf, sizeof(uint8_t), n, to);
    }
}

void WebUtil::copy(FILE* from, FILE* to, unsigned long size)
{
    uint8_t buf[4096];
    size_t n;
    while (size > 0 && (n = fread(buf, sizeof(uint8_t), MIN(size,sizeof(buf)), from)) > 0) {
       fwrite(buf, sizeof(uint8_t), n, to);
       size -= n;
    }
}


long WebUtil::filesize(string filename)
{
    struct stat sb;
    if (::stat(filename.c_str(), &sb) == -1) {
        throw_exception("Error stat on " + filename);        
    }
    off_t size = sb.st_size;
    return (long) size;
}


