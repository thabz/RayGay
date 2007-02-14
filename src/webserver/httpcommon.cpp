
#include "webserver/httpcommon.h"

////////////////////////////////////////////////////////////////////////
// HTTPResponse
////////////////////////////////////////////////////////////////////////

HTTPResponse::HTTPResponse(int status, string contenttype) {
    this->status = status;
    this->contenttype = contenttype;
    addHeader("Content-type",contenttype);
    bodyFILE = NULL;
    bodyString = "";
    length = -1;
}

HTTPResponse::HTTPResponse() {
    bodyFILE = NULL;
    bodyString = "";
    length = -1;
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

void HTTPResponse::addHeader(string name, string value)
{
    headers.push_back(pair<string,string>(name,value));        
}

void HTTPResponse::setBody(FILE* data) {
    this->bodyFILE = data;        
}

void HTTPResponse::setBody(const string& data) {
    this->bodyString = data;        
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
