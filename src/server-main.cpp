
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "http/httpserver.h"
#include "exception.h"
#include <iostream>
#include <sstream>
#include <csignal>
#include <cerrno>
#include <cstdlib>
#include <cstdio>

extern "C" {
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

}

#define DEFAULT_LISTEN_PORT 20000

Webserver* server = NULL;

void term(int sig) {
    delete server;        
}

class HelloWorldAction : public Action {
    HTTPResponse execute(const HTTPRequest& request) {
        HTTPResponse response = HTTPResponse(200, "text/plain");    
        response.setBody("Hello world! RayGay renderserver up and running.");
        return response;
    };
};

class DefaultAction : public Action {
    HTTPResponse execute(const HTTPRequest& request) {
        HTTPResponse response = HTTPResponse(200, "text/plain");    
        response.setBody("Hello world! RayGay renderserver up and running.");
        return response;
    };
};

void print_usage() {
    cout << "Usage: server [OPTION...]" << endl;
    cout << "       -p PORTNUM           Listenport (default " << DEFAULT_LISTEN_PORT << ")" << endl;
    cout << "       -d WORKINGDIR        Workingdir (default /tmp/raygay-server-PORTNUM)" << endl;
    cout << "       -h                   Show this help message" << endl;
    cout << "       -v                   Show version" << endl;
}

void print_version() {
    cout << "Raygay Server " << VERSION << endl;
    cout << "Copyright (C) 2004, 2005, 2006, 2007, 2008 Jesper Christensen" << endl;
    /*
    cout << "   Guile: " << parser->version() << endl;
    cout << "   Kernel pagesize: " << getpagesize() << " bytes" << endl;
    cout << "   CPUs: " << getNumberOfCPUs() << endl;
    cout << "   Image formats: ";
    vector<string> formats = Image::supportedFormats();
    for(uint32_t i = 0; i < formats.size(); i++) {
        cout << formats[i];
        if (i != formats.size() -1 ) {
            cout << ", ";
        } 
    }
    cout << endl;
    */
}



int main(int argc, char *argv[]) {
    signal(SIGINT,term);        
    signal(SIGHUP,term);        
    signal(SIGKILL,term);       
    
    // Use getopt to parse arguments.
    int c;
    opterr = 0;
    int listen_port = DEFAULT_LISTEN_PORT;
    string docroot = "";
    while ((c = getopt (argc, argv, "hvp:d:")) != -1) {
	switch(c) {
	    case 'h':
		print_usage();
		return EXIT_SUCCESS;
	    case 'v':
		print_version();
		return EXIT_SUCCESS;
	    case 'p':
		if (sscanf(optarg,"%u",&listen_port) != 1) {
		    cerr << "Illegal -p option" << endl;
		    print_usage();
		    return EXIT_FAILURE;
		}
		if (listen_port < 1024) {
		    cerr << "Illegal listen port. Must be above 1024." << endl;        
		    return EXIT_FAILURE;
		}
		break;
    	    case 'd':
    	        docroot = string(optarg);
    		break;
	    case '?':
		cerr << "Unknown option -" << char(optopt) << endl << endl;
		print_usage();
		return EXIT_FAILURE;
	    default:
		return EXIT_FAILURE;
	}
    }
    
    if (docroot == "") {
        ostringstream os;
        os << "/tmp/raygay-server-";
        os << listen_port;
        docroot = os.str();
    }

    cout << "Workingdir is " << docroot;
    
    if (mkdir(docroot.c_str(), 0777) == -1) {
        if (errno != EEXIST) {    
            cerr << "Couldn't create workingdir " << docroot << endl;
            return EXIT_FAILURE;  
        }  
    } else {
       cout << " (created)";        
    }
    cout << endl;
    
    try {
        server = new Webserver(listen_port, docroot);
        server->addAction("/hello_world", new HelloWorldAction());
        server->addAction("/", new DefaultAction());
        server->run();
    } catch (Exception e) {
        std::cout << "Exception: " << e.getMessage() 
                  << " at " << e.getSourceFile() << ":" << e.getSourceLine() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
