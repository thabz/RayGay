
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "queuemanager-main.h"
#include "exception.h"
#include <iostream>
#include <csignal>
#include <cstdio>
#include <unistd.h>

extern "C" {
#include <sys/types.h>
#include <fcntl.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen ((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) ((dirent)->d_namlen)
# ifdef HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# ifdef HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# ifdef HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif
// #include <sys/dirent.h>    
}

QueueMaster* queuemaster;

// Creates a dir on the server
void createDir(HTTPClient* client, string dirname) 
{
    HTTPRequest request;
    HTTPResponse response;
        
    request.method = "MKCOL";
    request.path = "/" + dirname;

    request.addHeader("Content-length", 0);
    request.setBody("");
    response = client->send(request);
}

// Sends a file to the server
void sendFile(HTTPClient* client, string filename) 
{
    HTTPRequest request;
    HTTPResponse response;

    // Send a HEAD request 
    request.method = "HEAD";
    request.path = "/" + filename;
    response = client->send(request);

    if (response.status == 200) {
        // The file exists. Compare MD5 checksums
        // TODO: Make a local MD5 checksums cache when this get's multithreaded.            
        if (WebUtil::MD5(filename) == response.getHeader("Content-MD5")) {
            return;        
        }
    }

    request.method = "PUT";
    request.path = "/" + filename;
    FILE* f = fopen(filename.c_str(),"r");
    if (f == NULL) {
        throw_exception("Couldn't read " + filename);        
    }
    request.setBody(f);

    request.addHeader("Content-type", WebUtil::pathToMimetype(filename));
    request.addHeader("Content-length", WebUtil::filesize(filename));
    response = client->send(request);
    fclose(f);
}

void sendDirRecursively(HTTPClient* client, string dir_name) {
    uint16_t buffer[2048];
    uint16_t *p;
    long base = 0;
    int len, fd, i;
    
    createDir(client, dir_name);    
    
    if ((fd = open(dir_name.c_str(), O_RDONLY, 0)) < 0) {
        throw_exception("Error opening " + dir_name);
    }
    // getdirentries is deprecated. Use readdir instead.
    /*
    while ((len = getdirentries(fd, (char*)buffer, sizeof(buffer), &base)) > 0) {
        for( i = 0, p = buffer; i < len; i += p[2], p += p[2]/2) {
            struct dirent *e = (struct dirent *) p;
            if (e->d_name[0] == '.') {
                continue;    
            }
            string ent_name = dir_name + "/" + string(e->d_name);
            if (e->d_type == DT_DIR) {
                //printf("D %s\n", e->d_name);
                sendDirRecursively(client, ent_name);
            } else if (e->d_type == DT_REG) {
                //printf("F %s\n", e->d_name);
                sendFile(client, ent_name);    
            }
        }
    }
    */
}

void* slaveThreadDo(void* obj) {
    QueueSlave* slave = (QueueSlave*) obj;
    try {
        slave->run();
    } catch (Exception e) {
	cout << endl << "Exception: " << e.getMessage() 
	     << " at " << e.getSourceFile() << ":" 
	     << e.getSourceLine() << endl;
    }
    return NULL;
}


QueueSlave::QueueSlave(string host, QueueMaster* master) {
    this->client = new HTTPClient(host);
    this->master = master;        
}

QueueSlave::~QueueSlave() {
    delete client;        
}

void QueueSlave::run() 
{
    sendDirRecursively(client, "scenes");        
}

QueueMaster::QueueMaster(set<string> hosts, vector<QueueJob> jobs) {
    this->jobs_to_do = jobs;
    this->hosts = hosts;
}

void QueueMaster::run() {
    uint32_t threads_num = hosts.size();
    pthread_t threads[threads_num];
    int j = 0;
    for(set<string>::iterator i = hosts.begin(); i != hosts.end(); i++) {
        QueueSlave* slave;    
        try {    
            slave = new QueueSlave(*i,this);
        } catch (Exception e) {
            cerr << "Can't resolve " << *i << ". Ignoring." << endl;
            threads_num--;
            continue;
        }
        slaves.push_back(slave);
        ::pthread_create(&threads[j++], NULL, slaveThreadDo, slave);
    }
    for(uint32_t i = 0; i < threads_num; i++) {
        ::pthread_join(threads[i], NULL);
    }
}

void print_usage() {
    cout << "Usage: queuemanager [OPTION...] SERVERS..." << endl;
    cout << "       -h                   Show this help message" << endl;
    cout << "       -v                   Show version" << endl;
}

void print_version() {
    cout << "Raygay Server " << VERSION << endl;
    cout << "Copyright (C) 2004, 2005, 2006, 2007, 2008 Jesper Christensen" << endl;
}


int main(int argc, char *argv[]) {

    // Use getopt to parse arguments.
    int c;
    opterr = 0;
    while ((c = getopt(argc, argv, "hv")) != -1) {
	switch(c) {
	    case 'h':
		print_usage();
		return EXIT_SUCCESS;
	    case 'v':
		print_version();
		return EXIT_SUCCESS;
	    case '?':
		cerr << "Unknown option -" << char(optopt) << endl << endl;
		print_usage();
		return EXIT_FAILURE;
	    default:
		return EXIT_FAILURE;
	}
    }
    
    set<string> hosts;
    while (optind  < argc) {
        hosts.insert(string(argv[optind++]));
    }
    
    char* env_hosts = getenv("RAYGAY_SLAVES");
    if (env_hosts != NULL) {
        printf("Env hosts: %s\n",env_hosts);
    }
    
    vector<QueueJob> jobs;
        
    try {
        /*
        HTTPClient client = HTTPClient("localhost:20000");
        HTTPRequest request;
        HTTPResponse response;
        
        request.host = "127.0.0.1:20000";
        request.method = "GET";
        request.path = "/";
        request.setBody("a=b&c=10&e=ddddd\r\n");
        request.addHeader("Content-type","application/x-www-form-urlencoded");
        response = client.send(request);
        */

        queuemaster = new QueueMaster(hosts, jobs);
        queuemaster->run();

    } catch (Exception e) {
        std::cout << "Exception: " << e.getMessage() 
                  << " at " << e.getSourceFile() << ":" << e.getSourceLine() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
};
