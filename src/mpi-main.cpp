#include <mpi.h>
#include <cstdio>
#include <cstdlib>
#include <string.h>
#include <iostream>
#include <unistd.h>

using namespace std;
 
#define BUFSIZE 128
#define TAG 0

enum tags {
    READY_FOR_WORK,
    RETURNING_WORK,
    HERE_IS_SOME_WORK,
    THERE_IS_NO_MORE_WORK
};

void master() {
    char buff[BUFSIZE];
    bool done = false;
    MPI::Status stat; 
    int jobs = 100;
    while(!done) {
	MPI::COMM_WORLD.Recv(buff, 1024, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, stat);
	int tag = stat.Get_tag();
	if (tag == READY_FOR_WORK) {
	    int source = stat.Get_source();
	    if (jobs > 0) {
		--jobs;
		cout << "Source " << source << " getting work." << endl;
		MPI::COMM_WORLD.Send(NULL, 0, MPI_CHAR, source, HERE_IS_SOME_WORK);
	    } else {
		cout << "Source " << source << " laid off." << endl;
		MPI::COMM_WORLD.Send(NULL, 0, MPI_CHAR, source, THERE_IS_NO_MORE_WORK);
	    }
	} else if (tag == RETURNING_WORK) {
	    int source = stat.Get_source();
	    cout << "Source " << source << " returned some work." << endl;
	}
    }
}

void slave(int myid) {
    char buff[BUFSIZE];
    bool done = false;
    MPI::Status stat; 
    srandom((intptr_t)&stat);
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int namelen;

    MPI::Get_processor_name(processor_name, namelen);
    cout << "...Processor name: " << processor_name << endl;

    while(!done) {
        MPI::COMM_WORLD.Send(NULL, 0, MPI_CHAR, 0, READY_FOR_WORK);
	MPI::COMM_WORLD.Recv(buff, 1024, MPI_CHAR, 0, MPI_ANY_TAG, stat);
	int tag = stat.Get_tag();
	if (tag == THERE_IS_NO_MORE_WORK) {
	    done = true;
	} else if (tag == HERE_IS_SOME_WORK) {
	    int usecs = random() & 10000000 + 10; 
   	    cout << " Slave " << myid << " got some work for " << usecs << " microseconds." << endl;
	    //usleep(usecs);
	    int j = 10;
	    for(int i = 0; i < usecs; i++) {
		j *= j + i;
	    }
            MPI::COMM_WORLD.Send(NULL, 0, MPI_CHAR, 0, RETURNING_WORK);
	}
    }
}
 
int main(int argc, char *argv[])
{
   MPI::Init(argc,argv); /* all MPI programs start with MPI_Init; all 'N' processes exist thereafter */
   int numprocs = MPI::COMM_WORLD.Get_size(); /* find out how big the SPMD world is */
   int myid = MPI::COMM_WORLD.Get_rank(); /* and this processes' rank is */
 
   /* At this point, all the programs are running equivalently, the rank is used to
      distinguish the roles of the programs in the SPMD model, with rank 0 often used
      specially... */
   if (myid == 0) {
       cout << numprocs << " slaves are ready." << endl;
       master();
   } else {
       slave(myid);
   }

   cout << "Exit for process " << myid << endl;
 
   MPI::Finalize(); /* MPI Programs end with MPI Finalize; this is a weak synchronization point */
   return 0;
 }

