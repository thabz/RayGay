#include <mpi.h>
#include <stdio.h>
#include <string.h>
 
#define BUFSIZE 128
#define TAG 0

enum {
    READY_FOR_WORK,
    RETURNING_WORK,
    HERE_IS_SOME_WORK,
} tags;

int master() {

}

int slave() {
    char buff[BUFSIZE];
    bool done = false;
    while(!done) {
        MPI_Send(NULL, 0, MPI_CHAR, 0, READY_FOR_WORK, MPI_COMM_WORLD);
        MPI_Recv(buff, 1024, MPI_CHAR, 0, HERE_IS_SOME_WORK, MPI_COMM_WORLD, &stat);
        MPI_Send(NULL, 0, MPI_CHAR, 0, RETURNING_WORK, MPI_COMM_WORLD);
    }
}
 
int main(int argc, char *argv[])
{
   char idstr[32];
   char buff[BUFSIZE];
   int numprocs;
   int myid;
   int i;
   MPI_Status stat; 
 
   MPI_Init(&argc,&argv); /* all MPI programs start with MPI_Init; all 'N' processes exist thereafter */
   MPI_Comm_size(MPI_COMM_WORLD,&numprocs); /* find out how big the SPMD world is */
   MPI_Comm_rank(MPI_COMM_WORLD,&myid); /* and this processes' rank is */
 
   /* At this point, all the programs are running equivalently, the rank is used to
      distinguish the roles of the programs in the SPMD model, with rank 0 often used
      specially... */
   if(myid == 0)
   {
     printf("%d: We have %d processors\n", myid, numprocs);
     for(i=1;i<numprocs;i++)
     {
       sprintf(buff, "Hello %d!\n", i);
       MPI_Send(buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD);
     }
     for(i=1;i<numprocs;i++)
     {
       MPI_Recv(buff, BUFSIZE, MPI_CHAR, i, TAG, MPI_COMM_WORLD, &stat);
       printf("%d: %s\n", myid, buff);
     }
   }
   else
   {
     /* receive from rank 0: */
     MPI_Recv(buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD, &stat);
     sprintf(idstr, "Processor %d ", myid);
     strcat(buff, idstr);
     strcat(buff, "reporting for duty\n");
     /* send to rank 0: */
     MPI_Send(buff, BUFSIZE, MPI_CHAR, 0, TAG, MPI_COMM_WORLD);
   }
 
   MPI_Finalize(); /* MPI Programs end with MPI Finalize; this is a weak synchronization point */
   return 0;
 }

