#include "mpi.h"
#include <iostream>
#include <iomanip>

using namespace std;
int main(int argc, char *argv[]){

    char idstr[128]; 
    char buff[128]; 
    int numprocs; 
    int myid; 
    int N;
    double sum = 0;
    int i; 
    int temp;
    MPI_Status stat; 

    /*********************************************
      Begin program
     *********************************************/

    MPI_Init(&argc,&argv);                 // Initialize
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);  // Get # processors
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);      // Get my rank (id)

    /*synchronize all processes*/
    MPI_Barrier( MPI_COMM_WORLD );
    double tbeg = MPI_Wtime();

    if( myid == 0 ) 
    {  // Master

        cin >> N;
        double recNum;
        for( i = 1; i < numprocs; i++) 
        { 
            MPI_Send(&N, 1, MPI_INT, i, 0, MPI_COMM_WORLD); 
        } 

        for(int i = myid; i<=N; i+= numprocs){
            if(i==0) continue;
            sum += (double) 1/(i*i);
        }

        for( i = 1; i < numprocs; i++) 
        { 
            MPI_Recv(&recNum, 1,MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &stat); 
            sum += recNum;
        } 
        cout << setprecision(7) << sum;
    } 
    else 
    {  // Slave

       MPI_Recv(&N, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &stat); 

        for(int i = myid; i<=N; i+= numprocs){
            if(i==0) continue;
            sum += (double) 1/(i*i);
        }

       MPI_Send(&sum, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD); 
    } 

    MPI_Barrier( MPI_COMM_WORLD );
    double elapsedTime = MPI_Wtime() - tbeg;
    double maxTime;
    MPI_Reduce( &elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD );
    if ( myid == 0 ) {
        printf( "Total time (s): %f\n", maxTime );
    }

    /* shut down MPI */
    MPI_Finalize(); 
    return 0;
}