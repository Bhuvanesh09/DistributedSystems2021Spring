
/* MPI Program Template */

#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include <fstream>
#include <bits/stdc++.h>
using namespace std;
typedef long long int ll;


void sendVector(int rec, int *ar, int N){
    // cout << "Sedingi vector with size " << N << endl;
    MPI_Send(&N, 1, MPI_INT, rec, 0, MPI_COMM_WORLD);
    MPI_Send(ar, N, MPI_INT, rec, 0, MPI_COMM_WORLD);
    // cout << "vector sent with size" << N << endl;
}

void broadcastVector(int numprocs, int *ar, int N){
    for(int i=1; i<numprocs; i++){
            MPI_Send(&N, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            MPI_Send(ar, N, MPI_INT, i, 0, MPI_COMM_WORLD);
    }
}

vector <int> receiveVector(int sender){
    int N;
    MPI_Status stat; 
    MPI_Recv(&N, 1, MPI_INT, sender, 0, MPI_COMM_WORLD, &stat);

    vector <int> ar = vector<int> (N,0);

    MPI_Recv(&ar[0], N, MPI_INT, sender, 0, MPI_COMM_WORLD, &stat);

    // cout << "vector received with size: " << N << endl;

    // for(auto x:ar) cout << x << " ";
    return ar;
}

vector <vector <int>> createGraph(int orgN, int orgM, vector <int> orgEdgeListFrom, vector <int> orgEdgeListTo){

        vector <vector <int>> commonNodeGroupOrg(orgN+1);
        vector <vector <int>> adjListNew(orgM+1);

        for(int i=1; i<=orgM; i++){
            commonNodeGroupOrg[orgEdgeListFrom[i]].push_back(i);
            commonNodeGroupOrg[orgEdgeListTo[i]].push_back(i);
        }

        for(int i=1; i<=orgN; i++){
           for(auto x: commonNodeGroupOrg[i]){
               for(auto y: commonNodeGroupOrg[i]){
                    if(x==y) continue;
                    adjListNew[x].push_back(y);
               }
           }
        }
        return adjListNew;
}

bool toColourOrNot(vector <vector <int>> &adjList, vector <int> &currentColour, int node){
    if(currentColour[node] != 0) return false;

    for(auto x: adjList[node]){
        if(x > node && currentColour[x] == 0) return false;
    }

    return true;
}

int findLowestAvailColour(vector <vector <int>> &adjList, vector <int> &currentColour, int node){
    vector <int> flag(502, 0);

    for(auto x : adjList[node]){
        if(currentColour[x]) flag[currentColour[x]] = 1;
    }

    int probe = 1;
    for(probe = 1; flag[probe] != 0; probe++) ;
    return probe;
}

vector <int> colourGraph(vector <vector <int>> &adjList, vector <int> currentColour, int left, int right){
    //Colours from [left, right)
    for(int i=left; i<right; i++){
        if(toColourOrNot(adjList, currentColour, i)){
            currentColour[i] = findLowestAvailColour(adjList, currentColour, i);
        }
    }
    return currentColour; 
}

bool taskDone(vector <int> currentColour, int M){
    for(int i=1; i<=M; i++){
        if(currentColour[i] == 0) return false;
    }

    return true;
}

int main( int argc, char **argv ) {
    int rank, numprocs;

    MPI_Status stat;
    /* start up MPI */
    MPI_Init( &argc, &argv );

    MPI_Comm_rank( MPI_COMM_WORLD, &rank );
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
    
    /*synchronize all processes*/
    MPI_Barrier( MPI_COMM_WORLD );
    double tbeg = MPI_Wtime();

    /* write your code here */
    int orgN, orgM;

    if(rank == 0){
        //Master Code here
        cin >> orgN >> orgM;
        vector <int> orgEdgeListFrom(orgM+1);
        vector <int> orgEdgeListTo(orgM+1);
        for(int i=1; i<=orgM; i++){
            cin >> orgEdgeListFrom[i] >> orgEdgeListTo[i];
        }

        for(int proc=1; proc<numprocs; proc++){
            MPI_Send(&orgN, 1, MPI_INT, proc, 0, MPI_COMM_WORLD);
            MPI_Send(&orgM, 1, MPI_INT, proc, 0, MPI_COMM_WORLD);
            sendVector(proc, &orgEdgeListFrom[0], orgM+1);
            sendVector(proc, &orgEdgeListTo[0], orgM+1);
        }
        vector <vector <int>> adjListNew = createGraph(orgN, orgM, orgEdgeListFrom, orgEdgeListTo);

        vector <int> currentColour(orgM+1, 0);
        int numInts = (rank == numprocs-1) ? orgM - (orgM/numprocs) * rank : (orgM/numprocs);
        int tempInts;
        vector <vector <int>> newColours(numprocs);
        while(!taskDone(currentColour, orgM)){
            broadcastVector(numprocs, &currentColour[0], orgM+1);
            newColours[0] = colourGraph(adjListNew, currentColour, (orgM/numprocs)*rank + 1, (orgM/numprocs)*rank + 1 + numInts);

            for(int i=1; i<numprocs; i++){
                newColours[i] = receiveVector(i);
                tempInts = (i == numprocs-1) ? orgM - (orgM/numprocs) * rank : (orgM/numprocs);
                for(int j = (orgM/numprocs)*rank + 1; j < (orgM/numprocs)*rank + 1 + tempInts; j++){
                    currentColour[j] = newColours[i][j];
                }
            }
        }
        // for(int i=1; i<=orgM; i++){
        //     cout << "\nFor new node " << i << endl;
        //     for(auto x: adjListNew[i]){
        //         cout << x << " ";
        //     }
        // }
    }
    else {
        //Slave Code here

        //Getting the graph:
        int orgN, orgM;

        MPI_Recv(&orgN, 1,MPI_INT, 0, 0, MPI_COMM_WORLD, &stat);
        MPI_Recv(&orgM, 1,MPI_INT, 0, 0, MPI_COMM_WORLD, &stat);
        vector <int> orgEdgeListFrom = receiveVector(0);
        vector <int> orgEdgeListTo = receiveVector(0);

        auto adjListNew = createGraph(orgN, orgM, orgEdgeListFrom, orgEdgeListTo);

        int numInts = (rank == numprocs-1) ? orgM - (orgM/numprocs) * rank : (orgM/numprocs);
        vector <int> currentColour = receiveVector(0);
        while(!taskDone(currentColour, orgM)){
            currentColour = colourGraph(adjListNew, currentColour, (orgM/numprocs)*rank + 1, (orgM/numprocs)*rank + 1 + numInts);
            sendVector(0, &currentColour[0], orgM);
            currentColour = receiveVector(0);
        }
        // cout << "Slave num: " << rank << endl;
        // for(int i=1; i<=orgM; i++){
        //     cout << "\nFor new node " << i << endl;
        //     for(auto x: adjListNew[i]){
        //         cout << x << " ";
        //     }
        // }
        // cout << "\n-----\n";
    }
    MPI_Barrier( MPI_COMM_WORLD );
    double elapsedTime = MPI_Wtime() - tbeg;
    double maxTime;
    MPI_Reduce( &elapsedTime, &maxTime, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD );
    if ( rank == 0 ) {
        printf( "Total time (s): %f\n", maxTime );
    }

    /* shut down MPI */
    MPI_Finalize();
    return 0;
}