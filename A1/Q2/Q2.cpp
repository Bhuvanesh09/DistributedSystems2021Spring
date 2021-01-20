#include "mpi.h"
#include <iostream>
#include <iomanip>
#include <vector>
using namespace std;

void swap(int *ptr1, int *ptr2){
    if(ptr1 == ptr2) return;

    *ptr1 = *ptr1 ^ *ptr2;
    *ptr2 = *ptr1 ^ *ptr2;
    *ptr1 = *ptr1 ^ *ptr2;

    return ;

}
int putInPlace(vector <int> &ar, int left, int right){
    auto anchor = ar[right-1];

    int idx = left;
    for(int i=left; i<right; i++){
        if(ar[i] < anchor){
            swap(&ar[i], &ar[idx]);
            idx++;
        }
    }

    swap(&ar[right-1], &ar[idx]);
    return idx;
}

void quickSort(vector <int> &ar, int left, int right){

    if(left < right){
        auto partition = putInPlace(ar, left, right);
        quickSort(ar, left, partition);
        quickSort(ar, partition+1, right);
    }
    return ;
}

void sendVector(int rec, int *ar, int N){
    // cout << "Sedingi vector with size " << N << endl;
    MPI_Send(&N, 1, MPI_INT, rec, 0, MPI_COMM_WORLD);
    MPI_Send(ar, N, MPI_INT, rec, 0, MPI_COMM_WORLD);
    // cout << "vector sent with size" << N << endl;
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

    if( myid == 0 ) 
    {  // Master

        cin >> N;
        vector <int> ar(N,0);
        vector <vector <int>> sortedParts(numprocs);
        for(int i=0; i<N; i++) cin >> ar[i];

        for(int i = 1; i < numprocs; i++) 
        { 
            // int numInts = (N/numprocs) * (i+1) < N ? N/numprocs : N - (N/numprocs) * i;
            int numInts = (i == numprocs-1) ? N - (N/numprocs) * i : N/numprocs;
            // MPI_Send(&ar + (N/numprocs) * i, numInts, MPI_INT, i, 0, MPI_COMM_WORLD); 
            // cout << i << " " << numInts << endl;
            sendVector(i, &ar[0] + (N/numprocs) * i, numInts);
        } 

        quickSort(ar, 0, (N/numprocs));
        sortedParts[0] = vector <int>(ar.begin(), ar.begin()+(N/numprocs));

        for(int i = 1; i<numprocs; i++)
        {
            sortedParts[i] = receiveVector(i);
        }

        // for(int i=0 ; i<numprocs; i++){
        //     cout << "\nSorted Part: " << i << endl;
        //     for(auto x: sortedParts[i]){
        //         cout << x << " ";
        //     }
        // }

        int gIdx, minIdx;
        vector <int> idx(numprocs, 0);
        
        for(gIdx=0; gIdx<N; gIdx++){
            for(int i=0; i<numprocs; i++){
                if(idx[i] < sortedParts[i].size()) minIdx = i;
            }
            for(int i=0; i<numprocs; i++){
                if(idx[i] >= sortedParts[i].size()) continue;
                if(sortedParts[i][idx[i]] < sortedParts[minIdx][idx[minIdx]]) minIdx = i;
            }
            ar[gIdx] = sortedParts[minIdx][idx[minIdx]];
            idx[minIdx]++;
        }
        
        // cout << endl;
        for(auto x : ar) cout << x << " ";


    } 
    else 
    {  // Slave

    //    MPI_Recv(&N, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &stat); 

        vector <int> myar = receiveVector(0);
        quickSort(myar, 0, myar.size());
        sendVector(0,&myar[0],myar.size());
    } 

    MPI_Finalize(); 
    return 0;
}