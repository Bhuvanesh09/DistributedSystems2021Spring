#include <bits/stdc++.h>
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

int main(){
    int N;
    cin >> N;

    vector <int> ar(N, 0);

    for(int i=0; i<N; i++) cin >> ar[i];

    quickSort(ar, 0, N);

    for(int i=0; i<N; i++) cout << ar[i];
}