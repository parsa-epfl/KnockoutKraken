#define LENGTH 4096
void selectSort(char data[LENGTH]){
  for(int i = 0; i < LENGTH - 1; ++i){
    // pick the smallest one.
    char s = data[i];
    int index = i;
    for(int j = i+1; j < LENGTH; ++j){
      if(data[j] < s){
        index = j;
        s = data[j];
      }
    }
    data[index] = data[i];
    data[i] = s;
  }
}