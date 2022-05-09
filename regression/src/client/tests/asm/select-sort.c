void selectSort(char data[4096]){
  for(int i = 0; i < 4095; ++i){
    // pick the smallest one.
    int s = data[i];
    int index = i;
    for(int j = i+1; j < 4096; ++j){
      if(data[j] < s){
        index = j;
        s = data[j];
      }
    }
    data[index] = data[i];
    data[i] = s;
  }
}