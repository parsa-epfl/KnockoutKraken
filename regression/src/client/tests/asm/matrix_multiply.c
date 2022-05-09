int multiply(char i[64][64], char j[64][64], char o[64][64]) {
  for(int x = 0; x < 64; ++x){
    for(int y = 0; y < 64; ++y){
      o[x][y] = 0;
      for(int z = 0; z < 64; ++z){
          o[x][y] += i[x][z] * j[z][y];
      }
    }
  }
  return 0;
}