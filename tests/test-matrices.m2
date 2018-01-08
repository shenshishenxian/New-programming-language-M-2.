int main() {
  matrix int [4][4] m33;
  m33 = [[2,9,1,1];
        [0,2,1,1];
        [1,1,1,1];
        [3,1,1,1]];
  myprint(m33:tr);
  return 0;
}

void myprint(matrix int [4][4] M){
  int i;
  int j;
  for(i = 0; i < M:rows; ++i){
    for(j = 0; j < M:cols; ++j){
      printInt(M[i,j]);
    }
    printStr("");
  }
}

