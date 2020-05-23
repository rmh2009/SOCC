int main() {
  int arr[2][10];
  for (int i = 0; i < 2; i=i+1) {
    for (int j = 0; j < 10; j = j+1) {
      arr[i][j] = i*10 + j;
    }
  }
  return arr[1][8];
}
