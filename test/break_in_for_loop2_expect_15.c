
int main() {
  int sum = 0;
  int i = 0;
  for (i = 0; i < 10; i=i+1) {
    int notrelated = 2;
    int notrelated2 = 2;
    sum = sum + 1;
    if (sum == 5) {
      if (1) break;
    }
  }
  int temp = 10;
  return sum + temp;
}
