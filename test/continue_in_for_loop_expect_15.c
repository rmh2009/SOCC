
int main() {
  int sum = 0;
  for (int i = 0; i < 10; i=i+1) {
    int notrelated = 2;
    int notrelated2 = 2;
    if (i < 5) continue;
    sum = sum + 1;
  }
  int temp = 10;
  return sum + temp;
}
