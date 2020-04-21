
int main() {
  int i = 1;
  int sum = 0;
  for (int i = 0; i < 10; i=i+1) {
    sum = sum + 1;
  }

  for (i = 0; i < 10; i=i+1) sum = sum + 1;

  for (;;) {
    sum = sum + 1;
    if (sum == 100) break;
  }

  while(1) {
    sum = sum + 1;
    if (sum == 200) break;
  }

  do {
    sum = sum + 1;
    if (sum == 300) break;
  } while (0)
  return sum;
}
