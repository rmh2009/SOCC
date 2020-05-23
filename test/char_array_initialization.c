
int putchar(int a);

int f(int a) {
  char test[11] = "helloworld!";
  for (int i = 0; i < 11; i = i+1) {
    int t = test[i];
    putchar(t);
  }
  return 0;
}

int main() {
  f(0);
}
