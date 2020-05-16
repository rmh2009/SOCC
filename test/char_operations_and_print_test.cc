
int putchar(int a);

int main() {
  char a[20];
  a[0] = 'a';
  for (int i = 1; i < 10; i = i + 1) {
    a[i] = a[i-1]  + 1;
  }
  for (int i = 0; i < 10; i = i + 1) {
    int t = a[i];
    putchar(t);
  }
}
