
int f(int* p) {
  *p = *p + 1;
  return 0;
}

int main() {
  int a = 5;
  f(&a);
  return a;
}
