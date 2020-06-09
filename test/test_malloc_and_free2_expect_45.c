
void* malloc();
void free(void* p);

int main() {
  int *p = malloc(4 * 10);
  for (int i = 0; i < 10; ++i) {
    p[i] = i;
  }
  int sum = 0;
  for (int i = 0; i < 10; ++i) {
    sum = sum + p[i];
  }
  free(p);

  return sum;
}
