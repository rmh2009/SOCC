int main() {
  int array[3];
  array[0] = 1;
  array[1] = 2;
  array[2] = 3;
  int* b = &array[2];
  return *b;
}
