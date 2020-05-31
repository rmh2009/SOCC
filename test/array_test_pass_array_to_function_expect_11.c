
int printf();

int helper(int data[6]) {
  data[0] = data[0] + 10;
  return 0;
}

int main() {
  int data[6];
  data[0] = 1;

  helper(data);
  return data[0];
}
