
int helper(int c) {
  return c + 2;
}

int main(int a, int b) {
  int sum = 5;
  int temp = 10;
  return sum + helper(helper(temp));
}

