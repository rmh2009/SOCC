
int helper(int c, int d) {
  return c + d + 2;
}

int main(int a, int b) {
  int sum = 5;
  int temp = 10;
  return sum + helper(helper(temp, 1), 1);
}

