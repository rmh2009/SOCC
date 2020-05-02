
int helper(int c) {
  if (c == 0 || c == 1) {
    return c;
  }
  return helper(c-1) + helper(c-2);

}

int main() {
  return helper(5);
}

