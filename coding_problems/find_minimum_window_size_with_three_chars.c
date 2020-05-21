
int main() {
  char data[30] = "abcabdabyabzm01230124";
  int x1 = 0;
  int x2 = 0;
  int x3 = 0;

  char t1 = 'a';
  char t2 = 'y';
  char t3 = 'z';

  int minlen = 10000;
  int j = 0;
  for (int i = 0; i < 30; i=i+1) {
    if (data[i] == t1) x1 = x1 + 1;
    if (data[i] == t2) x2 = x2 + 1;
    if (data[i] == t3) x3 = x3 + 1;
    while (j != i) {
      if (x1 == 0 || x2 == 0 || x3 == 0) {
        break;
      }
      if (minlen > i - j + 1) minlen = i - j + 1;
      if (data[j] == t1) x1 = x1 - 1;
      if (data[j] == t2) x2 = x2 - 1;
      if (data[j] == t3) x3 = x3 - 1;
      j = j+1;
    }
  }
  // Expect 4.
  return minlen;
}
