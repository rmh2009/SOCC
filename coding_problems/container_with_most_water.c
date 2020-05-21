
int min(int a, int b) {
  return a < b? a : b;
}

int main() {

  int data[10];
  data[0] = 1;
  data[1] = 8;
  data[2] = 7;
  data[3] = 6;
  data[4] = 5;
  data[5] = 7;
  data[6] = 8;
  data[7] = 9;
  data[8] = 5;
  data[9] = 1;

  int total = 0;
  int l = 0;
  int r = 9;
  total = (r - l) * min(data[l], data[r]);

  while (r - l > 1) {
    if (data[r] < data[l]) {
      r = r -1;
    } else {
      l = l + 1;
    }
    int temp = (r - l) * min(data[l], data[r]);
    if (total < temp) total = temp;
  }
  // Expect 48.
  return total;
}

