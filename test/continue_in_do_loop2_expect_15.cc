
int main() {
  int sum = 0;
  int i = 0;
  do {
    if ( i == 5) {
      int unrelated = 100;
      break;
    }
    sum = sum + 1;
    i = i + 1;
  } while (i < 10)
  int temp = 10;
  return sum + temp;
}
