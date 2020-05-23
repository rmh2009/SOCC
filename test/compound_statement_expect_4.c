
int main() {
  int b = 1;
  {
    int b = 2;
    b = 3;
  }
  int c = 4;
  return c; 
}

