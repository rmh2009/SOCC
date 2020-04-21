
int main() {
  int b = 1;
  {
    int b = 2;
    b = 3;
  }
  return b; 
}

