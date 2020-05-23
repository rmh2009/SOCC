// Calculates all permuations of the 8 queen problem and print to stdout.
// Expecting 92 distinct solutions.

int putchar(int a);

int printboard(int p[8][8]) {
  for (int i = 0; i < 8; ++i) {
    for (int j = 0; j < 8; ++j) {
      putchar(p[i][j] + '0');
    }
    putchar(10); // 10 is new line.
  }
  putchar(10);
}

// Checks if p[i][j] can be 1 in the given board.
int check(int p[8][8], int x, int y){
  for (int i = 0; i < 8; ++i) {
    if (p[i][y] == 1 && i != x) return 0;
  }

  for (int i = 0; i < 8; ++i) {
    if (p[x][i] == 1 && i != y) return 0;
  }

  int xt = x - 1;
  int yt = y - 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt--][yt--]) return 0;
  }
  xt = x + 1;
  yt = y + 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt++][yt++]) return 0;
  }
  xt = x - 1;
  yt = y + 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt--][yt++]) return 0;
  }
  xt = x + 1;
  yt = y - 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt++][yt--]) return 0;
  }
  return 1;
}

int populaterow(int p[8][8], int row, int* solutions) {
  if (row == 8) {
    printboard(p);
    *solutions = *solutions + 1;
    return 0;
  }

  for (int j = 0; j <8; ++j) {
    if (check(p, row, j)) {
      p[row][j] = 1;
      populaterow(p, row+1, solutions);
      p[row][j] = 0;
    }
  }
  return 0;
}

int main() {
  int solutions = 0;
  int board[8][8];
  for (int i = 0; i < 8; ++i) {
    for (int j = 0; j < 8; ++j) {
      board[i][j] = 0;
    }
  }
  populaterow(board, 0, &solutions);

  // Expecting 92.
  return solutions;
}
