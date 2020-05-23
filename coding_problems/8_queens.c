// Calculates all permuations of a 8 queen problem and print to stdout.
// Expecting 92 distinct solutions.

int putchar(int a);

int printboard(int p[8][8]) {
  for (int i = 0; i < 8; i = i + 1) {
    for (int j = 0; j < 8; j = j + 1) {
      putchar(p[i][j] + '0');
    }
    putchar(10); // 10 is new line.
  }
  putchar(10);
}

// Checks if p[i][j] can be 1 in the given board.
int check(int p[8][8], int x, int y){
  for (int i = 0; i < 8; i=i+1) {
    if (i == x) continue;
    if (p[i][y] == 1) return 0;
  }

  for (int i = 0; i < 8; i=i+1) {
    if (i == y) continue;
    if (p[x][i] == 1) return 0;
  }

  int xt = x - 1;
  int yt = y - 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt][yt] == 1) return 0;
    xt = xt - 1;
    yt = yt - 1;
  }
  xt = x + 1;
  yt = y + 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt][yt] == 1) return 0;
    xt = xt + 1;
    yt = yt + 1;
  }
  xt = x - 1;
  yt = y + 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt][yt] == 1) return 0;
    xt = xt - 1;
    yt = yt + 1;
  }
  xt = x + 1;
  yt = y - 1;
  while (xt >= 0 && xt <8 && yt >=0 && yt <8) {
    if (p[xt][yt] == 1) return 0;
    xt = xt + 1;
    yt = yt - 1;
  }
  return 1;
}

int populaterow(int p[8][8], int row, int* solutions) {
  if (row == 8) {
    printboard(p);
    *solutions = *solutions + 1;
    return 0;
  }

  for (int j = 0; j <8; j = j+1) {
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
  for (int i = 0; i < 8; i = i + 1) {
    for (int j = 0; j < 8; j = j + 1) {
      board[i][j] = 0;
    }
  }
  populaterow(board, 0, &solutions);

  // Expecting 92.
  return solutions;
}
