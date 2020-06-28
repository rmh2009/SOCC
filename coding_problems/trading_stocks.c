// Best time trading stocks.
// You are allowed to make at most K trades (buy/sell).

// printf function from c standard library.
int printf();

// Returns the maximum profit at the end of an operation (either buy or sell).
int calculate(int op, int cols, int data[6], int profit[7][7]) {
  int buy = op / 2 * 2 == op ? 1 : -1; 

  profit[op][op] = profit[op-1][op-1] + buy * data[op-1];
  for (int i = op + 1; i < cols; ++i) {
    int temp = profit[op-1][i-1] + buy * data[i-1];
    profit[op][i] = profit[op][i-1] > temp? profit[op][i-1] : temp;
  }
  return profit[op][cols-1];
}

int main() {

  int data[6];
  data[0] = 1;
  data[1] = 5;
  data[2] = 7;
  data[3] = 2;
  data[4] = 3;
  data[5] = 9;

  int K = 3;

  int max_profit = -100000;

  int profit[7][7];

  // Makes no trades in the first row (op 0).
  for (int i = 0; i < 7; ++i) {
    profit[0][i] = 0;
  }

  for (int i = 1; i < 7; ++i) {
    int profit = calculate(i, 7, data, profit);
    max_profit = max_profit < profit? profit : max_profit;
  }

  for (int i = 0; i < 7; ++i ){
    for (int j = 0; j < 7; ++j) {
      printf("%d,", profit[i][j]);
    }
    printf("\n");
  }

  // Expecting 13;
  return max_profit;
}


