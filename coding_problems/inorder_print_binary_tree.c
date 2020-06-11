
void* malloc(int size);
void free(void* p);
int printf();
int putchar(int c);

struct Node {
  int num;
  struct Node* left;
  struct Node* right;
};

struct Node* createnode(int num) {
  struct Node* node = malloc(50);
  node->num = num;
  node->left = 0;
  node->right = 0;
  return node;
}

int printnode(struct Node* node) {
  char format[5] = "(%d,";
  // hacky way to set the last char to 0.
  format[4] = 0;
  printf(format, node->num);
  if (node->left != 0) {
    printnode(node->left);
  } else {
    putchar('#');
  }
  putchar(',');
  if (node->right != 0) {
    printnode(node->right);
  } else {
    putchar('#');
  }
  putchar(')');
  return 0;
}

int main() {
  struct Node* node = createnode(0);
  node->left = createnode(1);
  node->right = createnode(2);
  node->left->left = createnode(3);
  printnode(node);
  putchar(10);

  // Expect stdout: (0,(1,(3,#,#),#),(2,#,#))
  return 0;
}
