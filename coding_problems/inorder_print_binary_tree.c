
void* malloc(int size);
void free(void* p);
int printf();

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
  printf("(%d,", node->num);
  if (node->left != 0) {
    printnode(node->left);
  } else {
    printf("#");
  }
  printf(",");
  if (node->right != 0) {
    printnode(node->right);
  } else {
    printf("#");
  }
  printf(")");
  return 0;
}

int main() {
  struct Node* node = createnode(0);
  node->left = createnode(1);
  node->right = createnode(2);
  node->left->left = createnode(3);
  printnode(node);
  printf("\n");

  // Expect stdout: (0,(1,(3,#,#),#),(2,#,#))
  return 0;
}
