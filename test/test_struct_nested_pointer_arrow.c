
int printf();

struct Person {
  char name[100];
  int age;
  struct Person* child;
};

int main() {
  struct Person child;
  struct Person person;
  struct Person* pp = &person;
  pp->child = &child;
  pp->child->age = 10;
  pp->child->name[0] = 'y';
  pp->child->name[1] = 'a';
  pp->child->name[2] = 0;
  printf(pp->child->name);
  return pp->child->age;
}
