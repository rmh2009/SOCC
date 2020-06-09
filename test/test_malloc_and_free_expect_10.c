
void* malloc();
void free(void* p);

struct Person {
  char name[100];
  int age;
};

int main() {
  struct Person* pp;
  pp = malloc(300);
  pp->age = 10;
  int a = pp->age;
  free(pp);
  return a;
}
