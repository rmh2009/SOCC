
struct Person {
  char name[100];
  int age;
};

int main() {
  struct Person person;
  struct Person* pp = &person;
  pp->age = 10;
  return pp->age;
}
