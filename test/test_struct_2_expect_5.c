struct Person{
  int num;
  char name[10];
  int age;
};

int main() {
  struct Person p;
  p.num = 5;
  return p.num;
}
