struct Address {
  char street[100];
  int zip;
};

struct Person{
  int num;
  char name[10];
  struct Address address[2];
  int age;
};

int main() {
  struct Person p;
  p.num = 5;
  p.address[0].zip = 10;
  struct Person* persons[100];
  return p.address[0].zip;
}
