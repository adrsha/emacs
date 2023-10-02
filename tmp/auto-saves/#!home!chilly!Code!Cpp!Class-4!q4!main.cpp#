#include <iostream>
using namespace std;

class People {
  char name[20];
  int age;

public:
  People() {
    cout << "\nEnter your name" << endl;
    cin >> name;
    cout << "\nEnter your age" << endl;
    cin >> age;
  }
  int getAge() { return age; }
  bool isOlderThan(People a1) {
    if (this->age > a1.getAge()) {
      return true;
    }
    return false;
  }
};

int main() {
  People p1, p2;
  if (p1.isOlderThan(p2)) {
    cout << "The man with age: " << p1.getAge()
         << " is older than the one with age" << p2.getAge();
  } else {
    cout << " The man with age: " << p2.getAge()
         << " is older than the one with age" << p1.getAge();
  }
  return 0;
}
