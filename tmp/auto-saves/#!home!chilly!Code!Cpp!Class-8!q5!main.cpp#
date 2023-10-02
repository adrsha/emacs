#include <iostream>

using namespace std;

class Test1 {
public:
  Test1() { cout << "Base Class Created" << endl; }
  ~Test1() { cout << "Base Class Destroyed" << endl; }
};

class Test2 : public Test1 {
public:
  Test2() { cout << "Middle Class Created" << endl; }
  ~Test2() { cout << "Middle Class Destroyed" << endl; }
};

class Test3 : public Test2 {
public:
  Test3() { cout << "Last Class Created" << endl; }
  ~Test3() { cout << "Last Class Destroyed" << endl; }
};

int main() {
  { Test3 A; }
}
