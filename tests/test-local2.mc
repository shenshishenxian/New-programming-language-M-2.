int foo(int a, bool b)
{
  int c;
  bool d;

  c = a;

  return c + 10;
}

int main() {
 printInt(foo(37, false));
 return 0;
}
