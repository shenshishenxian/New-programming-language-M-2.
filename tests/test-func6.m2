void foo() {}

int bar(int a, bool b, int c) { return a + c; }

int main()
{
  printInt(bar(17, false, 25));
  return 0;
}
