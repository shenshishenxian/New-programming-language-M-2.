int a;

void foo(int c)
{
  a = c + 42;
}

int main()
{
  foo(73);
  printInt(a);
  return 0;
}
