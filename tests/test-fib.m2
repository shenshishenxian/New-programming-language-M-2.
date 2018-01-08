int fib(int x)
{
  if (x < 2) return 1;
  return fib(x-1) + fib(x-2);
}

int main()
{
  printInt(fib(0));
  printInt(fib(1));
  printInt(fib(2));
  printInt(fib(3));
  printInt(fib(4));
  printInt(fib(5));
  return 0;
}
