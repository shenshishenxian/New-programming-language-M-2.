int main()
{
  int i;
  i = 0;
  for ( ; i < 5; ) {
    printInt(i);
    i = i + 1;
  }
  printInt(42);
  return 0;
}
