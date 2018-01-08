bool i;

int main()
{
  int i; /* Should hide the global i */

  i = 42;
  printInt(i + i);
  return 0;
}
