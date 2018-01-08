int gcd(int a, int b) {
  while (a != b)
    if (a > b) a = a - b;
    else b = b - a;
  return a;
}

int main()
{
  printInt(gcd(14,21));
  printInt(gcd(8,36));
  printInt(gcd(99,121));
  return 0;
}
