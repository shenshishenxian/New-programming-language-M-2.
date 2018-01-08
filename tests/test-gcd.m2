int gcd(int a, int b) {
  while (a != b) {
    if (a > b) a = a - b;
    else b = b - a;
  }
  return a;
}

int main()
{
  printInt(gcd(2,14));
  printInt(gcd(3,15));
  printInt(gcd(99,121));
  return 0;
}
