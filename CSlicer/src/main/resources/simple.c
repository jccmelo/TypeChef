void print(int x) {}

void main()
{
	int x = 42;
	int y = 0;
	int z = 1;

#ifdef FOO
	y = x;
#endif
	
	if(y > 0)
		print(2 * z);

#ifdef BAR
	x = z;
#endif
		
	print(x); // criterion
	print(y);
	print(z);
}