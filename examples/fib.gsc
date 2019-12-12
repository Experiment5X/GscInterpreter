fibonacci(n)
{
	first = 0;
	second = 1;

	for (i = 0; i < n; i++)
	{
		print(first);
		temp = second;
		second = first + second;
		first = temp;
	}
}

main()
{
	fibonacci(10);
}
