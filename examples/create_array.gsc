create_array(n, x)
{
    toReturn = [];
    for (i = 0; i < n; i++)
        append(toReturn, x);
    return toReturn;
}

print_array(arr)
{
    for (i = 0; i < arr.size; i++)
        print(arr[i]);
}

main()
{
    a = create_array(3, "adam");
    b = create_array(3, "bob");

    print_array(a);
    print_array(b);
}