area()
{
    return 3.14159 * self.radius * self.radius;
}

circumference()
{
    return 3.14159 * 2 * self.radius;
}

display()
{
    print(self.name);
    print("Radius: " + self.radius);
    print("Area: " + (self area()));
    print("Circumference: " + (self circumference()));
}

create_sphere(name, radius)
{
    sphere = [];
    sphere.name = name;
    sphere.radius = radius;

    sphere.area = ::area;
    sphere.circumference = ::circumference;
    sphere.display = ::display;

    return sphere;
}

main()
{
    s1 = create_sphere("My sphere", 3);
    s2 = create_sphere("My other sphere", 5);

    s1 display();
    print();
    s2 display();
}
