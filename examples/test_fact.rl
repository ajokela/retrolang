// Simple factorial test
func fact(n: int): int
    var r: int;

    if n <= 1 then
        print("base, n=");
        printi(n);
        println();
        return 1;
    end;

    print("rec, n=");
    printi(n);
    println();
    r := fact(n - 1);
    print("got ");
    printi(r);
    print(" * ");
    printi(n);
    println();
    return n * r;
end;

proc main()
    var result: int;
    print("3!:");
    println();
    result := fact(3);
    print("= ");
    printi(result);
    println();
end;
