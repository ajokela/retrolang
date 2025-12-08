// Factorial calculator for RetroShield Z80
// Demonstrates recursive function calls

func factorial(n: int): int
    if n <= 1 then
        return 1;
    else
        return n * factorial(n - 1);
    end;
end;

proc main()
    var i: int;
    var result: int;

    print("Factorial Calculator");
    println();
    println();

    for i := 1 to 8 do
        result := factorial(i);
        printi(i);
        print("! = ");
        printi(result);
        println();
    end;
end;
