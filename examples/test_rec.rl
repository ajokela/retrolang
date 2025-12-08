// Test recursion
func countdown(n: int): int
    printi(n);
    print(" ");
    if n <= 0 then
        return 0;
    else
        return countdown(n - 1);
    end;
end;

proc main()
    print("Countdown: ");
    countdown(5);
    println();
    print("Done!");
    println();
end;
