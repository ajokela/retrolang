// Counter example for RetroShield Z80
// Counts from 1 to 10 and prints each number

proc main()
    var i: int;

    print("Counting to 10:");
    println();

    for i := 1 to 10 do
        printi(i);
        print(" ");
    end;

    println();
    print("Done!");
    println();
end;
