// This file presents highlighting issues due to hybrid usage of keywords.

void foo(in int a,
         out string b)
in {
    assert(a == 0);
} out {
    assert(b == "bye");
} body {
    return "bye";
}

void bar(scope string a, string file = __FILE__)
if(typeof(a) == enum) {
    scope(exit) destroy(n);
}
