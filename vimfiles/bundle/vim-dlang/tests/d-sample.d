
import std.stdio;

void main(char[][] args)
{
    writefln("Hello World, Reloaded");

    // auto type inference and built-in foreach
    foreach (argc, argv; args)
    {
        // Object Oriented Programming
        CmdLin cl = new CmdLin(argc, argv);
        // Improved typesafe printf
        writefln(cl.argnum, cl.suffix, " arg: %s", cl.argv);
        // Automatic or explicit memory management
        delete cl;
    }

    // Nested structs and classes
    struct specs
    {
        // all members automatically initialized
        int count, allocated;
    }

    // Nested functions can refer to outer
    // variables like args
    specs argspecs()
    {
        specs* s = new specs;
        // no need for '->'
        s.count = args.length;             // get length of array with .length
        s.allocated = typeof(args).sizeof; // built-in native type properties
        foreach (argv; args)
            s.allocated += argv.length * typeof(argv[0]).sizeof;
        return *s;
    }

    // built-in string and common string operations
    writefln("argc = %d, " ~ "allocated = %d",
        argspecs().count, argspecs().allocated);
}

class CmdLin
{
    private int _argc;
    private char[] _argv;

public:
    this(int argc, char[] argv) // constructor
    {
        _argc = argc;
        _argv = argv;
    }

    int argnum()
    {
        return _argc + 1;
    }

    char[] argv()
    {
        return _argv;
    }

    char[] suffix()
    {
        char[] suffix = "th";
        switch (_argc)
        {
          case 0:
            suffix = "st";
            break;
          case 1:
            suffix = "nd";
            break;
          case 2:
            suffix = "rd";
            break;
          default:
            break;
        }
        return suffix;
    }
}


class Test
{
       static this()
       {
       }
}


void func1(int x)
{   int x;      // illegal, x shadows parameter x

    int y;

    { int y; }  // illegal, y shadows enclosing scope's y

    void delegate() dg;
    dg = { int y; };    // ok, this y is not in the same function

    struct S
    {
        int y;          // ok, this y is a member, not a local
    }

    { int z; }
    { int z; }  // ok, this z is not shadowing the other z

    { int t; }
    { t++;   }  // illegal, t is undefined
}

char[] a;

foreach (int i, char c; a)
{
    printf("a[%d] = '%c'\n", i, c);
}


char[] a = "\xE2\x89\xA0";      // \u2260 encoded as 3 UTF-8 bytes

foreach (dchar c; a)
{
    printf("a[] = %x\n", c);    // prints 'a[] = 2260'
}

dchar[] b = "\u2260";

foreach (char c; b)
{
    printf("%x, ", c);  // prints 'e2, 89, a0'
}

int[] arr = [1, 2, 3];
writefln("%(%d, %)", arr); // prints '1, 2, 3'

int gethardware()
{
    asm // a comment
    {
        mov EAX, dword ptr 0x1234;
    }
    asm { push 3; }
}

switch(x)
{
	case 'a':
		goto case 'b';
		goto default;
		goto y;
}

// *** DDoc (D documentation) ***

/**
 * Square root function.
 *
 * Params:
 *   x = The number to calculate the square root of.
 *
 * Returns: The square root of x.
 *
 * Throws:
 *   $(D MathException) if x is negative.
 *
 * Example:
 * ---
 * assert(sqrt(16) == 4);
 * ---
 */
real sqrt(real x);
float sqrt(float x); /// ditto
double sqrt(double x); /++ ditto +/

/++
    $(LI $(D filename) must not contain any of the following reserved
        characters: <>:"/\|?*)
    $(LI $(D filename) must not contain any of the following $(I reserved
        characters): <>:"/\|?*)
 +/

/++
 +  $(LI $(D filename) must not contain any of the following reserved
 +      characters: <>:"/\|?*)
 +  $(LI $(D filename) must not contain any of the following $(I reserved
 +      characters): <>:"/\|?*)
 + Example:
 + -----
 + struct S // A comment
 + {
 +    int x;
 +    enum y = max(
 +        1,
 +        2,
 +        3);
 + }
 + -----
 +/

/// Highlight this: $(D n == sqrt(n)*sqrt(n))
/// $(D $(LREF takeOne))

// Bug/TODO highlighting:
/// $(LI $(BUGZILLA 8022): BigInt division bug (2))

/// Unmatched paranthesis: )

/// Broken nesting: $(D [)
/// Backslashes: $(D \dmd2\windows\bin\dmd.exe)
/// Template: $(D SortedRange!(RangeIndex, (a, b) => binaryFun!less(*a, *b)))
/// Nesting: $(D if ((())))
/// Nesting 2: $(D void multiSort(Range)(Range r) if (validPredicates!(ElementType!Range, less));)
/// Strings: $(D multiSort!("a.id < b.id", "a.date > b.date")(r))
/// Octal: $(D 0755)
/// (example: x = sin(0.1); )

/// Example:
/// ----
/// void $(B invariant())
/// {
///     const tokenStrings =
///     [
///         q{ foreach (i; 1..10) { writeln("C:\\Hello"); } },
///         ...
/// ----

// *** string escapes ***
const bell = "\007";                    // octal escape
const badEscape = "\z";                 // unrecognized escapes should be errors

// *** D2 string literals ***
const quotes = "C:\\Hello"c;
const backquotes = `C:\Hello`w;
const wysiwyg = r"C:\Hello"d;
const hex = x"48 65 6c 6C 6F";
const hexError = x"wrong";
const heredoc = q"EOF
C:\Hello
not end of string: "
also not end of string: EOF";
actual end of string:
EOF"; // "
const matching1 = q"( [{<" "(a)" )"; // "
const matching2 = q"[ ({<" "[a]" ]"; // "
const matching3 = q"{ ([<" "{a}" }"; // "
const matching4 = q"< ([{" "<a>" >"; // "
const tokenString = q{ foreach (i; 1..10) { writeln("C:\\Hello"); } };

// *** standard library types ***
string str;
wstring wstr;
dstring dstr;
size_t size;
hash_t hash;
Object obj;

// *** additional keywords ***
immutable typeof(this) thisType;
__gshared pure nothrow shared immutable @property @safe @trusted @system @disable x; // attributes
__gshared: pure: nothrow: shared: immutable: @property: @safe: @trusted: @system: @disable: // attribute sections
try {} catch {}
void takesByReference(ref int x);
enum b = __traits(isAbstractClass, T);
static assert(__traits(compiles, interval.span(iPosInfInterval)));

// *** decl ***
pragma(msg, "Hello");
pragma(msg, typeof(std.algorithm.map!q{a+1}([0,1,2])).stringof);
pragma(startaddress, 0x123456);
scope (exit) {}

// *** enum ***
enum x = 5; // {}                       // manifest constants // {}
enum int ZERO = 0;                      // typed manifest constants

// *** outliner ***
enum                  spacious                  = { bar }; // outliner shouldn't contain spaces              // yes
void httpRequest(HttpRequest request, void delegate(Data) resultHandler, void delegate(string) errorHandler) // yes
fn(foo);                                                                                                     // no
debug fn(foo)                                                                                                // no
		.writeln();                                                                                          // no
void fn(int i)                                                                                               // yes
{}
struct S(T)                                                                                                  // yes
{
	this(void delegate(string[] lines) handler)                                                              // yes
	{}
}
string escapeShellCommand(in char[][] args...)                                                               // yes
{}


// *** D2 operator overloading ***
struct Overloaded
{
	bool opDispatch();
	bool opUnary();
	bool opBinary();
	bool opBinaryRight();
	// ... 
}

// *** : ***
static assert(is(int : long));

// *** numeric literals ***
int a = 123456;
int a = 1_2_3_4_5_6_;
int au = 1u;
int aU = 1U;
int aL = 1L;
int al = 1l; // error
int al = 1LL; // error
const bytemax = 0xFF; // last F is part of number, not float suffix
auto ulongmax = 18_446_744_073_709_551_615UL;
const sqrt_minus_1 = 1i;
const imaginaryHexFloat = 0x04p+1i;
const NULL = null;
auto b = 60.0;
auto b = 60f;
auto b = 60F;
auto b = 60.0L;
auto b = 60.0F;
auto b = 60. seconds(); // Don't color the .
auto b = 60.f; // UFCS, not float
auto b = 60.i; // UFCS, not imaginary
auto zero = 0;
auto octal = 0755; // error, octal literals are deprecated in D2
auto agent = 007; // allowed, 7 is the same in decimal and octal
real n = frexp(0x1p-16384L, x);
enum real PI = 0x1.921fb54442d18469898cc51701b84p+1L; /** $(_PI) = 3.141592... */
int y2i;

// *** declarations ***
class C;
class D:E;
struct foo;
union bar;

// *** standard properties ***
x = T.init;
arr.sort;    // built-in
arr.sort();  // std.algorithm
arr.sort!(); // std.algorithm

struct S { int sort; }
S s; s.sort = 5; // custom type false-positive

{foo!(bar).init}

// *** templates and casts ***
interface I(T) { T getValue(); }
T enforce(T)(T condition, lazy string message = "Precondition failed") { ... }
T op(T, string OP = "+")(T a, T b) { return mixin("a" ~ OP ~ "b"); }
void foo(int A = 2 + 2, X...)(int b = 2 + 2) {}
void getopt(T...)(ref string[] args, T opts) {}
auto addr = cast(const(sockaddr)*) &sin;
auto a = cast(std.traits.Signed!T) b;
auto a = cast(typeof({return 5;})) b;

// *** # ***
int #line 6 "foo\bar"
x;  // this is now line 6 of file foo\bar

#unknown // error

// *** TODO ***

auto EB_FE = { x : goto x; };           // delegate literals with labels (ambiguous with struct literals)
//SomeStruct s = { x : 0 };            // struct literals (ambiguous with delegate literals)
