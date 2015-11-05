struct Foo { int a,b,c; }
int bar(Foo *f) {
  asm {
    // This is a comment
    mov EBX,f                   ;
    /* This is a comment */
    mov EAX,Foo.b.offsetof[EBX] ;
    /+ This is a comment +/
  }
}
void main() {
    Foo f = Foo(0, 2, 0);
    assert(bar(&f) == 2);
}
