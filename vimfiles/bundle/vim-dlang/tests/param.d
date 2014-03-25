void main() {
    writeln(q"EOS
    This
    is a multi-line
    heredoc string
EOS");

    enum a = "Not in heredoc";

	writeln(q"[
<!DOCTYPE html>]", 0x82);
    import std.stdio;

}
