// Tests import of modules which are keywords
module void // Module name
;

import
std.string;
import  std.assert;
import std.stdio, // Import io module
       std.property;

void main(string[] args) {
    enum Problem
    {
        importVar,
        or_moduleVar
    }

    // string not highlighting
    string myvar;
    assert(true);
}
