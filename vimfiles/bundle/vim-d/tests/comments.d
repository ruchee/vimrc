// This is the highlighting of comments file
module comments;

/* It shows the different comments
 * and how they are highlighted
 */

import std.stdio;

/++
 + As we can see it does seem to work.
 +/

void main() {
  for (i = 0; i < 10; ++i)
    {
        writeln(i, "\* This is not a comment \+");
    }*/ // Error
  while(true) {
  }+/ // Error

  /* This is an /* inner comment test */
  int a;
}
