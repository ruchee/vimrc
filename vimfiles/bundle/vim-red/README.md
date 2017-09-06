Syntax Highlighting For REBOL (3) in Vim
----

I'm going to assume that anyone needing REBOL syntax highlighting for Vim is already aware that REBOL is a homoiconic language. Most syntax highlighting I've seen for REBOL downplays its homoiconicity, treating it as if the DO dialect were paramount. I wanted to strike a balance between the DO dialect and plain homoiconicity, and I think I've hit on it.

What this means is that I don't highlight a token based on whether the DO dialect predefines it as a function or some other data type. Rather, I highlight it based on its _homoiconic_ data type and whether or not it is predefined. E.g., X is a WORD!, but it is not predefined. DO is also a WORD! and it is predefined. Each will get different highlighting. 

There are some exceptions. There's a convention in REBOL that data types end with an exclamation point. _Any word ending in an exclamation point will be highlighted as a datatype, whether predefined or not._ Also, the tokens REBOL, SELF, YES, NO, ON, OFF, TRUE, FALSE, and /LOCAL get special emphasis. 

There are alse some compromises. REBOL allows some pretty complex path expressions, e.g.

    foo/:x/(2 + 3): 7

What to do with this? My solution isn't perfect, but try it out and see. :)

This is a work in progress, but it is mostly complete with few problems. There is a bug with / used as an operator and I still have to finish proper highlighting of non-integral numbers, dates, issues, and a few other types. Complex paths such as those mentioned above may need a few tweaks, though they work pretty well. 

Lastly, the ftdetect plugin in this bundle assumes the use of the .reb extension rather than .r as suggested by Carl Sassenrath (http://www.rebol.com/cgi-bin/blog.r?view=0540). I use this for everything I do. (Most of what I write is R3, not R2.)
