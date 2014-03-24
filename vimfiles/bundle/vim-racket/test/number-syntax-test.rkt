; highlight as normal identifiers (not numbers):
+2+
--nan--
-2+
90a09
/123/
123/
26e
26e/
e12+4
2e12+4/
.12e1d2
2e2/1e5
inf.0
nan.f
-inf.0@+
-inf.0@
.
d
e
l
f
i
n
a
s
@
#
@2
3@4@5
-inananan.f
0n4
+inf.
-nan.0-inf.f

; highlight as valid numbers:
0
01
1
9
0#
#b1
2/1e5
-1.4e2@6/7d2
#xa
#x-2
#xab.
#x#e1.2-3.abds2i
#o-37.
#i#o30.
-inf.f
+nan.0
-inf.f@5
+nan.0@4e71
-nan.0-inf.fi
#xasef-blci
#xe/9s4
#b1.1e1@1.1e11
#x1#####.##s4#-1i

; not allowed by the syntax according to the online docs, but the actual reader
; accepts them, so they should probably be highlighted as numbers once someone
; makes the necessary complications to the existing rules:
-i
#x1#####.##s4#-i

; these all have errors which should be highlighted:
#x
#o
#d
#b
#e
#i
#x#e
#d9/1/
#d-inf.1
#b01234a
#o31.99913
#e#x
#x#e
#d-@2
#o-39.
#d.inf.0
#d0inf.0
#d1nf.0
#d1@2@3
#d1##9
#x1####.##e4
#x0499abcdefi
#x.
#xe1.2-3.abds2
#xe/2.4/3
