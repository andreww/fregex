program test

use fexp

print*, "Regexp, String, Result, expected result"
print*, "Foo, FooBar, ", match("Foo", "FooBar"), "true"
print*, "Poo, FooBar, ", match("Poo", "FooBar"), "false"
print*, "Bar, FooBar, ", match("Bar", "FooBar"), "true"
print*, "Par, FooBar, ", match("Par", "FooBar"), "false"
print*, "Foo, Foo, ", match("Foo", "Foo"), "true"
print*, "Fo, Foo, ", match("Fo", "Foo"), "true"
print*, "Foo, Fo, ", match("Foo", "Fo"), "false"
print*, "ooB, FooBar, ", match("ooB", "FooBar"), "true"
print*, "ooP, FooBar, ", match("ooP", "FooBar"), "false"
print*, "., FooBar, ", match(".", "FooBar"), "true"
print*, "P., FooBar, ", match("P.", "FooBar"), "false"
print*, "^Foo, FooBar, ", match("^Foo", "FooBar"), "true"
print*, "^Bar, FooBar, ", match("^Bar", "FooBar"), "false"
print*, "Foo$, FooBar, ", match("Foo$", "FooBar"), "false"
print*, "Bar$, FooBar, ", match("Bar$", "FooBar"), "true"
print*, ".*o, FooBar, ", match(".*o", "FooBar"), "true"
print*, "o*o, FooBar, ", match("o*o", "FooBar"), "true"
print*, "P*o, FooBar, ", match("P*o", "FooBar"), "true"
print*, "Fo*o, FooBar, ", match("Fo*o", "FooBar"), "true"
print*, "Po*o, FooBar, ", match("Po*o", "FooBar"), "false"
print*, ".+o, FooBar, ", match(".+o", "FooBar"), "true"
print*, "o+o, FooBar, ", match("o+o", "FooBar"), "true"
print*, "P+o, FooBar, ", match("P+o", "FooBar"), "false"
print*, "Fo+o, FooBar, ", match("Fo+o", "FooBar"), "true"
print*, "Po+o, FooBar, ", match("Po+o", "FooBar"), "false"
print*, "F[po]o, FooBar, ", match("F[po]o", "FooBar"), "true"
print*, "F[op]o, FooBar, ", match("F[op]o", "FooBar"), "true"
print*, "F[qp]o, FooBar, ", match("F[qp]o", "FooBar"), "false"

print*, "F[qpo, FooBar, Stops!"
print*, match("F[qpo", "FooBar")

end program test
