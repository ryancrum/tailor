# tailor

A unified diff format generator for clojure.

## Usage

Tailor is a simple helper library that helps with the generation and formatting of unified diff files.

```clojure
user> (use 'tailor.diff)
nil
user> (-> (create-changeset ["a" "b" "c" "d" "e"])
          (append-line "f" 6)
          (change-line "A" 1)
          (remove-line 3)
          (file-diff "tmp/moose.txt" 1)
          (print))
```

```diff
--- tmp/moose.txt
+++ tmp/moose.txt
@@ -1,4 +1,3 @@
-a
+A
 b
-c
 d
@@ -5,1 +4,2 @@
 e
+f
````


## License

Copyright (C) 2012 Ryan Crum

Distributed under the Eclipse Public License, the same as Clojure.
