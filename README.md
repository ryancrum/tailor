# tailor

A unified diff format generator for clojure.

## Usage

Tailor is a simple helper library that helps with the generation and formatting of unified diff files.

```clojure
user> (use 'tailor.diff)
nil
user> (def changes (atom (create-changeset ["a" "b" "c" "d" "e"])))
#'user/changes
user> (swap! changes append-line "f" 6)
{:lines ("a" "b" "c" "d" "e" "f"), :offset 0, :change-map {5 :add}}
user> (swap! changes change-line "A" 1)
{:lines ("a" "A" "b" "c" "d" "e" "f"), :offset 0, :change-map {1 :add, 6 :add, 0 :remove}}
user> (swap! changes remove-line 3)
{:lines ("a" "A" "b" "c" "d" "e" "f"), :offset 0, :change-map {3 :remove, 1 :add, 6 :add, 0 :remove}}
user> (print (file-diff "tmp/moose.txt" @changes 1))
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
@@ -5,1 +5,2 @@
 e
+f
nil
````


## License

Copyright (C) 2012 Ryan Crum

Distributed under the Eclipse Public License, the same as Clojure.
