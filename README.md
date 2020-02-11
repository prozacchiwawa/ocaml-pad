## Bucklescript environment for phones as a single monolithic html.

Packs in everything needed for a net-free dev experience in ocaml on a phone.
Includes a programmer style keyboard widget and a more text-oriented editor
than the standard ones on android.  Capturing keyboard events on phones is
inconsistent... I gave up and provided a keyboard in the app.  This isn't great;
it's mostly an experiment and lots of things can be improved.

# Build
```
make BS_PLAYGROUND=<path to bucklescript/playground>
```
