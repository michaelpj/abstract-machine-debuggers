# Debuggers for abstract machines

An exploration of writing a debugger for CEK-style machines.

The machines are all implemented as state machines (see `StateMachine.hs`).

The machines progress in order:
- A basic CEK machine (`CEK.hs`)
- A CEK machine with direction control (`DCEK.hs`)
- A CEK machine with direction control and breakpoints (`DCEBK.hs`)

You can play around with a state machine by doing something like the 
following:
```
> cabal repl
> :m +REPL StateMachine BreakLambda DCEBK Text.Megaparsec Data.Maybe
> repl dcebk (inject (fromJust $ parseMaybe parseExp "(\\x.x)(\\y.y"))
# type "Step"/"Continue"/"Next" etc.
```

## References

The approach here is very much inspired by [Advice about Debugger Construction]
(https://pdfs.semanticscholar.org/7ae9/3a0f1ecac0094051345e518282a333a9df96.pdf), although 
I think I have managed to find a significantly simpler way of achieving the same thing.
