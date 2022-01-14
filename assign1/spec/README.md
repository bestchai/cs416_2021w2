# CPSC 416 2021W2 A1 Specification

This folder provides an automated checker script you can use to mechanically check that your A1 solution follows the provided spec.
It will check that any trace you give it follows the rules, _but will not invent testing scenarios for you_.
It is up to you to create meaningful testing scenarios for your work: if your code only works under happy-path conditions (e.g no packet loss), it will not get a top score.

## How to run it

Check out this folder (or, clone this repository and `cd` to this folder).
The checker script is `a1spec.sc`.
It is an [Ammonite](https://ammonite.io/Ammonite) script, which means it is a plain-text file containing Scala code.

To run it, you will need to get Ammonite working.
This directory contains a bootstrap script `amm` which should work on Linux and Mac.
This link lists other ways to get it working, e.g for Windows users: http://ammonite.io/#InstallationonLinux.
You can also run it on the student Linux servers.

Assuming a working Ammonite setup accessible via `./amm`, get the script's help message in your terminal with:
```
./amm a1spec.sc --help
```

The script's first run may take a few minutes, as, depending on how you set Ammonite up, you will at least be downloading all of the tool's dependencies, or maybe even a copy of Ammonite itself.
This means that this first run requires an internet connection.

The help message will read:
```
a1spec
  --expectedSeed <str>  the seed passed to the client which produced the trace being checked, in
                        decimal with no leading zeroes
  --traceFile <path>    path to the trace file to analyse. this file will the one you told the
                        tracing server to generate, and should contain exactly one trace
```

Once you have a trace you want to check, run the tool as with this example:
```
./amm a1spec.sc --expectedSeed 100 --traceFile trace_output.log
```

The example trace output `trace_output.log` is provided, and comes from our private example solution, running against a pre-release version of the server, with the seed `100`.
The checker script should claim this trace passes all conditions.
To see failing outputs, you can "break" the example by editing it: try changing e.g `MoveCount` in one of the move records.

## What does the output mean?

If your trace passes all the conditions, you should see a list of properties with all green check marks.

If any properties are not satisfied by the trace, it will try to indicate which one, detailing a counter-example.
To understand a failure in depth, you will have to look up the line numbers it indicates in the checker script, like reading a prettified stacktrace in Go, or most other languages you might already know.
Ideally you should only need to understand general terms like `forall`, `exists` (first-order logic), `require` (an assertion), `label` (relevant values that will be in the checker's output), and the English output is intended to be reasonably informative.

If you want more in-depth insight into what the script is doing, most of it is written in a DSL that is open-sourced here: https://github.com/DistributedClocks/TraceChecker.
The source code has extensive documentation, and the answers to your deeper questions may be found by reading [Queries.scala](https://github.com/DistributedClocks/TraceChecker/blob/master/src/main/scala/Queries.scala) and [Query.scala](https://github.com/DistributedClocks/TraceChecker/blob/master/src/main/scala/Query.scala).

## I think the checker is wrong, or the instructions didn't work for me

Pull Requests with additional information, insight on how to install on an OS type we don't have, or other corrections, are welcome.
This includes if we made a mistake in the checker itself, which can happen, especially for complex specs where we might fail to consider a valid interpretation of the assignment spec that is not obvious to us.

If you just think the checker is wrong but are not sure of the correction, ask on Piazza first, and we can escalate to a change in the checker if it becomes necessary.
