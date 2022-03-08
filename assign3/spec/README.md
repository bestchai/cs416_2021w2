# CPSC 416 2021W2 A3 Specification

This folder provides an automated checker script you can use to mechanically check that your A2 solution follows the provided spec.
It will check that any trace you give it follows the rules, _but will not invent testing scenarios for you_.

## How to run it

Check out this folder (or, clone this repository and `cd` to this folder).
The checker script is `a3spec.sc`.
It is an [Ammonite](https://ammonite.io/Ammonite) script, which means it is a plain-text file containing Scala code.

To run it, you will need to get Ammonite working.
This directory contains a bootstrap script `amm` which should work on Linux and Mac.
This link lists other ways to get it working, e.g for Windows users: http://ammonite.io/#InstallationonLinux.
You can also run it on the student Linux servers.

Assuming a working Ammonite setup accessible via `./amm`, get the script's help message in your terminal with:
```
./amm a3spec.sc --help
```

## Example
```
./amm a3spec.sc -n 5 trace_output1.log trace_output2.log trace_output3.log
```

- `-n [N]` is the length of your server chain

If you hit an out-of-memory or stack-overflow exception, you can pass JVM arguments by:
```
JAVA_OPTS="-Xmx1024m -Xss100m" ./amm a3spec.sc -n 5 trace_output1.log trace_output2.log trace_output3.log
```
where `-Xmx` is for memory limit and `-Xss` is for stack size.

## I think the checker is wrong, or the instructions didn't work for me

Pull Requests with additional information, insight on how to install on an OS type we don't have, or other corrections, are welcome.
This includes if we made a mistake in the checker itself, which can happen, especially for complex specs where we might fail to consider a valid interpretation of the assignment spec that is not obvious to us.

If you just think the checker is wrong but are not sure of the correction, ask on Piazza first, and we can escalate to a change in the checker if it becomes necessary.
