# CPSC 416 2021W2 A2 Specification

This folder provides an automated checker script you can use to mechanically check that your A2 solution follows the provided spec.
It will check that any trace you give it follows the rules, _but will not invent testing scenarios for you_.

## The differences from A1 spec
1. The command line flags are simplified (see the example blow).
2. The checker script now can take multiple traces at once. For each grading rule, the _lowest_ score among all traces will be considered as your final score.
3. There are some _conditional rules_ in the A2 spec. For example, the rules that check "Nim servers total failure handled properly" will only be triggered when [AllNimServersDown] action appears in your trace, otherwise, the script will consider your traces as passing these rules. Therefore, the final score you get from the script is only the _highest possible grade_ you may get.
4. The GameState field will now be deserialized into a list in the _error message_ for readability.

## How to run it

Check out this folder (or, clone this repository and `cd` to this folder).
The checker script is `a2spec.sc`.
It is an [Ammonite](https://ammonite.io/Ammonite) script, which means it is a plain-text file containing Scala code.

To run it, you will need to get Ammonite working.
This directory contains a bootstrap script `amm` which should work on Linux and Mac.
This link lists other ways to get it working, e.g for Windows users: http://ammonite.io/#InstallationonLinux.
You can also run it on the student Linux servers.

Assuming a working Ammonite setup accessible via `./amm`, get the script's help message in your terminal with:
```
./amm a2spec.sc --help
```

## Example
```
./amm a2spec.sc --seed 100 -n 3 trace_output1.log trace_output2.log trace_output3.log
```

- `--seed [seed]` is the seed you used to intiate the game.
- `-n [N]` is the length of your nim server list
- `trace_output1.log trace_output2.log ...` are the traces you want the script to check

## I think the checker is wrong, or the instructions didn't work for me

Pull Requests with additional information, insight on how to install on an OS type we don't have, or other corrections, are welcome.
This includes if we made a mistake in the checker itself, which can happen, especially for complex specs where we might fail to consider a valid interpretation of the assignment spec that is not obvious to us.

If you just think the checker is wrong but are not sure of the correction, ask on Piazza first, and we can escalate to a change in the checker if it becomes necessary.
