Protein folder
==============

Installation
------------

To compile and install, first make sure you have haskell platform installed.
Then run:
    
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build
    $ cabal install

There is now a runnable file in the location "<path to project>/dist/pfolder/pfolder".
On windows, it is called "pfolder.exe".

To see how it is used run it with

    $ <path to project>/dist/pfolder/pfolder --help

If you have the cabal bin directory in your path, you can use
    
    $ pfolder --help


Usage
-----

To make a ordinary run with the resulting chain printed to stdout:

    $ pfolder [-l latticetype] <residues> <iterations>

The different lattice types is 2d, 2d-r, 3d, fcc (default: fcc) where 
2d-r represent the human readable result of 2d.

To make a large run of the algorithm where the chain is folded several times
you use the following command:
    
    $ pfolder large <residues> <iterations> <number of runs>
