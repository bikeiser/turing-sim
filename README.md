# TuringSim
Turing Machine simulator written in haskell.

Basic usage: either the Turing Machine description is read from `STDIN` or read from a file using `-f FILENAME`

## Turing Machine Description Syntax
```
M=({"0","1","2","3","4"},{'0','1'},{'0','1','B'},d,"0",'B',{"4"})
d=[
    ("0",'0')->("0",'0',R),
    ("0",'1')->("0",'1',R),
    ("0",'B')->("1",'B',L),
    ("1",'0')->("3",'1',L),
    ("1",'1')->("2",'0',L),
    ("2",'1')->("2",'0',L),
    ("2",'0')->("3",'1',R),
    ("2",'B')->("3",'1',R),
    ("3",'1')->("3",'1',L),
    ("3",'0')->("3",'0',L),
    ("3",'B')->("4",'B',R)
]
```

Currently a Turing Machine description is given by two ``variables'': `M`, and `d`. `M` must be given before `d`. Whitespaces are ignored between literals.
`M` is a 7-tuple with a set of states, input alphabet, tape alphabet, `d`, a starting state, the blank symbol, and a set of accepting states.
`d` is a partial function describing the transitions. It is described using a list of input output pairs separated by `->`. The input pair consists of a state and a tape symbol, the output tuple consists of a state, tape symbol, and a direction (L or R).


## Command usage
```
Turing Machine simulator

Usage: turing-sim [-f|--file FILE] [-x|--function] [-t|--time] [-c|--trace] 
                  [-o|--only-time] INPUT

  Run a Turing machine

Available options:
  -f,--file FILE           Turing machine description file
  -x,--function            Show tape instead of accept/reject
  -t,--time                Show how many steps
  -c,--trace               Print all configurations leading up to the final one
  -o,--only-time           Only print time
  INPUT                    Input string
  -h,--help                Show this help text
```


