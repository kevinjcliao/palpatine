# Palpatine: STV Vote Counter

![Chancellor Palpatine][palpatine]

> The Senate will decide your fate.  
> I am the senate.  
> -(Mace Windu and Chancellor Palpatine, 19BBY). 

During elections, ballots for the Australian senate are currently counted by proprietary, unverified software. This pressing issue is what is driving me to write Palpatine, a vote counter for the STV system written in Edwin Brady's Idris. The next steps will be to see how a dependently-typed language can give us better guarantees of program-correctness. 

Palpatine is part of my senior thesis at [Haverford College](https://www.haverford.edu), which I am undertaking under the advisement of [Professor Richard Eisenberg](https://cs.brynmawr.edu/~rae/). 

## Running Palpatine: 
You should have Idris installed. This is usually as easy as:  
`cabal install idris`

Palpatine is set up as an [Idrin project](https://github.com/zjhmale/idringen). You should follow the steps on the [Idrin Github](https://github.com/zjhmale/idringen) to install Idrin. After which, you should be able to do: 
```
idrin build
idrin run
```

[palpatine]: https://vignette.wikia.nocookie.net/starwars/images/9/9a/Palp_trustme.jpg/revision/latest/scale-to-width-down/250?cb=20070114040526