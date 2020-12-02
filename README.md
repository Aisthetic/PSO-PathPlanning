# PSO-Pathfinding
Implementation of the Particle Swarm Optimization algorithm  for finding a path in a dynamic environment. This project contains the algorithm implementation and its environment plus an user interface to visualize the solution(s).

## Installation

This project requires the ocaml environment to be run.

```bash
apt-get install ocaml opam ocamlbuild
opam init
eval $(opam env)
opam install ocamlfind
```

## Usage
```bash
ocamlbuild -use-ocamlfind -pkg graphics main.d.byte 
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/) Poufpouf property
