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
## Arborescence
```
PSO-PathPlanning
│
└───PSO_general 
│   └─── initialisation.ml
│   |   │   génération basique 
│   │
│   └─── particleswarm.ml
│   |   │   l'algo du PSO (appliqué au problème du wagon pour exemple)
│   
└───PSO_avion_statique (obstacles statiques dans le temps
│   └─── initialisation_avion.ml
│   |   │  génération de particules appliquées a notre problème
│   │
│   └─── algo.ml
│   |   │  résolution à l'aide du PSO (encore en debugage)
|
|___PSO_avion_deterministe (position des obstacles connue dans le temps)
|   |
|   |___ Working on it
|
|
|___Modules_communs 
    └─── geometrie.ml
    |   │   génération basique 
    │
    └─── gui.ml
    |   │   l'algo du PSO (appliqué au problème du wagon pour exemple)
    |
    └─── Utility_print.ml
        │   les fonctions de debugage (notamment print)
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/) Poufpouf property
