# Información del repo

Nombre del equipo: Equipo [0]

Nombre de los integrantes
- Alistac Flores Nancy Estefany
- Rosales Guillen Emilio Alexeiv "Amy"

## GHCup
**Para evitar problemas en distros rolling release como Arch linux u openSUSE Tumbleweed, se usará [GHCup](https://www.haskell.org/ghcup/), también se manejará todo lo de cabal (bibliotecas de hackage), stack (gestión y desarrollo de proyectos).**

- La versión de Haskell a usar es ghc-9.10.3
- La versión de cabal es 3.16.1.0
- La versión de stack es 3.11.1
- La versión de ghci (el intérprete de Haskell) es 9.10.3

Todo compilado desde el código fuente

### Bibliotecas y módulos

Se usarán las bibliotecas/paquetes y módulos pedidos durante el laboratorio, esto son instalados mediante:
```bash
$ cabal install alex happy
$ cabal install --lib QuickCheck
```

**(Esta lista se irá actualizando constantemente)**

Las versiones instaladas a la fecha 2026-08-30 son:
- alex 3.5.4.2
- happy 2.2
- QuickCheck 2.18.0.0

QuickCheck fue instalado de manera global para todo archivo en Haskell, si se instala para proyectos con cabal, en el .cabal escribelo como dependencia del proyecto en desarrollo
```cabal
executable <NOMBRE_PROGRAMA>
    main-is             Main.hs
    build-depends       base >= 4.14,
                        QuickCheck
```

Así también tendrias una version estable de Prelude para ese proyecto :D

#### Módulos ocultos

Algunos modulos y paquetes que por lo general vienen en el paquete ghc de cualquier distro como Data.Array suelen instalarse de forma global, pero GHCup los aisla para que nada explote, si quieres pasar paquetes a globales simplemente corre la línea
```bash
$ cabal install --lib array
```

De esta forma se hará visible para todo el PATH (o scope o como se llame), pero si no quieres, puedes indicarle a ghc que use la libreria de la forma
```bash
$ ghc -package array <ARCHIVO.hs>
```

*Esta libreria se usa a la hora de que alex genere el analizador léxico con "saltos" eficientes, similar a la biblioteca Vector de C++*

Puede ocurrir que después de instalar los modulos o paquetes estos después de un reainicio ya no estén habilitados, simplemente se debe de agregar en .bashrc o zshrc la siguiente línea
```
export PATH="$HOME/.local/bin/:$HOME/.ghcup/bin/:$PATH"
```
