Welcome to our project, the Decentralized Prescription Validator. Here are installation instructions:

- Unzip the compressed version of the project
- Our project uses two external packages (other than Csv and OUnit). These are [`Yojson`](https://github.com/ocaml-community/yojson) (used in the Blockchain compilation unit) and [`digestif`](https://github.com/mirage/digestif) (used in the Block compilation unit). To our knowledge, `yojson` is included as part of the OCaml/OPAM/Dune distribution we installed in CS 3110. You can ensure it is installed by running `opam install yojson`. To install `digestif`, run `opam install digestif`
- Next, enter the unzipped project and run `dune build`. 
- An existing blockchain with some data and accounts is already provided for you to play with. Examine accounts.csv to see usernames and passwords for test accounts. You can simply run `dune exec bin/main.exe` and use one of these logins to try the system out.

Happy Validating!
