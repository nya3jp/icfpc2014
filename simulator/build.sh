ocamlbuild main.native
mv main.native simulator
ocamlbuild lambdaman_executor.native
mv lambdaman_executor.native lambdaman_executor
