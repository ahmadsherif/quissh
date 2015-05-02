.RECIPEPREFIX = >

app:
> gcc -o quissh_nif.so -fpic -I/usr/lib/erlang/usr/include -shared quissh_nif.c
> erlc *.erl

clean:
> rm -f *.beam *.so
