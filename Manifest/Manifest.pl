:- bundle(ciaowasm).
depends([
    core
]).
alias_paths([
    ciaowasm = 'src',
    library = 'lib',
    ciaobld = 'src_builder' % (extends the builder with 'wasm' grade)
]).
lib('lib').


