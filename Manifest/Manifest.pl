:- bundle(ciaowasm).
depends([
    core
]).
alias_paths([
    ciaowasm = 'src',
    ciaobld = 'src_builder' % (extends the builder with 'wasm' grade)
]).
service(toplevel_js, [http, redirect('examples/toplevel.html')]).
service(lpdoc_js, [http, redirect('examples/lpdoc_view.html')]).
service(ciaopp_js, [http, redirect('examples/ciaopp.html')]).
service(testsuite_js, [http, redirect('examples/testsuite.html')]).
service(chat80_js, [http, redirect('examples/chat80.html')]).


