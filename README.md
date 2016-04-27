# Crush - Untyped Lambda Calculus Interpreter

Crush is an interpreter for the untyped lambda calculus.
The executable reads a lambda expression from the standard input and processes it in one of the following ways:

    Usage: crush [-m|--mode MODE] [-s|--strategy STRATEGY] [-l|--limit LIMIT]
      Normalize and trace a lambda expression, reading the expression from standard
      input, writing the computation trace to standard error, and writing the
      normalized term to standard output.
 
    Available options:
      -h,--help                    Show this help text
      -m,--mode MODE               One of the following execution modes:
                                   * TraceNormalize (Default)
                                   * Normalize
                                   * Trace
      -s,--strategy STRATEGY       One of the following evaluation strategies:
                                   * NormalOrder (Default)
                                   * CallByValue
                                   * CallByName
      -l,--limit LIMIT             Reduction step limit

There are three different modes:

* `TraceNormalize` (default):  
   1. The lambda expression is read form the standard input.
   2. The parsed term is reduced via one of the three reduction strategies;  
       the trace of the reduction process is printed to `stderr`. 
   3. If the computation terminates the normal form is written to `stdout`.
* `Trace`:  
    Like `TraceNormalize`, but the trace is directly written to `stdout`.
* `Normalize`:  
    No tracing with this mode;
    it's faster than the two above there is no overhead for logging.  
    The normal form (if one exists) is written to `stdout`.

There are three different evaluation strategies:
   
1. Normal Order Reduction (flag `NormalOrder` - this is the default)
2. Call By Value (flag `CallByValue`)
3. Call By Name (flag `CallByName`) 

There is also the possibility to limit the number of reduction steps in the `TraceNormalize` and `Trace` modes
by issuing the `--limit Limit` flag where limit is a number (e.g. `--limit 100` limits execution to 100 reduction steps). 

## Syntax

The supported syntax is the lambda calculus with support for (non-recursive) *let-expressions*.

    Λ := x                      -- Variable 
         | Λ Λ                  -- Application
         | λ x Λ                -- Abstraction
         | let x = Λ in Λ       -- let-Expression

Some examples for valid lambda terms:  

* `λx.x`, `λf.λx.x`
* `λf.(λx.f (x x)) (x.f (x x)))`
*  `let tru = λt.λf.t in (λx.x) tru`  
* *Let-expressions* can be nested:  
   `let tru = λf.λt.t in let fls = λt.λf.f let if = λb.λt.λf.b t f in if tru fls tru`.

# Usage Examples

A little program in the lambda calculus. Something simple for starters - we compute 2 * 2:

    let fix    = λg.(λx.g (x x)) (λx.g (x x)) in
    let zro    = λf.λx.x in
    let scc    = λn.λf.λx.f (n f x) in
    let prd    = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u) in
    let one    = λf.λx.f x in 
    let n2c    = scc one in
    let tru    = λx.λy.x in
    let fls    = λx.λy.y in
    let if     = λp.λa.λb.p a b in
    let iszro  = λn.n (λx.fls) tru in
    let add    = fix (λradd.λm.λn.if (iszro n) m (scc (radd m (prd n)))) in
    let mlt    = fix (λrmlt.λm.λn.if (iszro m) zro (add n (rmlt (prd m) n))) in
    mlt n2c n2c 

Safe this to a file, say `2x2.lam`, and open a shell (in the same folder).
Then pipe the file to the executable:

    cat 2x2.lam | crush

Now you should see a surprisingly huge trace 
(were each line is one intermediate result of the reduction process)
and as a final result:

    λf1.λx2.f1 (f1 (f1 (f1 x2)))

This little program as well as further samples and templates can be found in the `examples` folder.
